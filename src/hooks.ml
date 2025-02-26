type 'a hook = ..

module Heterogenous_list = Heterogenous_list.Make (struct
    type 'a t = 'a hook
  end)

type 'a state = 'a Heterogenous_list.t

type ('remaining, 'processed) t =
  { remaining : 'remaining Heterogenous_list.t option
  ; processed : ('remaining * 'processed) Heterogenous_list.constructor
  ; onStateDidChange : unit -> unit }

let ofState remaining ~onStateDidChange =
  {remaining; processed = Heterogenous_list.init; onStateDidChange}

type nil = Heterogenous_list.nil
type empty = nil Heterogenous_list.t
type 'a all = (nil, 'a) t

let toState {processed} = Heterogenous_list.seal processed

let empty =
  (fun () ->
     { remaining = None
     ; processed = Heterogenous_list.init
     ; onStateDidChange = (fun () -> ()) }
   : unit -> _ t)

let printState = function Some _ -> "<Some>" | None -> "<Empty>"

let processNext
      ~default
      ?merge
      ~toWitness
      {remaining; processed; onStateDidChange}
  =
  match remaining with
  | Some l ->
      let {Heterogenous_list.value; toWitness}, rest =
        Heterogenous_list.dropFirst l
      in
      ( value
      , { remaining = Some rest
        ; processed =
            Heterogenous_list.append
              { value = (match merge with Some f -> f value | None -> value)
              ; toWitness }
              processed
        ; onStateDidChange } )
  | None ->
      ( default
      , { remaining = None
        ; processed =
            Heterogenous_list.append {value = default; toWitness} processed
        ; onStateDidChange } )

module State = struct
  type 'a t = {currentValue : 'a; mutable nextValue : 'a}
  type 'a hook += State : 'a t -> 'a t hook

  let make =
    (fun initialValue -> {currentValue = initialValue; nextValue = initialValue}
     : 'a -> 'a t)

  let wrapAsHook s = State s
  let setState nextValue stateContainer = stateContainer.nextValue <- nextValue

  let flush {currentValue; nextValue} =
    if currentValue == nextValue
    then None
    else Some {currentValue = nextValue; nextValue}

  let hook initialState hooks =
    let stateContainer, nextHooks =
      processNext ~default:(make initialState) ~toWitness:wrapAsHook hooks
    in
    let onStateDidChange = hooks.onStateDidChange in
    let setter updater =
      setState (updater stateContainer.nextValue) stateContainer;
      onStateDidChange ()
    in
    (stateContainer.currentValue, setter), nextHooks
end

module Reducer = struct
  type 'a t = {currentValue : 'a; updates : ('a -> 'a) list ref}
  type 'a hook += Reducer : 'a t -> 'a t hook

  let make =
    (fun initialValue -> {currentValue = initialValue; updates = ref []}
     : 'a -> 'a t)

  let flush =
    (fun reducerState ->
       let {currentValue; updates} = reducerState in
       let nextValue =
         List.fold_right
           (fun update latestValue -> update latestValue)
           !updates currentValue
       in
       updates := [];
       if currentValue == nextValue
       then None
       else Some {currentValue = nextValue; updates}
     : 'a t -> 'a t option)

  let wrapAsHook s = Reducer s
  let enqueueUpdate nextUpdate {updates} = updates := nextUpdate :: !updates

  let hook ~initialState reducer hooks =
    let stateContainer, hooks =
      processNext ~default:(make initialState) ~toWitness:wrapAsHook hooks
    in
    let onStateDidChange = hooks.onStateDidChange in
    let dispatch action =
      enqueueUpdate (fun prevValue -> reducer action prevValue) stateContainer;
      onStateDidChange ()
    in
    (stateContainer.currentValue, dispatch), hooks
end

module Ref = struct
  type 'a t = 'a ref
  type 'a hook += Ref : 'a t -> 'a t hook

  let wrapAsHook s = Ref s

  let hook initialState hooks =
    let internalRef, hooks =
      processNext ~default:(ref initialState) ~toWitness:wrapAsHook hooks
    in
    internalRef, hooks
end

module Effect = struct
  type lifecycle = Mount | Unmount | Update
  type always
  type onMount

  type 'a condition =
    | Always : always condition
    | OnMount : onMount condition
    | If : ('a -> 'a -> bool) * 'a -> 'a condition
    | OnMountAndIf : ('a -> 'a -> bool) * 'a -> 'a condition

  type handler = unit -> (unit -> unit) option

  type 'a t =
    { condition : 'a condition
    ; handler : unit -> (unit -> unit) option
    ; mutable cleanupHandler : (unit -> unit) option
    ; mutable previousCondition : 'a condition }

  type 'a hook += Effect : 'a t -> 'a t hook

  let wrapAsHook s = Effect s
  let executeOptionalHandler = function Some f -> f () | None -> ()

  let get : type conditionValue.
    lifecycle:lifecycle -> conditionValue t -> (unit -> unit) option
    =
   fun ~lifecycle state ->
    let {condition; previousCondition; handler; cleanupHandler} = state in
    let updateIf comparator previousConditionValue =
      let currentConditionValue =
        match condition with
        | If (_, currentConditionValue) | OnMountAndIf (_, currentConditionValue)
          ->
            currentConditionValue
        | Always -> previousConditionValue
        | OnMount -> previousConditionValue
      in
      if comparator previousConditionValue currentConditionValue
      then (
        state.previousCondition <- condition;
        Some
          (fun () ->
            executeOptionalHandler cleanupHandler;
            state.cleanupHandler <- handler ()))
      else (
        state.previousCondition <- condition;
        None)
    in
    match previousCondition with
    | Always ->
        Some
          (fun () ->
            ignore (executeOptionalHandler cleanupHandler);
            state.cleanupHandler <- handler ())
    | OnMount -> (
      match lifecycle with
      | Mount -> Some (fun () -> state.cleanupHandler <- handler ())
      | Update -> None
      | Unmount -> cleanupHandler)
    | If (comparator, previousConditionValue) -> (
      match lifecycle with
      | Mount -> None
      | Update -> updateIf comparator previousConditionValue
      | Unmount -> cleanupHandler)
    | OnMountAndIf (comparator, previousConditionValue) -> (
      match lifecycle with
      | Mount -> Some (fun () -> state.cleanupHandler <- handler ())
      | Update -> updateIf comparator previousConditionValue
      | Unmount -> cleanupHandler)

  let hook condition handler hooks =
    let _, hooks =
      processNext
        ~default:
          { condition
          ; handler
          ; cleanupHandler = None
          ; previousCondition = condition }
        ~merge:(fun prevEffect -> {prevEffect with condition; handler})
        ~toWitness:wrapAsHook hooks
    in
    (), hooks
end

let state = State.hook
let reducer = Reducer.hook
let ref = Ref.hook
let effect = Effect.hook

let pendingEffects ~lifecycle hooks =
  match hooks with
  | Some hooks ->
      Heterogenous_list.fold
        (fun acc opaqueValue ->
           match opaqueValue with
           | Heterogenous_list.Any (Effect.Effect state) -> (
             match Effect.get ~lifecycle state with
             | Some effect -> Effect_sequence.chain acc effect
             | None -> acc)
           | _ -> acc)
        Effect_sequence.noop hooks
  | None -> Effect_sequence.noop

let flushPendingStateUpdates hooks =
  let nextHooks =
    Heterogenous_list.map
      { f =
          (fun (type a) ->
            fun (hook : a hook) ->
             match hook with
             | Reducer.Reducer s -> (Reducer.flush s : a option)
             | State.State s -> (State.flush s : a option)
             | _ -> None) }
      hooks
  in
  match Heterogenous_list.compareElementsIdentity hooks nextHooks with
  | true -> hooks
  | false -> nextHooks

open Core_bench

let numberOfChildren = 4

let totalNodesAtDepth depth =
  let open Core.Int in
  numberOfChildren ** depth

module Update = struct
  type t = {depth : int; index : int; switchComponent : bool}

  let random ~depth ~switchComponent =
    {depth; index = Core.Random.int (totalNodesAtDepth depth); switchComponent}

  let remoteAction = (Brisk.Remote_action.create () : t Brisk.Remote_action.t)
end

module Unit = struct
  open Brisk

  let%nativeComponent make ~depth:_ () hooks =
    ( { make = (fun () -> ())
      ; configureInstance = (fun ~isFirstRender:_ -> fun () -> ())
      ; children = empty
      ; insertNode
      ; deleteNode
      ; moveNode }
    , hooks )
end

module A = struct
  open Brisk
  open Update

  module ComponentType = struct
    type t = A | A' | B | B'

    let toggle = function A | A' -> B | B | B' -> A
    let dummyUpdate = function A -> A' | A' -> A | B -> B' | B' -> B
  end

  let list4MapIndexExn ~f ~index l =
    match l with
    | [a; b; c; d] -> (
      match index with
      | 0 -> [f a; b; c; d]
      | 1 -> [a; f b; c; d]
      | 2 -> [a; b; f c; d]
      | 3 -> [a; b; c; f d]
      | _ -> raise (Invalid_argument "Index too big"))
    | _ -> raise (Invalid_argument "List should have 4 elements")

  let rec componentDefinition ~depth ~index hooks =
    let childrenIndexOffset = index * 4 in
    let childrenDepth = depth + 1 in
    let (children, dispatch), hooks =
      Hooks.reducer
        ~initialState:
          (let open ComponentType in
           [A; A; A; A])
        (fun {depth; index; switchComponent} ->
           fun state ->
            let mappedIndex = index - childrenIndexOffset in
            if childrenDepth = depth && mappedIndex >= 0 && mappedIndex <= 3
            then
              if switchComponent
              then
                list4MapIndexExn ~f:ComponentType.toggle ~index:mappedIndex
                  state
              else
                list4MapIndexExn ~f:ComponentType.dummyUpdate ~index:mappedIndex
                  state
            else state)
        hooks
    in
    let (), hooks =
      Hooks.effect OnMount
        (fun () ->
           Some (Brisk.Remote_action.subscribe ~handler:dispatch remoteAction))
        hooks
    in
    if depth < 5
    then
      ( hooks
      , Brisk.listToElement
          (List.mapi
             (fun i ->
                fun c ->
                 make c ~depth:(depth + 1) ~index:(i + childrenIndexOffset) [])
             children) )
    else hooks, (Unit.createElement ~depth:(depth + 1) ~children:[] () [@JSX])

  and componentA = component "A"
  and componentB = component "B"
  and makeA ~depth ~index = componentA (componentDefinition ~depth ~index)
  and makeB ~depth ~index = componentB (componentDefinition ~depth ~index)

  and make componentType ~depth ~index _children =
    match componentType with
    | A | A' -> makeA ~depth ~index
    | B | B' -> makeB ~depth ~index

  let render () =
    Brisk.RenderedElement.render
      {node = (); insertNode; deleteNode; moveNode}
      (makeA ~depth:0 ~index:0)
end

let benchUpdateAtDepth depth ~switchComponent =
  Bench.Test.create_with_initialization
    ~name:("Depth: " ^ string_of_int depth)
    (fun `init ->
       let renderedElement =
         A.render () |> Brisk.RenderedElement.executePendingEffects
       in
       Brisk.Remote_action.send
         ~action:(Update.random ~depth ~switchComponent)
         Update.remoteAction;
       fun () -> Brisk.RenderedElement.flushPendingUpdates renderedElement)

let main () =
  Random.self_init ();
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"First render" A.render
       ; Bench.Test.create_with_initialization ~name:"Execute mount effects"
           (fun `init ->
              let renderedElement = A.render () in
              fun () ->
                Brisk.RenderedElement.executePendingEffects renderedElement)
       ; Bench.Test.create_with_initialization ~name:"Execute host view updates"
           (fun `init ->
              let renderedElement =
                A.render () |> Brisk.RenderedElement.executePendingEffects
              in
              fun () ->
                Brisk.RenderedElement.executeHostViewUpdates renderedElement)
       ; Bench.Test.create_with_initialization ~name:"noop flushPendingUpdates"
           (fun `init ->
              let renderedElement = A.render () in
              fun () ->
                Brisk.RenderedElement.flushPendingUpdates renderedElement)
       ; Bench.Test.create_group ~name:"State update without subtree change"
           (List.map
              (benchUpdateAtDepth ~switchComponent:false)
              [0; 1; 2; 3; 4; 5])
       ; Bench.Test.create_group ~name:"State update with subtree change"
           (List.map
              (benchUpdateAtDepth ~switchComponent:true)
              [0; 1; 2; 3; 4; 5]) ])

let () = main ()

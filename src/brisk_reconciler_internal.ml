module GlobalState = struct
  let componentKeyCounter = ref 0
  let reset () = componentKeyCounter := 0
end

type handler = unit -> unit
type unregisterF = handler

let staleHandlers = ref []

let addStaleTreeHandler (handler : handler) =
  staleHandlers := handler :: !staleHandlers;
  fun () -> staleHandlers := List.filter (fun f -> f == handler) !staleHandlers

let callStaleHandlers () = List.iter (fun f -> f ()) !staleHandlers

module Key = struct
  type t = int

  let equal = ( = )
  let none = -1
  let dynamicKeyMagicNumber = 0

  let create () =
    incr GlobalState.componentKeyCounter;
    !GlobalState.componentKeyCounter
end

type 'a hostNode = Node of 'a | UpdatedNode of 'a * 'a
type 'a lazyHostNode = 'a hostNode Lazy.t
type 'a lazyHostNodeList = 'a lazyHostNode list
type 'a componentId = ..

type 'node element =
  | Flat of 'node opaqueComponent
  | Nested of 'node element list

and 'a component =
  { debugName : string
  ; key : int
  ; id : 'a instance componentId
  ; childrenType : 'viewSpec childrenType
  ; eq :
      'other.
      'other
      -> 'other componentId
      -> 'a instance componentId
      -> 'a instance option
  ; render : ('hooks, 'hooks) Hooks.t -> 'children * (Hooks.nil, 'hooks) Hooks.t
  }
  constraint 'viewSpec = 'node * 'children * 'childNode * 'wrappedNode
  constraint 'a = 'hooks * 'viewSpec

and 'a instance =
  { hooks : 'hooks Hooks.state
  ; opaqueComponent : 'node opaqueComponent
  ; component : 'a component
  ; children_ : 'children
  ; childInstances : 'childNode instanceForest
  ; wrappedHostNode : 'wrappedNode }
  constraint 'viewSpec = 'node * 'children * 'childNode * 'wrappedNode
  constraint 'a = 'hooks * 'viewSpec

and 'node opaqueComponent =
  | OpaqueComponent :
      (_ * ('node * _ * _ * _)) component
      -> 'node opaqueComponent

and 'node instanceForest =
  | IFlat of 'node opaqueInstance
  | INested of 'node instanceForest list * int

and 'node opaqueInstance =
  | Instance : (_ * ('node * _ * _ * _)) instance -> 'node opaqueInstance

and ('node, 'childNode) hostNodeElement =
  { make : unit -> 'node
  ; configureInstance : isFirstRender:bool -> 'node -> 'node
  ; children : 'childNode element
  ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
  ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
  ; moveNode : parent:'node -> child:'childNode -> from:int -> to_:int -> 'node
  }

and 'viewSpec childrenType =
  | Host :
      ('node
      * ('node, 'childNode) hostNodeElement
      * 'childNode
      * 'node lazyHostNode)
        childrenType
  | React :
      ('node * 'node element * 'node * 'node lazyHostNodeList) childrenType

type ('node, 'childNode) renderedElement =
  { nearestHostNode : 'node lazyHostNode
  ; nodeElement : ('node, 'childNode) hostNodeElement
  ; instanceForest : 'childNode instanceForest
  ; enqueuedEffects : Effect_sequence.t }

type ('node, 'childNode) opaqueInstanceUpdate =
  { nearestHostNode : 'node lazyHostNode
  ; nodeElement : ('node, 'childNode) hostNodeElement
  ; opaqueInstance : 'childNode opaqueInstance
  ; enqueuedEffects : Effect_sequence.t }

module InstanceForest = struct
  let getSubtreeSize : type node. node instanceForest -> int = function
    | INested (_, x) -> x
    | IFlat (Instance {wrappedHostNode; component = {childrenType}}) -> (
      match childrenType with React -> List.length wrappedHostNode | Host -> 1)

  let rec flatten = function
    | IFlat l -> [l]
    | INested (l, _) -> List_tailrec.concat (List_tailrec.map flatten l)

  let outputTreeNodes : type node. node instanceForest -> node lazyHostNodeList =
   fun subtree ->
    flatten subtree
    |> List.fold_left
         (fun (acc : node lazyHostNodeList)
           (Instance
              ({component = {childrenType}; wrappedHostNode} :
                (_ * (node * _ * _ * _)) instance) :
             node opaqueInstance) ->
            match childrenType with
            | React -> List.append wrappedHostNode acc
            | Host -> wrappedHostNode :: acc)
         []
    |> List.rev

  let pendingEffects ~lifecycle acc instanceForest =
    let f acc (Instance {hooks}) =
      Effect_sequence.chain (Hooks.pendingEffects ~lifecycle (Some hooks)) acc
    in
    let rec fold : type any.
      Effect_sequence.t -> any instanceForest -> Effect_sequence.t
      =
     fun acc instanceForest ->
      match instanceForest with
      | IFlat (Instance {childInstances} as opaqueInstance) ->
          f (fold acc childInstances) opaqueInstance
      | INested (l, _) ->
          List.fold_left
            (fun acc instanceForest -> fold acc instanceForest)
            acc l
    in
    fold acc instanceForest
end

module Element = struct
  let rec map f syntheticElement =
    match syntheticElement with
    | Flat l ->
        let opaqueInstance, enqueuedMountEffects = f l in
        IFlat opaqueInstance, enqueuedMountEffects
    | Nested l ->
        let instanceSubForestAndEffects = List_tailrec.map (map f) l in
        let subForest = List_tailrec.map fst instanceSubForestAndEffects in
        let effects = List_tailrec.map snd instanceSubForestAndEffects in
        ( INested
            ( subForest
            , subForest
              |> List.map InstanceForest.getSubtreeSize
              |> List.fold_left ( + ) 0 )
        , List.fold_left Effect_sequence.chain Effect_sequence.noop effects )

  let rec fold f renderedElement nearestHostNode nodeElement =
    match renderedElement with
    | Flat e ->
        let {nearestHostNode; nodeElement; opaqueInstance; enqueuedEffects} =
          f ~nearestHostNode ~nodeElement e
        in
        { nearestHostNode
        ; instanceForest = IFlat opaqueInstance
        ; nodeElement
        ; enqueuedEffects }
    | Nested l ->
        let nextL, nearestHostNode, enqueuedEffects, nodeElement =
          List.fold_left
            (fun (acc, nearestHostNode, enqueuedEffects, nodeElement) element ->
               let { nearestHostNode
                   ; instanceForest
                   ; enqueuedEffects = nextEffects
                   ; nodeElement }
                 =
                 fold f element nearestHostNode nodeElement
               in
               ( instanceForest :: acc
               , nearestHostNode
               , Effect_sequence.chain enqueuedEffects nextEffects
               , nodeElement ))
            ([], nearestHostNode, Effect_sequence.noop, nodeElement)
            (List.rev l)
        in
        { nearestHostNode
        ; nodeElement
        ; instanceForest =
            INested
              ( nextL
              , List.map InstanceForest.getSubtreeSize nextL
                |> List.fold_left ( + ) 0 )
        ; enqueuedEffects }
end

module HostNode = struct
  let make
        (type node)
        (type children)
        (type wrappedNode)
        (type childNode)
        (childrenType :
          (node * children * childNode * wrappedNode) childrenType)
        (children : children)
        (childInstances : childNode instanceForest) : wrappedNode
    =
    match childrenType with
    | Host ->
        lazy
          (let instance =
             children.make () |> children.configureInstance ~isFirstRender:true
           in
           Node
             (List.fold_left
                (fun (position, parent) child ->
                   ( position + 1
                   , let (Node child | UpdatedNode (_, child)) =
                       Lazy.force child
                     in
                     children.insertNode ~parent ~child ~position ))
                (0, instance)
                (InstanceForest.outputTreeNodes childInstances)
             |> snd))
    | React -> InstanceForest.outputTreeNodes childInstances
end

module SubtreeChange = struct
  let insertNodes
        ~nodeElement
        ~parent:parentWrapper
        ~children
        ~position:(initialPosition : int)
    =
    let (Node oldParent | UpdatedNode (_, oldParent)) = parentWrapper in
    let newParent =
      List.fold_left
        (fun (position, parent) child ->
           ( position + 1
           , let (Node child | UpdatedNode (_, child)) = Lazy.force child in
             nodeElement.insertNode ~parent ~child ~position ))
        (initialPosition, oldParent)
        children
      |> snd
    in
    match newParent == oldParent with
    | true -> parentWrapper
    | false -> UpdatedNode (oldParent, newParent)

  let deleteNodes
        ~nodeElement
        ~parent:parentWrapper
        ~children
        ~position:(initialPosition : int)
    =
    let (Node oldParent | UpdatedNode (_, oldParent)) = parentWrapper in
    let newParent =
      List.fold_left
        (fun (position, parent) child ->
           ( position + 1
           , let (Node child | UpdatedNode (_, child)) = Lazy.force child in
             nodeElement.deleteNode ~parent ~child ~position ))
        (initialPosition, oldParent)
        children
      |> snd
    in
    match newParent == oldParent with
    | true -> parentWrapper
    | false -> UpdatedNode (oldParent, newParent)

  let insertElement ~nodeElement ~parent ~children ~position =
    lazy
      (insertNodes ~nodeElement ~parent:(Lazy.force parent) ~children ~position)

  let replaceSubtree
        ~nodeElement
        ~parent
        ~prevChildren
        ~nextChildren
        ~(absoluteSubtreeIndex : int)
    =
    lazy
      (insertNodes ~nodeElement
         ~parent:
           (deleteNodes ~nodeElement ~parent:(Lazy.force parent)
              ~children:prevChildren ~position:absoluteSubtreeIndex)
         ~children:nextChildren ~position:absoluteSubtreeIndex)

  let reorderNode
        ~nodeElement
        ~child
        ~parent
        ~(indexShift : int)
        ~(from : int)
        ~(to_ : int)
    =
    let isVal = Lazy.is_val child in
    match Lazy.force child with
    | Node child -> (
      match from == to_ - indexShift with
      | true -> parent
      | false -> nodeElement.moveNode ~parent ~child ~from ~to_)
    | UpdatedNode (prevChild, child) when not isVal ->
        nodeElement.insertNode
          ~parent:
            (nodeElement.deleteNode ~parent ~child:prevChild ~position:from)
          ~child ~position:to_
    | UpdatedNode (_prevChild, child) -> (
      match from == to_ - indexShift with
      | true -> parent
      | false -> nodeElement.moveNode ~parent ~child ~from ~to_)

  let reorder
        (type node)
        (type childNode)
        ~nodeElement
        ~(parent : node lazyHostNode)
        ~instance:
          (Instance {wrappedHostNode; component = {childrenType}} :
            childNode opaqueInstance)
        ~indexShift
        ~from
        ~to_
    =
    match childrenType with
    | Host ->
        lazy
          (let parentWrapper = Lazy.force parent in
           let (Node oldParent | UpdatedNode (_, oldParent)) = parentWrapper in
           let newParent =
             reorderNode ~nodeElement ~parent:oldParent ~child:wrappedHostNode
               ~indexShift ~from ~to_
           in
           match newParent == oldParent with
           | true -> parentWrapper
           | false -> UpdatedNode (oldParent, newParent))
    | React ->
        lazy
          (let parentWrapper = Lazy.force parent in
           let (Node oldParent | UpdatedNode (_, oldParent)) = parentWrapper in
           let newParent =
             List.fold_left
               (fun (index, parent) child ->
                  ( index + 1
                  , reorderNode ~nodeElement ~parent ~child ~indexShift
                      ~from:(from + index) ~to_:(to_ + index) ))
               (0, oldParent) wrappedHostNode
             |> snd
           in
           match newParent == oldParent with
           | true -> parentWrapper
           | false -> UpdatedNode (oldParent, newParent))

  let updateNodes ~nodeElement ~parent ~instanceForest ~position:initialPosition
    =
    lazy
      (let parentWrapper = Lazy.force parent in
       let (Node oldParent | UpdatedNode (_, oldParent)) = parentWrapper in
       let newParent =
         List.fold_left
           (fun (position, instance) x ->
              ( position + 1
              , match Lazy.force x with
                | Node _child -> instance
                | UpdatedNode (oldNode, newNode) ->
                    nodeElement.insertNode
                      ~parent:
                        (nodeElement.deleteNode ~parent:instance ~child:oldNode
                           ~position)
                      ~child:newNode ~position ))
           (initialPosition, oldParent)
           (InstanceForest.outputTreeNodes instanceForest)
         |> snd
       in
       match newParent == oldParent with
       | true -> parentWrapper
       | false -> UpdatedNode (oldParent, newParent))
end

module OpaqueInstanceHash = struct
  type 'node t = (int, 'node opaqueInstance * int) Hashtbl.t lazy_t

  let addOpaqueInstance idTable index opaqueInstance =
    let (Instance {component = {key}}) = opaqueInstance in
    match key = Key.none with
    | true -> ()
    | false -> Hashtbl.add idTable key (opaqueInstance, index)

  let addRenderedElement idTable renderedElement index =
    let rec aux index = function
      | IFlat l -> addOpaqueInstance idTable index l
      | INested (l, _) -> List.iteri (fun i x -> aux i x) l
    in
    aux index renderedElement

  let createKeyTable renderedElement =
    lazy
      (let keyTable = Hashtbl.create 1 in
       addRenderedElement keyTable renderedElement 0;
       keyTable)

  let lookupKey table key =
    let keyTable = Lazy.force table in
    try Some (Hashtbl.find keyTable key) with Not_found -> None
end

module Instance = struct
  let rec ofComponent : type hooks node children childNode wrappedHostNode.
    node opaqueComponent
    -> (hooks * (node * children * childNode * wrappedHostNode)) component
    -> node opaqueInstance * Effect_sequence.t
    =
   fun opaqueComponent component : (node opaqueInstance * Effect_sequence.t) ->
    let children_, hooks =
      component.render (Hooks.ofState None ~onStateDidChange:callStaleHandlers)
    in
    let hooks = Hooks.toState hooks in
    let childElements =
      match component.childrenType with
      | React -> (children_ : childNode element)
      | Host -> children_.children
    in
    let childInstances, mountEffects = ofList childElements in
    ( Instance
        { hooks
        ; opaqueComponent
        ; component
        ; children_
        ; childInstances
        ; wrappedHostNode =
            HostNode.make component.childrenType children_ childInstances }
    , Effect_sequence.chain
        (Hooks.pendingEffects ~lifecycle:Hooks.Effect.Mount (Some hooks))
        mountEffects )

  and ofOpaqueComponent : type node.
    node opaqueComponent -> node opaqueInstance * Effect_sequence.t
    =
   fun (OpaqueComponent component as opaqueComponent) ->
    ofComponent opaqueComponent component

  and ofList : type node.
    node element -> node instanceForest * Effect_sequence.t
    =
   fun syntheticElement -> Element.map ofOpaqueComponent syntheticElement

  let pendingEffects ~lifecycle ~nextEffects ~instance:{childInstances; hooks} =
    InstanceForest.pendingEffects ~lifecycle
      (Effect_sequence.chain
         (Hooks.pendingEffects ~lifecycle (Some hooks))
         nextEffects)
      childInstances
end

module Render = struct
  let getOpaqueInstance ~useKeyTable (OpaqueComponent {key}) =
    match useKeyTable with
    | None -> None
    | Some keyTable -> OpaqueInstanceHash.lookupKey keyTable key

  type ('node, 'childNode) childElementUpdate =
    { updatedRenderedElement : ('node, 'childNode) renderedElement
    ; indexShift : int }

  module UpdateContext = struct
    type ('node, 'childNode) t =
      { shouldExecutePendingUpdates : bool
      ; useKeyTable : 'childNode OpaqueInstanceHash.t option
      ; nearestHostNode : 'node lazyHostNode
      ; nodeElement : ('node, 'childNode) hostNodeElement
      ; absoluteSubtreeIndex : int }
  end

  (** Initial render of an Element. Recurses to produce the entire tree
      ofComponent * instances. *)
  let rec renderElement : type parentNode node.
    nearestHostNode:parentNode lazyHostNode
    -> nodeElement:(parentNode, node) hostNodeElement
    -> ?useKeyTable:node OpaqueInstanceHash.t
    -> node opaqueComponent
    -> (parentNode, node) opaqueInstanceUpdate
    =
   fun ~nearestHostNode ~nodeElement ?useKeyTable opaqueComponent ->
    match getOpaqueInstance ~useKeyTable opaqueComponent with
    | Some (opaqueInstance, _) ->
        updateOpaqueInstance
          ~updateContext:
            (let open UpdateContext in
             { nearestHostNode
             ; nodeElement
             ; absoluteSubtreeIndex = 0
             ; useKeyTable
             ; shouldExecutePendingUpdates = false })
          opaqueInstance opaqueComponent
    | None ->
        let opaqueInstance, enqueuedEffects =
          Instance.ofOpaqueComponent opaqueComponent
        in
        {nearestHostNode; nodeElement; opaqueInstance; enqueuedEffects}

  and renderReactElement : type parentNode node.
    ?useKeyTable:node OpaqueInstanceHash.t
    -> parentNode lazyHostNode
    -> node element
    -> (parentNode, node) hostNodeElement
    -> (parentNode, node) renderedElement
    =
   fun ?useKeyTable neareastHostNode element nodeElement ->
    Element.fold
      (renderElement ?useKeyTable)
      element neareastHostNode nodeElement

  (** Update a previously rendered instance tree according to a new Element.

      Here's where the magic happens:

      We perform a dynamic check that two types are statically equal by way of a
      type witness! We have a value of type `instance` and another of type
      `element`, where each has their own `component 'x` with potentially
      different type variable 'x. We need to see if they are the same and if so
      safely "cast" one of the components. We do this by using a type witness
      that is able to prove type equality to the compiler through the use of an
      extensible GADT as a kind of dynamic type.

      For details, see:
      https://discuss.ocaml.org/t/types-as-first-class-citizens-in-ocaml/2030/3
      https://alan.petitepomme.net/cwn/2015.03.24.html#1

      The UpdateLog:

      The updates happen depth first and so the update log contains the deepes
      changes first. A change at depth N in the tree, causes all nodes from 0 to
      N generate an update. It's because the render tree is an immutable data
      structure. A change deep within a tree, means that the subtree of its
      parent has changed and it propagates to the root of a tree. *)
  and updateOpaqueInstance : type node parentNode.
    updateContext:(parentNode, node) UpdateContext.t
    -> node opaqueInstance
    -> node opaqueComponent
    -> (parentNode, node) opaqueInstanceUpdate
    =
   fun ~updateContext
     (Instance instance as originalOpaqueInstance)
     (OpaqueComponent nextComponent as nextOpaqueComponent) ->
    let nextState =
      match updateContext.shouldExecutePendingUpdates with
      | true -> Hooks.flushPendingStateUpdates instance.hooks
      | false -> instance.hooks
    in
    let stateChanged = nextState != instance.hooks in
    let bailOut =
      (not stateChanged) && instance.opaqueComponent == nextOpaqueComponent
    in
    if bailOut && not updateContext.shouldExecutePendingUpdates
    then
      { nearestHostNode = updateContext.nearestHostNode
      ; nodeElement = updateContext.nodeElement
      ; opaqueInstance = originalOpaqueInstance
      ; enqueuedEffects = Effect_sequence.noop }
    else
      let {component} = instance in
      match
        nextComponent.eq
          {instance with hooks = nextState}
          component.id nextComponent.id
      with
      | Some handedInstance -> (
          let ({ nearestHostNode
               ; nodeElement
               ; opaqueInstance = newOpaqueInstance
               ; enqueuedEffects } as ret)
            =
            updateInstance ~originalOpaqueInstance ~updateContext ~nextComponent
              ~nextOpaqueComponent ~stateChanged handedInstance
          in
          match newOpaqueInstance == originalOpaqueInstance with
          | true -> ret
          | false ->
              { nodeElement
              ; nearestHostNode =
                  SubtreeChange.updateNodes ~nodeElement ~parent:nearestHostNode
                    ~instanceForest:(IFlat newOpaqueInstance)
                    ~position:updateContext.absoluteSubtreeIndex
              ; opaqueInstance = newOpaqueInstance
              ; enqueuedEffects })
      | None ->
          (* Switching component type *)
          let opaqueInstance, mountEffects =
            Instance.ofOpaqueComponent nextOpaqueComponent
          in
          let enqueuedEffects =
            Instance.pendingEffects ~lifecycle:Hooks.Effect.Unmount
              ~nextEffects:mountEffects ~instance
          in
          { nodeElement = updateContext.nodeElement
          ; nearestHostNode =
              SubtreeChange.replaceSubtree
                ~nodeElement:updateContext.nodeElement
                ~parent:updateContext.nearestHostNode
                ~prevChildren:
                  (InstanceForest.outputTreeNodes (IFlat originalOpaqueInstance))
                ~nextChildren:
                  (InstanceForest.outputTreeNodes (IFlat opaqueInstance))
                ~absoluteSubtreeIndex:updateContext.absoluteSubtreeIndex
          ; opaqueInstance
          ; enqueuedEffects }

  and updateInstance :
        type hooks node children childNode wrappedHostNode parentNode.
    originalOpaqueInstance:node opaqueInstance
    -> updateContext:(parentNode, node) UpdateContext.t
    -> nextComponent:
         (hooks * (node * children * childNode * wrappedHostNode)) component
    -> nextOpaqueComponent:node opaqueComponent
    -> stateChanged:bool
    -> (hooks * (node * children * childNode * wrappedHostNode)) instance
    -> (parentNode, node) opaqueInstanceUpdate
    =
   fun ~originalOpaqueInstance
     ~updateContext
     ~nextComponent
     ~nextOpaqueComponent
     ~stateChanged
     instance ->
    let updatedInstanceWithNewElement =
      { instance with
        component = nextComponent
      ; opaqueComponent = nextOpaqueComponent }
    in
    let shouldRerender =
      stateChanged || nextOpaqueComponent != instance.opaqueComponent
    in
    let nextSubElements, initialHooks =
      if shouldRerender
      then
        let nextElement, initialHooks =
          nextComponent.render
            (Hooks.ofState (Some updatedInstanceWithNewElement.hooks)
               ~onStateDidChange:callStaleHandlers)
        in
        nextElement, Hooks.toState initialHooks
      else instance.children_, instance.hooks
    in
    let updatedInstanceWithNewState =
      {updatedInstanceWithNewElement with hooks = initialHooks}
    in
    let {children_; childInstances} = updatedInstanceWithNewState in
    let ( (nearestHostNode : parentNode lazyHostNode)
        , updatedInstanceWithNewSubtree
        , enqueuedEffects
        , (nodeElement : (parentNode, node) hostNodeElement) )
      =
      match nextComponent.childrenType with
      | React -> (
          let { nearestHostNode
              ; nodeElement
              ; instanceForest = nextInstanceSubForest
              ; enqueuedEffects }
            =
            updateInstanceSubtree ~updateContext
              ~oldInstanceForest:childInstances ~oldReactElement:children_
              ~nextReactElement:nextSubElements ()
          in
          match nextInstanceSubForest != childInstances with
          | true ->
              ( nearestHostNode
              , ({ updatedInstanceWithNewState with
                   children_ = nextSubElements
                 ; childInstances = nextInstanceSubForest }
                 : (hooks * (node * children * childNode * wrappedHostNode))
                     instance)
              , enqueuedEffects
              , nodeElement )
          | false ->
              ( nearestHostNode
              , updatedInstanceWithNewState
              , enqueuedEffects
              , nodeElement ))
      | Host ->
          let instanceWithNewHostView =
            ((match shouldRerender with
             | true ->
                 { updatedInstanceWithNewState with
                   wrappedHostNode =
                     lazy
                       (let instance =
                          Lazy.force updatedInstanceWithNewState.wrappedHostNode
                        in
                        let (Node beforeUpdate | UpdatedNode (_, beforeUpdate)) =
                          instance
                        in
                        let afterUpdate =
                          nextSubElements.configureInstance ~isFirstRender:false
                            beforeUpdate
                        in
                        match afterUpdate == beforeUpdate with
                        | true -> instance
                        | false -> UpdatedNode (beforeUpdate, afterUpdate)) }
             | false -> updatedInstanceWithNewState)
             : (hooks * (node * children * childNode * wrappedHostNode))
                 instance)
          in
          let { nearestHostNode = wrappedHostNode
              ; instanceForest = nextInstanceSubForest
              ; enqueuedEffects }
            =
            updateInstanceSubtree
              ~updateContext:
                { UpdateContext.shouldExecutePendingUpdates =
                    updateContext.shouldExecutePendingUpdates
                ; useKeyTable = None
                ; absoluteSubtreeIndex = 0
                ; nearestHostNode =
                    (instanceWithNewHostView.wrappedHostNode
                     : node lazyHostNode)
                ; nodeElement =
                    (instanceWithNewHostView.children_
                     : (node, childNode) hostNodeElement) }
              ~oldInstanceForest:childInstances
              ~oldReactElement:children_.children
              ~nextReactElement:nextSubElements.children ()
          in
          if nextInstanceSubForest != instanceWithNewHostView.childInstances
          then
            ( updateContext.nearestHostNode
            , ({ instanceWithNewHostView with
                 childInstances = nextInstanceSubForest
               ; children_ = nextSubElements
               ; wrappedHostNode }
               : (hooks * (node * children * childNode * wrappedHostNode))
                   instance)
            , enqueuedEffects
            , updateContext.nodeElement )
          else
            ( updateContext.nearestHostNode
            , instanceWithNewHostView
            , enqueuedEffects
            , updateContext.nodeElement )
    in
    if
      updatedInstanceWithNewSubtree == updatedInstanceWithNewElement
      && not stateChanged
    then
      { nodeElement
      ; nearestHostNode
      ; opaqueInstance = originalOpaqueInstance
      ; enqueuedEffects }
    else
      { nodeElement
      ; nearestHostNode
      ; opaqueInstance = Instance updatedInstanceWithNewSubtree
      ; enqueuedEffects =
          Effect_sequence.chain
            (Hooks.pendingEffects ~lifecycle:Hooks.Effect.Update
               (Some updatedInstanceWithNewSubtree.hooks))
            enqueuedEffects }

  (** updateRenderedElement recurses through the syntheticElement tree as long
      as the oldReactElement and nextReactElement have the same shape.

      The base case is either an empty list - Nested([]) or a Flat element.

      syntheticElement is a recursive tree like data structure. The tree doesn't
      contain children of the syntheticElements returned from children, it only
      contains the "immediate" children so to speak including all nested lists.

      `keyTable` is a hash table containing all keys in the syntheticElement
      tree. *)
  and updateInstanceSubtree : type parentNode node.
    updateContext:(parentNode, node) UpdateContext.t
    -> oldInstanceForest:node instanceForest
    -> oldReactElement:node element
    -> nextReactElement:node element
    -> unit
    -> (parentNode, node) renderedElement
    =
   fun ~updateContext
     ~oldInstanceForest
     ~oldReactElement
     ~nextReactElement
     () ->
    match oldInstanceForest, oldReactElement, nextReactElement with
    | ( INested (instanceSubTrees, subtreeSize)
      , Nested oldReactElements
      , Nested (nextReactElement :: nextReactElements) )
      when nextReactElements == oldReactElements ->
        let { nearestHostNode
            ; nodeElement
            ; instanceForest = addedElement
            ; enqueuedEffects }
          =
          renderReactElement updateContext.nearestHostNode nextReactElement
            updateContext.nodeElement
        in
        { nodeElement
        ; nearestHostNode =
            SubtreeChange.insertElement ~nodeElement ~parent:nearestHostNode
              ~children:(InstanceForest.outputTreeNodes addedElement)
              ~position:updateContext.absoluteSubtreeIndex
        ; instanceForest =
            INested
              ( addedElement :: instanceSubTrees
              , subtreeSize + InstanceForest.getSubtreeSize addedElement )
        ; enqueuedEffects }
    | ( INested (oldInstanceForests, _)
      , Nested oldReactElements
      , Nested nextReactElements )
      when List.length nextReactElements == List.length oldInstanceForests ->
        let keyTable = OpaqueInstanceHash.createKeyTable oldInstanceForest in
        let ( nearestHostNode
            , newInstanceForests
            , subtreeSize
            , _indexShift
            , enqueuedEffects
            , nodeElement )
          =
          List_tailrec.fold3
            (fun ( nearestHostNode
                 , renderedElements
                 , prevSubtreeSize
                 , indexShift
                 , enqueuedEffectsAcc
                 , nodeElement )
              oldInstanceForest
              oldReactElement
              nextReactElement ->
               let { indexShift
                   ; updatedRenderedElement =
                       { nearestHostNode
                       ; instanceForest
                       ; enqueuedEffects
                       ; nodeElement } }
                 =
                 updateChildRenderedElement
                   ~updateContext:
                     { updateContext with
                       nearestHostNode
                     ; useKeyTable = Some keyTable
                     ; absoluteSubtreeIndex = prevSubtreeSize
                     ; nodeElement }
                   ~indexShift ~oldInstanceForest ~oldReactElement
                   ~nextReactElement ()
               in
               ( nearestHostNode
               , instanceForest :: renderedElements
               , prevSubtreeSize + InstanceForest.getSubtreeSize instanceForest
               , indexShift
               , Effect_sequence.chain enqueuedEffects enqueuedEffectsAcc
               , nodeElement ))
            oldInstanceForests oldReactElements nextReactElements
            ( updateContext.nearestHostNode
            , []
            , updateContext.absoluteSubtreeIndex
            , 0
            , Effect_sequence.noop
            , updateContext.nodeElement )
        in
        let newInstanceForests = List.rev newInstanceForests in
        { nodeElement
        ; nearestHostNode
        ; instanceForest = INested (newInstanceForests, subtreeSize)
        ; enqueuedEffects }
    | ( IFlat (Instance oldInstance as oldOpaqueInstance)
      , Flat (OpaqueComponent {key = oldKey})
      , Flat (OpaqueComponent {key = nextKey} as nextReactElement) ) ->
        if nextKey != oldKey
        then
          let { nearestHostNode
              ; opaqueInstance = newOpaqueInstance
              ; enqueuedEffects = mountEffects
              ; nodeElement }
            =
            renderElement nextReactElement
              ~nearestHostNode:updateContext.nearestHostNode
              ~nodeElement:updateContext.nodeElement
          in
          let enqueuedEffects =
            Instance.pendingEffects ~lifecycle:Unmount ~nextEffects:mountEffects
              ~instance:oldInstance
          in
          let newInstanceForest = IFlat newOpaqueInstance in
          { nodeElement
          ; nearestHostNode =
              SubtreeChange.replaceSubtree ~nodeElement ~parent:nearestHostNode
                ~prevChildren:(InstanceForest.outputTreeNodes oldInstanceForest)
                ~nextChildren:(InstanceForest.outputTreeNodes newInstanceForest)
                ~absoluteSubtreeIndex:0
          ; instanceForest = newInstanceForest
          ; enqueuedEffects }
        else
          let { nearestHostNode
              ; opaqueInstance = newOpaqueInstance
              ; enqueuedEffects
              ; nodeElement }
            =
            updateOpaqueInstance
              ~updateContext:{updateContext with useKeyTable = None}
              oldOpaqueInstance nextReactElement
          in
          { nodeElement
          ; nearestHostNode
          ; instanceForest =
              (match oldOpaqueInstance != newOpaqueInstance with
              | true -> IFlat newOpaqueInstance
              | false -> oldInstanceForest)
          ; enqueuedEffects }
    | oldInstanceForest, _, _ ->
        let keyTable =
          match updateContext.useKeyTable with
          | None -> OpaqueInstanceHash.createKeyTable oldInstanceForest
          | Some keyTable -> keyTable
        in
        let { nearestHostNode
            ; instanceForest = newInstanceForest
            ; enqueuedEffects = mountEffects
            ; nodeElement }
          =
          renderReactElement ~useKeyTable:keyTable updateContext.nearestHostNode
            nextReactElement updateContext.nodeElement
        in
        let enqueuedEffects =
          InstanceForest.pendingEffects ~lifecycle:Hooks.Effect.Unmount
            mountEffects oldInstanceForest
        in
        { nodeElement
        ; nearestHostNode =
            SubtreeChange.replaceSubtree ~nodeElement ~parent:nearestHostNode
              ~prevChildren:(InstanceForest.outputTreeNodes oldInstanceForest)
              ~nextChildren:(InstanceForest.outputTreeNodes newInstanceForest)
              ~absoluteSubtreeIndex:updateContext.absoluteSubtreeIndex
        ; instanceForest = newInstanceForest
        ; enqueuedEffects }

  and updateChildRenderedElement : type parentNode node.
    updateContext:(parentNode, node) UpdateContext.t
    -> indexShift:int
    -> oldInstanceForest:node instanceForest
    -> oldReactElement:node element
    -> nextReactElement:node element
    -> unit
    -> (parentNode, node) childElementUpdate
    =
   fun ~updateContext:
         { UpdateContext.shouldExecutePendingUpdates
         ; useKeyTable
         ; nearestHostNode
         ; nodeElement
         ; absoluteSubtreeIndex }
     ~indexShift
     ~oldInstanceForest
     ~oldReactElement
     ~nextReactElement
     () ->
    match oldInstanceForest, oldReactElement, nextReactElement with
    | ( IFlat oldOpaqueInstance
      , Flat (OpaqueComponent {key = oldKey})
      , Flat (OpaqueComponent {key = nextKey} as nextReactElement) ) -> (
        let keyTable =
          match useKeyTable with
          | None -> OpaqueInstanceHash.createKeyTable (IFlat oldOpaqueInstance)
          | Some keyTable -> keyTable
        in
        let nearestHostNode, update, newOpaqueInstance, enqueuedEffects =
          let (OpaqueComponent component) = nextReactElement in
          if component.key != Key.none
          then
            match OpaqueInstanceHash.lookupKey keyTable component.key with
            | Some (subOpaqueInstance, previousIndex) ->
                let { nearestHostNode
                    ; opaqueInstance = updatedOpaqueInstance
                    ; enqueuedEffects }
                  =
                  updateOpaqueInstance
                    ~updateContext:
                      (let open UpdateContext in
                       { useKeyTable
                       ; shouldExecutePendingUpdates
                       ; nearestHostNode
                       ; absoluteSubtreeIndex = previousIndex + indexShift
                       ; nodeElement })
                    subOpaqueInstance nextReactElement
                in
                ( nearestHostNode
                , `NoChangeOrNested previousIndex
                , updatedOpaqueInstance
                , enqueuedEffects )
            | None ->
                let { nearestHostNode
                    ; opaqueInstance = newOpaqueInstance
                    ; enqueuedEffects }
                  =
                  renderElement ~nearestHostNode nextReactElement ~nodeElement
                in
                nearestHostNode, `NewElement, newOpaqueInstance, enqueuedEffects
          else
            let { nearestHostNode
                ; opaqueInstance = updatedOpaqueInstance
                ; enqueuedEffects }
              =
              updateOpaqueInstance
                ~updateContext:
                  (let open UpdateContext in
                   { nodeElement
                   ; shouldExecutePendingUpdates
                   ; nearestHostNode
                   ; absoluteSubtreeIndex
                   ; useKeyTable })
                oldOpaqueInstance nextReactElement
            in
            ( nearestHostNode
            , `NoChangeOrNested absoluteSubtreeIndex
            , updatedOpaqueInstance
            , enqueuedEffects )
        in
        match update with
        | `NewElement ->
            let newInstanceForest = IFlat newOpaqueInstance in
            { updatedRenderedElement =
                { nearestHostNode =
                    SubtreeChange.replaceSubtree ~nodeElement
                      ~parent:nearestHostNode
                      ~prevChildren:
                        (InstanceForest.outputTreeNodes oldInstanceForest)
                      ~nextChildren:
                        (InstanceForest.outputTreeNodes newInstanceForest)
                      ~absoluteSubtreeIndex
                ; instanceForest = newInstanceForest
                ; enqueuedEffects
                ; nodeElement }
            ; indexShift = 0 }
        | `NoChangeOrNested previousIndex ->
            let changed = oldOpaqueInstance != newOpaqueInstance in
            let element =
              match changed with
              | true -> IFlat newOpaqueInstance
              | false -> oldInstanceForest
            in
            if oldKey <> nextKey
            then
              { updatedRenderedElement =
                  { nearestHostNode =
                      SubtreeChange.reorder ~nodeElement ~parent:nearestHostNode
                        ~instance:newOpaqueInstance ~indexShift
                        ~from:previousIndex ~to_:absoluteSubtreeIndex
                  ; instanceForest = element
                  ; enqueuedEffects
                  ; nodeElement }
              ; indexShift = InstanceForest.getSubtreeSize element }
            else
              { updatedRenderedElement =
                  { nearestHostNode
                  ; instanceForest = element
                  ; enqueuedEffects
                  ; nodeElement }
              ; indexShift = 0 })
    | _, _, _ ->
        { updatedRenderedElement =
            updateInstanceSubtree
              ~updateContext:
                { UpdateContext.absoluteSubtreeIndex
                ; nodeElement
                ; shouldExecutePendingUpdates
                ; nearestHostNode
                ; useKeyTable }
              ~oldInstanceForest ~oldReactElement ~nextReactElement ()
        ; indexShift = 0 }

  (** Execute the pending updates at the top level of an instance tree. If no
      state change is performed, the argument is returned unchanged. *)
  let flushPendingUpdates opaqueInstance nearestHostNode nodeElement =
    let (Instance {opaqueComponent}) = opaqueInstance in
    updateOpaqueInstance
      ~updateContext:
        (let open UpdateContext in
         { useKeyTable = None
         ; shouldExecutePendingUpdates = true
         ; nearestHostNode
         ; nodeElement
         ; absoluteSubtreeIndex = 0 })
      opaqueInstance opaqueComponent
end

module RenderedElement = struct
  type ('node, 'childNode) t = ('node, 'childNode) renderedElement
  (** Rendering produces a list of instance trees. *)

  type ('node, 'childNode) root =
    { node : 'node
    ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
    ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
    ; moveNode :
        parent:'node -> child:'childNode -> from:int -> to_:int -> 'node }

  let listToRenderedElement renderedElements =
    INested
      ( renderedElements
      , renderedElements
        |> List.fold_left (fun acc e -> acc + InstanceForest.getSubtreeSize e) 0
      )

  let render root children =
    let instanceForest, mountEffects = Instance.ofList children in
    { nodeElement =
        { make = (fun () -> root.node)
        ; configureInstance = (fun ~isFirstRender:_ i -> i)
        ; children
        ; insertNode = root.insertNode
        ; deleteNode = root.deleteNode
        ; moveNode = root.moveNode }
    ; instanceForest
    ; nearestHostNode =
        lazy
          (Node
             (InstanceForest.outputTreeNodes instanceForest
             |> List.fold_left
                  (fun (position, parent) child ->
                     ( position + 1
                     , let (Node child | UpdatedNode (_, child)) =
                         Lazy.force child
                       in
                       let parent = root.insertNode ~parent ~child ~position in
                       parent ))
                  (0, root.node)
             |> snd))
    ; enqueuedEffects = mountEffects }

  let update
        ~previousElement
        ~renderedElement:{instanceForest; nearestHostNode; nodeElement}
        nextReactElement
    =
    Render.updateInstanceSubtree
      ~updateContext:
        (let open Render.UpdateContext in
         { nodeElement
         ; nearestHostNode
         ; absoluteSubtreeIndex = 0
         ; useKeyTable = None
         ; shouldExecutePendingUpdates = false })
      ~oldInstanceForest:instanceForest ~oldReactElement:previousElement
      ~nextReactElement ()

  let rec map f renderedElement nearestHostNode nodeElement =
    match renderedElement with
    | IFlat e ->
        let {nearestHostNode; opaqueInstance; enqueuedEffects; nodeElement} =
          f e nearestHostNode nodeElement
        in
        let unchanged = e == opaqueInstance in
        { nodeElement
        ; nearestHostNode
        ; instanceForest =
            (match unchanged with
            | true -> renderedElement
            | false -> IFlat opaqueInstance)
        ; enqueuedEffects }
    | INested (l, _) ->
        let nextL, nearestHostNode, effects, nodeElement =
          List.fold_left
            (fun (acc, nearestHostNode, effectsAcc, nodeElement)
              renderedElement ->
               let { nearestHostNode
                   ; instanceForest = next
                   ; enqueuedEffects
                   ; nodeElement }
                 =
                 map f renderedElement nearestHostNode nodeElement
               in
               ( next :: acc
               , nearestHostNode
               , Effect_sequence.chain effectsAcc enqueuedEffects
               , nodeElement ))
            ([], nearestHostNode, Effect_sequence.noop, nodeElement)
            (List.rev l)
        in
        let unchanged = List.for_all2 ( == ) l nextL in
        { nodeElement
        ; nearestHostNode
        ; instanceForest =
            (match unchanged with
            | true -> renderedElement
            | false ->
                INested
                  ( nextL
                  , List.fold_left
                      (fun acc elem -> InstanceForest.getSubtreeSize elem + acc)
                      0 nextL ))
        ; enqueuedEffects = effects }

  (** Flush the pending updates in an instance tree. *)
  let flushPendingUpdates
        {instanceForest; nearestHostNode; enqueuedEffects; nodeElement}
    =
    let { nearestHostNode
        ; instanceForest = newInstanceForest
        ; enqueuedEffects = nextEnqueuedEffects
        ; nodeElement }
      =
      map Render.flushPendingUpdates instanceForest nearestHostNode nodeElement
    in
    { nodeElement
    ; instanceForest = newInstanceForest
    ; nearestHostNode
    ; enqueuedEffects =
        Effect_sequence.chain nextEnqueuedEffects enqueuedEffects }

  let executeHostViewUpdates ({nearestHostNode} : (_, _) t) =
    let (Node hostView | UpdatedNode (_, hostView)) =
      Lazy.force nearestHostNode
    in
    hostView

  let executePendingEffects ({enqueuedEffects} as renderedElement : (_, _) t) =
    enqueuedEffects ();
    {renderedElement with enqueuedEffects = Effect_sequence.noop}
end

let element ?key:(argumentKey = Key.none) component =
  let key =
    match argumentKey <> Key.none with
    | true -> argumentKey
    | false -> (
        let isDynamicKey = component.key = Key.dynamicKeyMagicNumber in
        match isDynamicKey with true -> Key.create () | false -> Key.none)
  in
  let componentWithKey =
    match key <> component.key with
    | true -> {component with key}
    | false -> component
  in
  Flat (OpaqueComponent componentWithKey)

let listToElement l = Nested l
let empty = Nested []

module Hooks = Hooks
module Remote_action = Remote_action

module Expert = struct
  let jsx_list = listToElement

  let component : type a node.
    ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> ((a, a) Hooks.t -> node element * (Hooks.nil, a) Hooks.t)
    -> node element
    =
   fun ?(useDynamicKey = false) debugName ->
    let module Component = struct
      type 'a componentId +=
        | Id :
            (a * (node * node element * node * node lazyHostNodeList)) instance
              componentId

      let eq : type c.
        c
        -> c componentId
        -> (a * (node * node element * node * node lazyHostNodeList)) instance
             componentId
        -> (a * (node * node element * node * node lazyHostNodeList)) instance
             option
        =
       fun instance id1 id2 ->
        match id1, id2 with Id, Id -> Some instance | _, _ -> None
    end
    in
    fun ?key render ->
      element ?key
        { debugName
        ; childrenType = React
        ; key =
            (match useDynamicKey with
            | true -> Key.dynamicKeyMagicNumber
            | false -> Key.none)
        ; id = Component.Id
        ; eq = Component.eq
        ; render }

  let nativeComponent : type a node childNode.
    ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> ((a, a) Hooks.t
        -> (node, childNode) hostNodeElement * (Hooks.nil, a) Hooks.t)
    -> node element
    =
   fun ?(useDynamicKey = false) debugName ->
    let module Component = struct
      type 'a componentId +=
        | Id :
            (a
            * (node
              * (node, childNode) hostNodeElement
              * childNode
              * node lazyHostNode))
              instance
              componentId

      let eq : type c.
        c
        -> c componentId
        -> (a
           * (node
             * (node, childNode) hostNodeElement
             * childNode
             * node lazyHostNode))
             instance
             componentId
        -> (a
           * (node
             * (node, childNode) hostNodeElement
             * childNode
             * node lazyHostNode))
             instance
             option
        =
       fun instance id1 id2 ->
        match id1, id2 with Id, Id -> Some instance | _, _ -> None
    end
    in
    fun ?key render ->
      element ?key
        { debugName
        ; childrenType = Host
        ; key =
            (match useDynamicKey with
            | true -> Key.dynamicKeyMagicNumber
            | false -> Key.none)
        ; id = Component.Id
        ; eq = Component.eq
        ; render }
end

let component ?useDynamicKey debugName =
  let c = Expert.component ?useDynamicKey debugName in
  fun ?key render ->
    c ?key (fun hooks ->
      let hooks, e = render hooks in
      e, hooks)

let nativeComponent ?useDynamicKey debugName =
  let c = Expert.nativeComponent ?useDynamicKey debugName in
  fun ?key render ->
    c ?key (fun hooks ->
      let hooks, e = render hooks in
      e, hooks)

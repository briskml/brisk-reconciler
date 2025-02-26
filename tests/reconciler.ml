type hostElement = Text of string | View [@@deriving show, eq]
type node = {name : string; element : hostElement} [@@deriving show, eq]

type testMountEntry =
  | MountChild of node * node * int
  | UnmountChild of node * node
  | RemountChild of node * node * int * int
  | ChangeText of string * string
[@@deriving show, eq]

type testMountLog = testMountEntry list [@@deriving show, eq]

let mountLog = ref []
let isDirty = ref false;;

Brisk_reconciler.addStaleTreeHandler (fun () -> isDirty := true)

let insertNode ~(parent : node) ~(child : node) ~(position : int) =
  (match child.element with
  | _ -> mountLog := MountChild (parent, child, position) :: !mountLog);
  parent

let deleteNode ~(parent : node) ~(child : node) ~position:_ =
  (match child.element with
  | _ -> mountLog := UnmountChild (parent, child) :: !mountLog);
  parent

let moveNode ~(parent : node) ~(child : node) ~(from : int) ~(to_ : int) =
  mountLog := RemountChild (parent, child, from, to_) :: !mountLog;
  parent

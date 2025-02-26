module type Witness = sig
  type 'a t
end

module type S = sig
  type 'a witness
  type 'a valueContainer = {value : 'a; toWitness : 'a -> 'a witness}
  type nil = Nil

  type 'list t =
    | [] : nil t
    | ( :: ) : 'a valueContainer * 'l t -> ('a -> 'l) t

  type 'a constructor = private 'tail t -> 'after t
    constraint 'a = 'tail * 'after

  type 'value init = ('value * 'value) constructor

  val init : 'value init

  val append :
     'value valueContainer
    -> (('value -> 'b) * 'c) constructor
    -> ('b * 'c) constructor

  val seal : (nil * 'a) constructor -> 'a t
  val dropFirst : ('a -> 'b) t -> 'a valueContainer * 'b t

  type opaqueValue = Any : 'a witness -> opaqueValue

  val iter : (opaqueValue -> unit) -> 'a t -> unit
  val fold : ('acc -> opaqueValue -> 'acc) -> 'acc -> 'a t -> 'acc

  type mapper = {f : 'a. 'a witness -> 'a option}

  val map : mapper -> 'a t -> 'a t
  val compareElementsIdentity : 'a t -> 'a t -> bool
end

module Make : functor (Witness : Witness) ->
  S with type 'a witness = 'a Witness.t

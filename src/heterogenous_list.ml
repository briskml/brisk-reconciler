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

module Make (Witness : Witness) : S with type 'a witness = 'a Witness.t = struct
  type 'a witness = 'a Witness.t
  type 'a valueContainer = {value : 'a; toWitness : 'a -> 'a witness}
  type nil = Nil

  type 'list t =
    | [] : nil t
    | ( :: ) : 'a valueContainer * 'l t -> ('a -> 'l) t

  type 'a constructor = 'tail t -> 'after t constraint 'a = 'tail * 'after
  type 'value init = ('value * 'value) constructor

  let init = (fun a -> a : 'value init)
  let append value x : 'a constructor = fun hole -> x (value :: hole)
  let seal = (fun x -> x [] : (nil * 'a) constructor -> 'a t)

  let dropFirst : type a b. (a -> b) t -> a valueContainer * b t = function
    | a :: q -> a, q

  type opaqueValue = Any : 'a witness -> opaqueValue

  let rec iter : type a. (opaqueValue -> unit) -> a t -> unit =
   fun f l ->
    match l with
    | [] -> ()
    | {value; toWitness} :: t ->
        f (Any (toWitness value));
        iter f t

  let rec fold : type a acc. (acc -> opaqueValue -> acc) -> acc -> a t -> acc =
   fun f acc l ->
    match l with
    | [] -> acc
    | {value; toWitness} :: t -> fold f (f acc (Any (toWitness value))) t

  type mapper = {f : 'a. 'a witness -> 'a option}

  let rec map : type a. mapper -> a t -> a t =
   fun mapper l ->
    match l with
    | [] -> l
    | {value; toWitness} :: t ->
        let mapped = mapper.f (toWitness value) in
        {value = (match mapped with Some x -> x | None -> value); toWitness}
        :: map mapper t

  let rec compareElementsIdentity : type a. a t -> a t -> bool =
   fun l1 l2 ->
    match l1, l2 with
    | [], [] -> true
    | {value} :: t1, {value = value2} :: t2 ->
        value == value2 && compareElementsIdentity t1 t2
end

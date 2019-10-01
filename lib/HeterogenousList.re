module type Witness = {type t('a);};

module type S = {
  type witness('a);

  type valueContainer('a) = {
    value: 'a,
    toWitness: 'a => witness('a),
  };

  type nil =
    | Nil;

  type t('list) =
    | []: t(nil)
    | ::(valueContainer('a), t('l)): t('a => 'l);

  type constructor('a) = pri t('tail) => t('after)
  constraint 'a = ('tail, 'after);

  type init('value) = constructor(('value, 'value));

  let init: init('value);

  let append:
    (valueContainer('value), constructor(('value => 'b, 'c))) =>
    constructor(('b, 'c));

  let seal: constructor((nil, 'a)) => t('a);

  let dropFirst: t('a => 'b) => (valueContainer('a), t('b));

  type opaqueValue =
    | Any(witness('a)): opaqueValue;

  let iter: (opaqueValue => unit, t('a)) => unit;

  let fold: (('acc, opaqueValue) => 'acc, 'acc, t('a)) => 'acc;

  type mapper = {f: 'a. witness('a) => option('a)};

  let map: (mapper, t('a)) => t('a);

  let compareElementsIdentity: (t('a), t('a)) => bool;
};

module Make = (Witness: Witness) : (S with type witness('a) = Witness.t('a)) => {
  type witness('a) = Witness.t('a);

  type valueContainer('a) = {
    value: 'a,
    toWitness: 'a => witness('a),
  };

  type nil =
    | Nil;

  type t('list) =
    | []: t(nil)
    | ::(valueContainer('a), t('l)): t('a => 'l);

  type constructor('a) = t('tail) => t('after)
  constraint 'a = ('tail, 'after);

  type init('value) = constructor(('value, 'value));

  let init: init('value) = a => a;

  let append = (value, x): constructor('a) => hole => x([value, ...hole]);

  let seal: constructor((nil, 'a)) => t('a) = x => x([]);

  let dropFirst: type a b. t(a => b) => (valueContainer(a), t(b)) =
    fun
    | [a, ...q] => (a, q);

  type opaqueValue =
    | Any(witness('a)): opaqueValue;

  let rec iter: type a. (opaqueValue => unit, t(a)) => unit =
    (f, l) =>
      switch (l) {
      | [] => ()
      | [{value, toWitness}, ...t] =>
        f(Any(toWitness(value)));
        iter(f, t);
      };

  let rec fold: type a acc. ((acc, opaqueValue) => acc, acc, t(a)) => acc =
    (f, acc, l) =>
      switch (l) {
      | [] => acc
      | [{value, toWitness}, ...t] =>
        fold(f, f(acc, Any(toWitness(value))), t)
      };

  type mapper = {f: 'a. witness('a) => option('a)};

  let rec map: type a. (mapper, t(a)) => t(a) =
    (mapper, l) =>
      switch (l) {
      | [] => l
      | [{value, toWitness}, ...t] =>
        let mapped = mapper.f(toWitness(value));
        [
          {
            value:
              switch (mapped) {
              | Some(x) => x
              | None => value
              },
            toWitness,
          },
          ...map(mapper, t),
        ];
      };
  let rec compareElementsIdentity: type a. (t(a), t(a)) => bool =
    (l1, l2) => {
      switch (l1, l2) {
      | ([], []) => true
      | ([{value}, ...t1], [{value: value2}, ...t2]) =>
        value === value2 && compareElementsIdentity(t1, t2)
      };
    };
};

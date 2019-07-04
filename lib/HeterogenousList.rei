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

module Make: (Witness: Witness) => S with type witness('a) = Witness.t('a);

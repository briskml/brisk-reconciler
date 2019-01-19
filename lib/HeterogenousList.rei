module type Witness = {type t('a);};

module type S = {
  type witness('a);

  type valueContainer('a) = {
    value: 'a,
    toWitness: 'a => witness('a),
  };

  type t('list, 'last) =
    | []: t('a, 'a)
    | ::(valueContainer('a), t('l, 't)): t('a => 'l, 't);

  let append:
    (t('start, 'mid => 'rest), 'mid, 'mid => witness('mid)) =>
    t('start, 'rest);

  let dropFirst: t('a => 'b, unit) => (valueContainer('a), t('b, unit));

  type opaqueValue =
    | Any(witness('a)): opaqueValue;

  let iter: (opaqueValue => unit, t('a, 'b)) => unit;

  let fold: (('acc, opaqueValue) => 'acc, 'acc, t('a, 'b)) => 'acc;

  type mapper = {f: 'a. witness('a) => option('a)};

  let map: (mapper, t('a, 'b)) => t('a, 'b);
};

module Make: (Witness: Witness) => S with type witness('a) = Witness.t('a);

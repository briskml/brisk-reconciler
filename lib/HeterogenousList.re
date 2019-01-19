module type Witness = {
  type t('a);
};

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

module Make = (Witness: Witness) => {
  type witness('a) = Witness.t('a);

  type valueContainer('a) = {
    value: 'a,
    toWitness: 'a => Witness.t('a),
  };

  type t('list, 'last) =
    | []: t('a, 'a)
    | ::(valueContainer('a), t('l, 't)): t('a => 'l, 't);

  let rec append:
    type start mid rest.
      (t(start, mid => rest), mid, mid => Witness.t(mid)) => t(start, rest) =
    (l, value, toWitness) =>
      switch (l) {
      | [] => [{value, toWitness}]
      | [a, ...q] => [a, ...append(q, value, toWitness)]
      };

  let dropFirst:
    type a b. t(a => b, unit) => (valueContainer(a), t(b, unit)) =
    fun
    | [a, ...q] => (a, q);

  type opaqueValue =
    | Any(Witness.t('a)): opaqueValue;

  let rec iter: type a b. (opaqueValue => unit, t(a, b)) => unit =
    (f, l) =>
      switch (l) {
      | [] => ()
      | [{value, toWitness}, ...t] =>
        f(Any(toWitness(value)));
        iter(f, t);
      };

  let rec fold: type a b acc. ((acc, opaqueValue) => acc, acc, t(a, b)) => acc =
    (f, acc, l) =>
      switch (l) {
      | [] => acc
      | [{value, toWitness}, ...t] =>
        fold(f, f(acc, Any(toWitness(value))), t)
      };
  type mapper = {f: 'a. witness('a) => option('a)};

  let rec map: type a b. (mapper, t(a, b)) => t(a, b) =
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
};

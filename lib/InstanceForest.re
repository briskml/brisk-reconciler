type t('node) = CoreTypes.instanceForest('node);

let pendingEffects = (~lifecycle, instanceForest) => {
  let f = (acc, CoreTypes.Instance({hooks})) =>
    EffectSequence.chain(
      Hooks.pendingEffects(~lifecycle, Some(hooks)),
      acc,
    );
  let rec fold: type any. (EffectSequence.t, t(any)) => EffectSequence.t =
    (acc, instanceForest) => {
      switch (instanceForest) {
      | IFlat(Instance({childInstances}) as opaqueInstance) =>
        f(fold(acc, childInstances), opaqueInstance)
      | INested(l, _) =>
        List.fold_left(
          (acc, instanceForest) => fold(acc, instanceForest),
          acc,
          l,
        )
      | IDiffableSequence(l, _) =>
        Seq.fold_left(
          (acc, instanceForest) => fold(acc, instanceForest),
          acc,
          l.toSeq(),
        )
      };
    };
  fold(EffectSequence.noop, instanceForest);
};

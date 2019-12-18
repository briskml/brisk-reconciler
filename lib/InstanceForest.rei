let pendingEffects:
  (
    ~lifecycle: Hooks.Effect.lifecycle,
    CoreTypes.instanceForest(_)
  ) =>
  EffectSequence.t;

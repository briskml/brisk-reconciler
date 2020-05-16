type t('node) = CoreTypes.element('node);

let rec fold = (~f, ~init, renderedElement) => {
  let foldSequence = (seq, container, add, wrapResult) => {
    let update =
      Seq.fold_left(
        (update: Update.t(_, _, _), element) => {
          // Append child nodes here
          fold(~f, ~init=update.Update.hostTreeUpdate, element)
          |> Update.map(payload => add(update.Update.payload, payload))
          |> Update.mapEffects(nextEffects =>
               EffectSequence.chain(
                 update.Update.enqueuedEffects,
                 nextEffects,
               )
             )
        },
        {
          Update.payload: container,
          enqueuedEffects: EffectSequence.noop,
          hostTreeUpdate: init,
          childNodes: Seq.empty,
        },
        seq,
      );
    update
    |> Update.map(payload =>
         wrapResult(
           payload,
           update.hostTreeUpdate.absoluteSubtreeIndex
           - init.absoluteSubtreeIndex,
         )
       );
  };
  CoreTypes.(
    switch (renderedElement) {
    | Leaf(c) =>
      f(~hostTreeState=init, ~component=c)
      |> Update.map(instance => IFlat(instance))
    | DiffableSequence(seq) =>
      foldSequence(
        seq.toSeq(),
        seq.empty(),
        (instances, instance) => instances.insert(instance),
        (seq, length) => IDiffableSequence(seq, length),
      )
    | StaticList(l) =>
      foldSequence(
        l |> List.to_seq,
        [],
        (rest, h) => [h, ...rest],
        (list, length) => INested(list, length),
      )

    | Movable(l, ref) =>
      let res = fold(~f, ~init, l);
      ref :=
        Some({
          instanceForest: res.payload,
          /* THIS IS BROKEN */
          index: 0,
          subtreeSize: 0,
        });
      res;
    }
  );
};

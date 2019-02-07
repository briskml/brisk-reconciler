include Brisk_reconciler.Make({
  type node = unit;

  let insertNode = (~parent, ~child as _, ~position as _) => {
    parent;
  };

  let deleteNode = (~parent, ~child as _) => {
    parent;
  };

  let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
    parent;
  };

  let markAsStale = () => ();
  let beginChanges = () => ();
  let commitChanges = () => ();
});

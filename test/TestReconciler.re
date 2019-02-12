module Implementation = {
  [@deriving (show({with_path: false}), eq)]
  type hostElement =
    | Text(string)
    | View;

  [@deriving (show({with_path: false}), eq)]
  type node = {
    name: string,
    element: hostElement,
  };

  [@deriving eq]
  type testMountEntry =
    | MountChild(node, node, int)
    | UnmountChild(node, node)
    | RemountChild(node, node, int, int)
    | ChangeText(string, string);

  [@deriving eq]
  type testMountLog = list(testMountEntry);

  let mountLog = ref([]);

  let isDirty = ref(false);
  let markAsStale = () => isDirty := true;

  let insertNode = (~parent: node, ~child: node, ~position: int) => {
    switch (child.element) {
    | _ => mountLog := [MountChild(parent, child, position), ...mountLog^]
    };
    parent;
  };

  let deleteNode = (~parent: node, ~child: node) => {
    switch (child.element) {
    | _ => mountLog := [UnmountChild(parent, child), ...mountLog^]
    };
    parent;
  };

  let moveNode = (~parent: node, ~child: node, ~from: int, ~to_: int) => {
    mountLog := [RemountChild(parent, child, from, to_), ...mountLog^];
    parent;
  };
};

include Brisk_reconciler__Brisk_reconciler_internal.Make(Implementation);

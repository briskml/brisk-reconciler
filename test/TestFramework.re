include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "./snapshots",
      projectDir: ".",
    });
});

BriskReconcilerTest.TestFramework.run(
  Rely.RunConfig.(
    initialize()
    /* causes tests to fail if testOnly or describeOnly are used to prevent
     * accidentally disabling all other tests (as of Rely 2.1.0) */
    |> ciMode(true)
    |> withReporters([
         /* the Default reporter prints terminal output, the jUnit reporter
          * outputs junit xml to the provided filepath, most CI solutions have
          * integration with the junit xml format */
         Default,
         JUnit("./junit.xml"),
       ])
  ),
);

type t = unit => unit;

let noop: t;
let flatten: list(t) => t;
let chain: (t, t) => t;

type t = unit => unit;

let noop: t;
let chain: (t, t) => t;

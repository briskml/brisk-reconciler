type t = unit => unit;

let noop = () => ();

let flatten = (l, ()) => List.iter(f => f(), l);

let chain = (f', f, ()) => {
  f'();
  f();
};

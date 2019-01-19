type hook('a) = ..;

type state('a, 'b);

let createState: unit => state('a, 'b);

type t('a, 'b, 'c, 'd);

let toHooks:
  (state('a, 'b), ~onStateDidChange: unit => unit) => t('a, 'b, 'c, 'c);

let processNext:
  (
    ~default: 'value,
    ~merge: 'value => 'value=?,
    ~toWitness: 'value => hook('value),
    t('value => 'b, unit, 'c, 'value => 'd)
  ) =>
  ('value, t('b, unit, 'c, 'd));

module State: {
  type t('a);
  type hook('a) +=
    pri
    | State(t('a)): hook(t('a));
};

module Reducer: {
  type t('a);
  type hook('a) +=
    pri
    | Reducer(t('a)): hook(t('a));
};

module Ref: {
  type t('a);
  type hook('a) +=
    pri
    | Ref(t('a)): hook(t('a));
};

module Effect: {
  type t('a);
  type lifecycle =
    | Mount
    | Unmount
    | Update;
  type always;
  type onMount;
  type condition('a) =
    | Always: condition(always)
    | OnMount: condition(onMount)
    | If(('a, 'a) => bool, 'a): condition('a);
  type handler = unit => option(unit => unit);
  type hook('a) +=
    pri
    | Effect(t('a)): hook(t('a));
};

let state:
  ('state, t(State.t('state) => 'b, unit, 'c, State.t('state) => 'd)) =>
  ('state, 'state => unit, t('b, unit, 'c, 'd));

let reducer:
  (
    ~initialState: 'state,
    ('action, 'state) => 'state,
    t(Reducer.t('state) => 'b, unit, 'c, Reducer.t('state) => 'd)
  ) =>
  ('state, 'action => unit, t('b, unit, 'c, 'd));

let ref:
  ('state, t(Ref.t('state) => 'b, unit, 'c, Ref.t('state) => 'd)) =>
  ('state, 'state => unit, t('b, unit, 'c, 'd));

let effect:
  (
    Effect.condition('condition),
    Effect.handler,
    t(Effect.t('condition) => 'b, unit, 'c, Effect.t('condition) => 'd)
  ) =>
  t('b, unit, 'c, 'd);

let pendingEffects:
  (~lifecycle: Effect.lifecycle, t('a, 'b)) => list(unit => unit);

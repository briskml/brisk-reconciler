type hook('a) = ..;

type state('a);

type t('remaining, 'result);

type nil;
type empty;
type all('a) = t(nil, 'a);

let empty: unit => t('value, 'value);

let ofState:
  (option(state('b)), ~onStateDidChange: unit => unit) => t('b, 'b);

let toState: all('a) => state('a);

let printState: option(state('a)) => string;

let processNext:
  (
    ~default: 'value,
    ~merge: 'value => 'value=?,
    ~toWitness: 'value => hook('value),
    t('value => 'c, 'd)
  ) =>
  ('value, t('c, 'd));

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
  ('value, t(State.t('value) => 'c, 'd)) =>
  (('value, ('value => 'value) => unit), t('c, 'd));

let reducer:
  (
    ~initialState: 'value,
    ('b, 'value) => 'value,
    t(Reducer.t('value) => 'c, 'd)
  ) =>
  (('value, 'b => unit), t('c, 'd));

let ref:
  ('value, t(ref('value) => 'c, 'd)) =>
  (ref('value), t('c, 'd));

let effect:
  (
    Effect.condition('value),
    unit => option(unit => unit),
    t(Effect.t('value) => 'c, 'd)
  ) =>
  (unit, t('c, 'd));

let pendingEffects:
  (~lifecycle: Effect.lifecycle, option(state('a))) => EffectSequence.t;
let flushPendingStateUpdates: state('a) => state('a);

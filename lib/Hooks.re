type hook('a) = ..;

module HeterogenousList =
  HeterogenousList.Make({
    type t('a) = hook('a);
  });

type state('a) = HeterogenousList.t('a);

type t('remaining, 'processed) = {
  remaining: option(HeterogenousList.t('remaining)),
  processed: HeterogenousList.constructor(('remaining, 'processed)),
  onStateDidChange: unit => unit,
};

let ofState = (remaining, ~onStateDidChange) => {
  remaining,
  processed: HeterogenousList.init,
  onStateDidChange,
};

type nil = HeterogenousList.nil;
type empty = HeterogenousList.t(nil);

type all('a) = t(nil, 'a);

let toState = ({processed}) => HeterogenousList.seal(processed);

let empty: unit => t(_) =
  () => {
    remaining: None,
    processed: HeterogenousList.init,
    onStateDidChange: () => (),
  };

let printState =
  fun
  | Some(_) => "<Some>"
  | None => "<Empty>";

let processNext =
    (
      ~default,
      ~merge=?,
      ~toWitness,
      {remaining, processed, onStateDidChange},
    ) =>
  switch (remaining) {
  | Some(l) =>
    let ({HeterogenousList.value, toWitness}, rest) =
      HeterogenousList.dropFirst(l);
    (
      value,
      {
        remaining: Some(rest),
        processed:
          HeterogenousList.append(
            {
              value:
                switch (merge) {
                | Some(f) => f(value)
                | None => value
                },
              toWitness,
            },
            processed,
          ),
        onStateDidChange,
      },
    );
  | None => (
      default,
      {
        remaining: None,
        processed:
          HeterogenousList.append({value: default, toWitness}, processed),
        onStateDidChange,
      },
    )
  };

module State = {
  type t('a) = {
    currentValue: 'a,
    mutable nextValue: 'a,
  };

  type hook('a) +=
    | State(t('a)): hook(t('a));

  let make: 'a => t('a) =
    initialValue => {currentValue: initialValue, nextValue: initialValue};

  let wrapAsHook = s => State(s);

  let setState = (nextValue, stateContainer) => {
    stateContainer.nextValue = nextValue;
  };

  let flush = ({currentValue, nextValue}) =>
    if (currentValue === nextValue) {
      None;
    } else {
      Some({currentValue: nextValue, nextValue});
    };

  let hook = (initialState, hooks) => {
    let (stateContainer, nextHooks) =
      processNext(~default=make(initialState), ~toWitness=wrapAsHook, hooks);

    let onStateDidChange = hooks.onStateDidChange;

    let setter = updater => {
      setState(updater(stateContainer.nextValue), stateContainer);
      onStateDidChange();
    };

    ((stateContainer.currentValue, setter), nextHooks);
  };
};

module Reducer = {
  type t('a) = {
    currentValue: 'a,
    updates: ref(list('a => 'a)),
  };

  type hook('a) +=
    | Reducer(t('a)): hook(t('a));

  let make: 'a => t('a) =
    initialValue => {currentValue: initialValue, updates: ref([])};

  let flush: t('a) => option(t('a)) =
    reducerState => {
      let {currentValue, updates} = reducerState;
      let nextValue =
        List.fold_right(
          (update, latestValue) => update(latestValue),
          updates^,
          currentValue,
        );
      updates := [];
      if (currentValue === nextValue) {
        None;
      } else {
        Some({currentValue: nextValue, updates});
      };
    };

  let wrapAsHook = s => Reducer(s);

  let enqueueUpdate = (nextUpdate, {updates}) => {
    updates := [nextUpdate, ...updates^];
  };

  let hook = (~initialState, reducer, hooks) => {
    let (stateContainer, hooks) =
      processNext(~default=make(initialState), ~toWitness=wrapAsHook, hooks);

    let onStateDidChange = hooks.onStateDidChange;

    let dispatch = action => {
      enqueueUpdate(prevValue => reducer(action, prevValue), stateContainer);
      onStateDidChange();
    };

    ((stateContainer.currentValue, dispatch), hooks);
  };
};

module Ref = {
  type t('a) = ref('a);
  type hook('a) +=
    | Ref(t('a)): hook(t('a));
  let wrapAsHook = s => Ref(s);

  let hook = (initialState, hooks) => {
    let (internalRef, hooks) =
      processNext(~default=ref(initialState), ~toWitness=wrapAsHook, hooks);

    let setter = nextValue => internalRef := nextValue;

    ((internalRef^, setter), hooks);
  };
};

module Effect = {
  type lifecycle =
    | Mount
    | Unmount
    | Update;
  type always;
  type onMount;
  type condition('a) =
    | Always: condition(always)
    | OnMount: condition(onMount)
    | If(('a, 'a) => bool, 'a): condition('a)
    | OnMountAndIf(('a, 'a) => bool, 'a): condition('a);
  type handler = unit => option(unit => unit);
  type t('a) = {
    condition: condition('a),
    handler: unit => option(unit => unit),
    mutable cleanupHandler: option(unit => unit),
    mutable previousCondition: condition('a),
  };
  type hook('a) +=
    | Effect(t('a)): hook(t('a));

  let wrapAsHook = s => Effect(s);

  let executeOptionalHandler =
    fun
    | Some(f) => {
        f();
      }
    | None => ();

  let get:
    type conditionValue.
      (~lifecycle: lifecycle, t(conditionValue)) => option(unit => unit) =
    (~lifecycle, state) => {
      let {condition, previousCondition, handler, cleanupHandler} = state;
      switch (previousCondition) {
      | Always =>
        Some(
          () => {
            ignore(executeOptionalHandler(cleanupHandler));
            state.cleanupHandler = handler();
          },
        )
      | If(comparator, previousConditionValue) =>
        switch (lifecycle) {
        | Mount
        | Update =>
          let currentConditionValue =
            switch (condition) {
            | If(_, currentConditionValue)
            | OnMountAndIf(_, currentConditionValue) => currentConditionValue
            /* The following cases are unreachable because it's
             * Impossible to create a value of type condition(always)
             * Or condition(onMount) using the If constructor
             */
            | Always
            | OnMount => previousConditionValue
            };
          if (comparator(previousConditionValue, currentConditionValue)) {
            state.previousCondition = condition;
            Some(
              () => {
                ignore(executeOptionalHandler(cleanupHandler));
                state.cleanupHandler = handler();
              },
            );
          } else {
            state.previousCondition = condition;
            None;
          };
        | Unmount => cleanupHandler
        }
      | OnMount =>
        switch (lifecycle) {
        | Mount => Some(() => state.cleanupHandler = handler())
        | Unmount => cleanupHandler
        | _ => None
        }
      | OnMountAndIf(comparator, previousConditionValue) =>
        switch (lifecycle) {
        | Mount => Some(() => state.cleanupHandler = handler())
        | Update =>
          let currentConditionValue =
            switch (condition) {
            | If(_, currentConditionValue)
            | OnMountAndIf(_, currentConditionValue) => currentConditionValue
            /* The following cases are unreachable because it's
             * Impossible to create a value of type condition(always)
             * Or condition(onMount) using the If constructor
             */
            | Always
            | OnMount => previousConditionValue
            };
          if (comparator(previousConditionValue, currentConditionValue)) {
            state.previousCondition = condition;
            Some(
              () => {
                ignore(executeOptionalHandler(cleanupHandler));
                state.cleanupHandler = handler();
              },
            );
          } else {
            state.previousCondition = condition;
            None;
          };
        | Unmount => cleanupHandler
        }
      };
    };

  let hook = (condition, handler, hooks) => {
    let (_, hooks) =
      processNext(
        ~default={
          condition,
          handler,
          cleanupHandler: None,
          previousCondition: condition,
        },
        ~merge=prevEffect => {...prevEffect, condition, handler},
        ~toWitness=wrapAsHook,
        hooks,
      );
    ((), hooks);
  };
};

let state = State.hook;
let reducer = Reducer.hook;
let ref = Ref.hook;
let effect = Effect.hook;

let pendingEffects = (~lifecycle, hooks) =>
  switch (hooks) {
  | Some(hooks) =>
    HeterogenousList.fold(
      (acc, opaqueValue) =>
        switch (opaqueValue) {
        | HeterogenousList.Any(Effect.Effect(state)) =>
          switch (Effect.get(~lifecycle, state)) {
          | Some(effect) => EffectSequence.chain(acc, effect)
          | None => acc
          }
        | _ => acc
        },
      EffectSequence.noop,
      hooks,
    )
  | None => EffectSequence.noop
  };

/*
  * Not implemented yet. Not sure how to type map,
  * and if it even makes sense.
 */
let flushPendingStateUpdates = hooks => {
  let nextHooks =
    HeterogenousList.map(
      {
        f: (type a, hook: hook(a)) => {
          switch (hook) {
          | Reducer.Reducer(s) => (Reducer.flush(s): option(a))
          | State.State(s) => (State.flush(s): option(a))
          | _ => None
          };
        },
      },
      hooks,
    );
  HeterogenousList.compareElementsIdentity(hooks, nextHooks)
    ? hooks : nextHooks;
};
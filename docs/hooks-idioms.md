# Hooks idioms

A short field guide for `Brisk_reconciler.Hooks` — the patterns
that come up in real use, the lifecycle semantics that aren't
obvious from the `.mli`, and the foot-guns to avoid.

Assumes familiarity with React-style hooks. Brisk's hooks are
intentionally close to React's mental model; the differences are
called out below.

## The four hook primitives

| Hook | Use it for |
|---|---|
| `Hooks.state`   | A value the component owns + a setter that updates it. |
| `Hooks.reducer` | Action-based state machines (an `'action -> 'state -> 'state` reducer + a `dispatch` that takes actions). |
| `Hooks.ref`     | A mutable cell with stable identity across renders. **Not** reactive — changes don't trigger re-renders. |
| `Hooks.use_effect` | Side effects tied to mount / unmount / dependency change. |

### Mental model

Each component instance has a per-instance "hooks tape" — an
ordered list of hook states laid down by the `let%hook` /
`let%component` PPX expansion. On every render, the PPX walks the
hooks in declaration order; `Hooks.state` / `Hooks.reducer` /
`Hooks.ref` each pull the next slot off the tape (creating it
with the supplied default on first render, reusing the existing
state on subsequent renders).

The implication: **hook declarations must run in the same order
on every render**. No `Hooks.state` calls inside conditionals,
loops, or after early returns. (Same constraint as React.)

## `use_effect` lifecycle conditions

`Hooks.use_effect condition handler` registers an effect. The
handler returns `Some cleanup_fn` or `None`. When the
handler/cleanup actually fires depends on the `condition`:

| Condition           | Mount  | Re-render (Update) | Unmount |
|---------------------|--------|--------------------|---------|
| `Always`            | run handler | run cleanup then handler | run cleanup |
| `OnMount`           | run handler | — | run cleanup |
| `If (eq, dep)`      | — | if `eq prev_dep dep` then run cleanup + handler | run cleanup |
| `OnMountAndIf (eq, dep)` | run handler | if `eq prev_dep dep` then run cleanup + handler | run cleanup |

Note the unintuitive bit: `If`'s `eq` is treated as
**"changed when comparator returns true"**. The natural reading
of "`eq`" suggests "fires when equal", but the comparator slot
is really a "this matters" predicate. Most callers pass `(<>)`
(not-equal) to get the React-style "fire when dep changed"
behaviour:

```ocaml
let%hook () =
  Hooks.use_effect
    (If ((<>), some_value))
    (fun () ->
      do_thing_with some_value;
      Some (fun () -> cleanup_thing_with some_value))
```

## Common patterns

### Counter (the canonical `state` example)

```ocaml
let%component counter () =
  let%hook count, set_count = Hooks.state 0 in
  <vstack>
    <label text=(Printf.sprintf "Count: %d" count) />
    <button text="+" on_click=(fun () -> set_count (fun n -> n + 1)) />
    <button text="-" on_click=(fun () -> set_count (fun n -> n - 1)) />
  </vstack>
```

The setter takes an `('a -> 'a)` updater, not the new value
directly. This lets the updater see the latest pending value
when several updates batch — the React `setState(prev => prev + 1)`
idiom is the only shape.

### Action-based state (the `reducer` example)

```ocaml
type action = Increment | Decrement | Set of int

let reduce action state =
  match action with
  | Increment -> state + 1
  | Decrement -> state - 1
  | Set n     -> n

let%component counter () =
  let%hook count, dispatch = Hooks.reducer ~initialState:0 reduce in
  <vstack>
    <label text=(Printf.sprintf "Count: %d" count) />
    <button text="+" on_click=(fun () -> dispatch Increment) />
    <button text="-" on_click=(fun () -> dispatch Decrement) />
    <button text="0" on_click=(fun () -> dispatch (Set 0)) />
  </vstack>
```

Use `reducer` when your state transitions are best expressed as
a closed action variant (i.e. when "the kinds of update" are a
fixed enum, not arbitrary `'a -> 'a` functions). For everything
else, `state` is fine.

### Timer / subscription / async — `use_effect` + cleanup

The most common `use_effect` shape: acquire a resource on mount,
release it on unmount. The handler returns `Some cleanup`; brisk
runs `cleanup` when the component unmounts.

```ocaml
let%component clock () =
  let%hook time, set_time = Hooks.state (Unix.time ()) in
  let%hook () =
    Hooks.use_effect OnMount (fun () ->
      let invalidate =
        Timer.schedule_repeating ~interval:1.0 (fun () ->
          set_time (fun _ -> Unix.time ()))
      in
      Some invalidate)
  in
  <label text=(Printf.sprintf "%.0f" time) />
```

The cleanup closure (`invalidate`) is captured in `Some`. On
unmount, brisk invokes it — the timer stops cleanly.

**The setter is stable across renders.** A setter captured at
mount-time (here, inside the `OnMount` handler's closure) keeps
working for the lifetime of the component. The internal state
container is replaced on each flush, but the `updates` ref it
holds is shared by reference into the post-flush container — so
the captured setter still routes through to the live state.

### Conditional sub-trees (mount / unmount lifecycle)

```ocaml
let%component app () =
  let%hook visible, set_visible = Hooks.state true in
  <vstack>
    <button text="Toggle"
            on_click=(fun () -> set_visible (fun v -> not v)) />
    (if visible then <child /> else empty)
  </vstack>
```

`empty` is the "no element" sentinel. When `visible` flips
true → false, `child`'s instance is unmounted (its `use_effect`
cleanups fire). When it flips back, a *fresh* instance mounts —
`Hooks.state` initial values are restored, `use_effect OnMount`
handlers fire again. This is brisk's intended subtree-lifecycle
semantics, distinct from "hide" (which would keep the instance
alive and just toggle visibility on the view).

### Stable callback identity via `Hooks.ref`

If you need a callback whose *identity* must be stable across
renders (because it's compared by reference somewhere — rare in
brisk, common with imperative APIs), the pattern is:

```ocaml
let%hook latest_cb = Hooks.ref ignore in
latest_cb := (fun () -> (* always-fresh-closure body *));

let%hook () =
  Hooks.use_effect OnMount (fun () ->
    register_callback (fun () -> !latest_cb ());
    Some (fun () -> unregister_callback ()))
in
```

The trick: register a stable trampoline closure (`fun () ->
!latest_cb ()`) that dereferences the ref on every call. Update
the ref's contents on every render. Imperative callers see the
same function pointer for the trampoline; the actual body uses
the latest closure.

This is the React `useRef`-for-callbacks pattern. Comes up less
often in brisk than in React because brisk doesn't have an
equivalent of React's `useEffect(..., [deps])` "rebind on every
dep change" rebuild — once a callback is registered via a
`OnMount` use_effect, it stays registered until unmount.

## Gotchas

### Hook ordering must be stable

```ocaml
(* WRONG — conditional state hook. *)
let%component bad () =
  if some_condition then
    let%hook x, _ = Hooks.state 0 in
    ...
  else ...
```

The hook tape advances by ONE on each `let%hook` call. If the
order differs across renders, slot N now corresponds to a
different hook than before — brisk's `processNext` will hand you
a state container of the wrong shape. Same constraint as React.

The fix: always call `Hooks.state` unconditionally, then branch
on the value:

```ocaml
let%component good () =
  let%hook x, set_x = Hooks.state 0 in
  if some_condition then ... use x ...
  else ...
```

### `Hooks.ref` doesn't trigger re-renders

```ocaml
let%hook count_ref = Hooks.ref 0 in
count_ref := !count_ref + 1  (* mutates, but no re-render fires *)
```

`Hooks.ref` is a stable mutable cell. Mutating it has no effect
on the rendered tree until *something else* triggers a re-render
(a `state.set`, a `reducer.dispatch`). Use `ref` for "I need a
mutable scratch slot tied to this component's lifetime", not for
"I have state that should drive the UI". For UI state, use
`Hooks.state` or `Hooks.reducer`.

### `use_effect`'s comparator is "this matters", not "are equal"

Already noted above: `If (comparator, dep)` fires when
`comparator prev_dep dep` returns true. To get the natural
"fire when dep changed" semantics, pass `(<>)` (not-equal), not
`(=)`. Passing `(=)` would fire every time `prev == dep`, which
is not usually what you want.

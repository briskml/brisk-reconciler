include Brisk_reconciler_internal

(* Snake-case public surface.

   The internal module [Brisk_reconciler_internal] retains its
   original (camelCase) identifier set; the renames below provide
   the snake_case names exposed in [brisk_reconciler.mli], with the
   camelCase variants kept as backward-compatible aliases marked
   [@@deprecated] in the [.mli]. *)

let add_stale_tree_handler = addStaleTreeHandler
let list_to_element = listToElement
let native_component = nativeComponent

module Rendered_element = struct
  include RenderedElement

  let flush_pending_updates = flushPendingUpdates
  let execute_host_view_updates = executeHostViewUpdates
  let execute_pending_effects = executePendingEffects
end

(* Shadow [RenderedElement] (brought in by the include above) with
   the new module so [Brisk_reconciler.RenderedElement] resolves to
   [Rendered_element] for backward-compat callers. *)
module RenderedElement = Rendered_element

module Expert = struct
  include Expert
  let native_component = nativeComponent
end

module RemoteAction = Remote_action

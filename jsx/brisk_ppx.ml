module P = Ppxlib.Ast
module ATH = Ppxlib.Ast_helper
module Ast_builder = Ppxlib.Ast_builder.Default

let component_ident ~loc =
  Ast_builder.(pexp_ident ~loc (Located.lident ~loc "brisk-component"))

let component_ident_pattern ~loc =
  Ast_builder.(ppat_var ~loc (Located.mk ~loc "brisk-component"))

let hooks_ident ~loc =
  Ast_builder.(pexp_ident ~loc (Located.lident ~loc "brisk-hooks"))

let hooks_ident_pattern ~loc =
  Ast_builder.(ppat_var ~loc (Located.mk ~loc "brisk-hooks"))

module JSX_ppx = struct
  let rec props_filter_children ~acc = function
    | [] -> List.rev acc
    | (P.Labelled "children", P.([%expr []])) :: tail ->
        props_filter_children ~acc tail
    | (P.Labelled "children", P.([%expr [%e? h] :: [%e? t]] as exp)) :: tail ->
        let loc = exp.P.pexp_loc in
        let prop =
          ( P.Labelled "children",
            P.([%expr Brisk_reconciler.Expert.jsx_list ([%e h] :: [%e t])]) )
        in
        props_filter_children ~acc:(prop :: acc) tail
    | prop :: tail -> props_filter_children ~acc:(prop :: acc) tail

  let props_filter_children props = props_filter_children ~acc:[] props

  let rewrite_apply ~loc ~attributes:attrs props =
    let args = props_filter_children props in
    ATH.Exp.apply ~loc ~attrs (component_ident ~loc) args

  let is_jsx =
    let open Ppxlib.Ast_pattern in
    let jsx_attr = attribute ~name:(string "JSX") ~payload:__ in
    fun attr ->
      parse jsx_attr Ppxlib.Location.none
        ~on_error:(fun _ -> false)
        attr
        (fun _ -> true)

  let filter_jsx = List.filter is_jsx

  let exists_jsx = List.exists is_jsx

  (* Use ppxlib's stable [Longident] (old shape: [Ldot of t * string])
     rather than the stdlib one. On OCaml 5.4, the compiler's
     [Longident.Ldot] grew location wrappers on both arguments —
     [Ldot of t loc * string loc] — and the stdlib constructors no
     longer unify with [Ppxlib.Ast.longident], which ppxlib pins to
     the pre-5.4 shape for PPX portability. Opening
     [Ppxlib.Longident] keeps this function working across OCaml
     versions without conditional compilation. *)
  let rec transform_createElement =
    let open Ppxlib.Longident in
    function
    | Ldot (head, "createElement") -> Ldot (head, "make")
    | Lapply (left, right) -> Lapply (left, transform_createElement right)
    | Lident _ as ident -> ident
    | Ldot _ as ldot -> ldot

  let expr expr =
    match expr.P.pexp_desc with
    | P.Pexp_apply (fn, args) when exists_jsx expr.pexp_attributes ->
        let attributes = filter_jsx expr.pexp_attributes in
        let args = List.map (fun (label, arg) -> (label, arg)) args in
        let loc = expr.P.pexp_loc in
        let fn =
          match fn.P.pexp_desc with
          | P.Pexp_ident { txt; loc } ->
              let txt = transform_createElement txt in
              { fn with pexp_desc = Pexp_ident { txt; loc } }
          | _ -> fn
        in
        P.(
          [%expr
            let [%p component_ident_pattern ~loc] = [%e fn] in
            [%e rewrite_apply ~attributes ~loc:expr.P.pexp_loc args]])
    | _ -> expr
end

module Declaration_ppx = struct
  let attribute_name = function
    | `Component -> "component"
    | `Native -> "nativeComponent"

  (* OCaml 5.4 unified [Pexp_fun] and the old [Pexp_function] into a
     single [Pexp_function (params, type_constraint, body)] where
     [params : function_param list] holds all the parameters of a
     [fun] expression. The compiler doesn't always flatten across
     [fun ... -> fun ... -> body] — nested function literals stay
     nested — so we still recurse through nested [Pexp_function]
     layers, but at each layer we iterate the [params] list rather
     than unwrapping one [Pexp_fun] at a time. *)
  let transform_component_expr ~useDynamicKey ~attribute ~component_name expr =
    (* Find the [()] terminator in a [function_param list]. Returns
       [Some (before, terminator, after)] where [before] are the
       labelled/optional/newtype params preceding [()], and [after]
       is everything following it (including unlabelled / hook-style
       params like [hooks]). Returns [None] if no terminator at
       this layer — caller then recurses into the inner body. *)
    let rec split_at_unit before = function
      | [] -> None
      | ({ P.pparam_desc =
             P.Pparam_val (P.Nolabel, _, [%pat? ()]); _ } as term) :: after ->
          Some (List.rev before, term, after)
      | ({ P.pparam_desc =
             P.Pparam_val ((P.Labelled _ | P.Optional _), _, _); _ } as p)
        :: rest
      | ({ P.pparam_desc = P.Pparam_newtype _; _ } as p) :: rest ->
          split_at_unit (p :: before) rest
      | { P.pparam_loc; _ } :: _ ->
          Location.raise_errorf ~loc:pparam_loc
            "A labelled argument or () was expected"
    in
    let rec process_function_body expr =
      match expr.P.pexp_desc with
      | P.Pexp_newtype (ident, inner) ->
          (* Locally-abstract type binding wrapping the function. The
             mlx-pp preprocessor emits [(type a)] params as nested
             [Pexp_newtype] rather than as flat [Pparam_newtype] inside
             [Pexp_function]'s params list; preserve that shape. *)
          { expr with
            P.pexp_desc =
              P.Pexp_newtype (ident, process_function_body inner) }
      | P.Pexp_function (params, type_constraint, P.Pfunction_body inner) ->
        (match split_at_unit [] params with
         | Some (before, terminator, after) ->
             (* Wrap everything from the terminator inward as
                [component ~key <rest>]. If [after] is non-empty,
                [<rest>] is a function [fun p1 ... pn -> inner];
                otherwise it's just [inner]. *)
             let wrap_arg =
               match after with
               | [] -> inner
               | _ ->
                   { expr with
                     P.pexp_desc =
                       P.Pexp_function
                         (after, type_constraint, P.Pfunction_body inner) }
             in
             let loc = wrap_arg.P.pexp_loc in
             let wrapped =
               [%expr [%e component_ident ~loc] ~key [%e wrap_arg]]
             in
             { expr with
               P.pexp_desc =
                 P.Pexp_function
                   ( before @ [ terminator ]
                   , None
                   , P.Pfunction_body wrapped ) }
         | None ->
             (* No terminator at this layer — recurse into [inner]. *)
             { expr with
               P.pexp_desc =
                 P.Pexp_function
                   ( params
                   , type_constraint
                   , P.Pfunction_body (process_function_body inner) ) })
      | P.Pexp_function (_, _, P.Pfunction_cases _) ->
          Location.raise_errorf ~loc:expr.P.pexp_loc
            "A 'fun' expression was expected, not 'function ... | ...'"
      | _ ->
          Location.raise_errorf ~loc:expr.P.pexp_loc
            "A function with a () parameter was expected"
    in
    let open P in
    let loc = expr.P.pexp_loc in
    let create_component_expr =
      match attribute with
      | `Native -> [%expr Brisk_reconciler.Expert.nativeComponent]
      | `Component -> [%expr Brisk_reconciler.Expert.component]
    in
    [%expr
      let [%p component_ident_pattern ~loc] =
        [%e create_component_expr]
          ~useDynamicKey:[%e Ast_builder.(ebool ~loc useDynamicKey)]
          [%e component_name]
      in
      fun ?(key = Brisk_reconciler.Key.none) ->
        [%e process_function_body expr]]

  let declare_attribute ctx typ =
    let open Ppxlib.Attribute in
    declare (attribute_name typ) ctx
      Ppxlib.Ast_pattern.(
        alt_option (single_expr_payload (pexp_ident (lident __'))) (pstr nil))
      (function
        | Some { txt = "useDynamicKey" } -> true
        | Some { loc } ->
            Location.raise_errorf ~loc "A labelled argument or () was expected"
        | None -> false)

  let expr_attribute_component =
    declare_attribute Ppxlib.Attribute.Context.expression `Component

  let expr_attribute_nativeComponent =
    declare_attribute Ppxlib.Attribute.Context.expression `Native

  let expr_attribute = function
    | `Component -> expr_attribute_component
    | `Native -> expr_attribute_nativeComponent

  let expr unmatched_expr =
    let consume_attr attr =
      Ppxlib.Attribute.consume (expr_attribute attr) unmatched_expr
    in
    let transform ~useDynamicKey attribute expr =
      let loc = expr.P.pexp_loc in
      transform_component_expr ~useDynamicKey ~attribute
        ~component_name:[%expr __LOC__] expr
    in
    match consume_attr `Component with
    | Some (expr, useDynamicKey) -> transform ~useDynamicKey `Component expr
    | None -> (
        match consume_attr `Native with
        | Some (expr, useDynamicKey) -> transform ~useDynamicKey `Native expr
        | None -> unmatched_expr )

  let value_binding_attribute_component =
    declare_attribute Ppxlib.Attribute.Context.value_binding `Component

  let value_binding_attribute_nativeComponent =
    declare_attribute Ppxlib.Attribute.Context.value_binding `Native

  let value_binding_attribute = function
    | `Component -> value_binding_attribute_component
    | `Native -> value_binding_attribute_nativeComponent

  let value_binding unmatched_value_binding =
    let consume_attr attr =
      Ppxlib.Attribute.consume
        (value_binding_attribute attr)
        unmatched_value_binding
    in
    let transform ~useDynamicKey attribute value_binding =
      let value_binding_loc = value_binding.P.pvb_loc in
      Ppxlib.Ast_pattern.(
        parse
          (value_binding ~pat:(ppat_var __) ~expr:__ ~constraint_:drop))
        value_binding_loc value_binding (fun var_pat expr ->
          let component_name =
            ATH.Exp.constant ~loc:expr.P.pexp_loc (ATH.Const.string var_pat)
          in
          let component_pat = value_binding.pvb_pat in
          let transformed_expr =
            transform_component_expr ~useDynamicKey ~attribute ~component_name
              expr
          in
          Ast_builder.(
            value_binding ~pat:component_pat ~loc:value_binding_loc
              ~expr:transformed_expr))
    in
    match consume_attr `Component with
    | Some (value_binding, useDynamicKey) ->
        transform ~useDynamicKey `Component value_binding
    | None -> (
        match consume_attr `Native with
        | Some (value_binding, useDynamicKey) ->
            transform ~useDynamicKey `Native value_binding
        | None -> unmatched_value_binding )

  let register attribute =
    let open Ppxlib in
    Extension.declare (attribute_name attribute)
      Extension.Context.structure_item
      Ast_pattern.(
        pstr
          ( pstr_value __
              ( value_binding ~pat:(ppat_var __) ~expr:__ ~constraint_:drop
                ^:: nil )
          ^:: nil ))
      (fun ~loc ~path recursive pat expr ->
        let component_name =
          ATH.Exp.constant ~loc (ATH.Const.string (path ^ "." ^ pat))
        in
        let transformed_expression =
          transform_component_expr ~useDynamicKey:false ~attribute
            ~component_name expr
        in
        let pat = ATH.Pat.var ~loc (Ast_builder.Default.Located.mk ~loc pat) in
        match recursive with
        | Recursive -> [%stri let rec [%p pat] = [%e transformed_expression]]
        | Nonrecursive -> [%stri let [%p pat] = [%e transformed_expression]])
end

module Hooks_ppx = struct
  open Ppxlib

  (* Grab a list of all the output expressions *)
  let lint_hook_usage =
    object
      inherit [bool] Ast_traverse.fold as super

      method! expression expr _ =
        let open Extension.Context in
        match get_extension expression expr with
        | Some (({ txt = "hook" }, _), _) -> true
        | Some _ | None -> super#expression expr false
    end

  let contains_hook_expression expr = lint_hook_usage#expression expr false

  let expand ~loc expr =
    let expansion =
      match expr.pexp_desc with
      | Pexp_let (Nonrecursive, [ binding ], next_expression) ->
          let wrapped_next_expression =
            if contains_hook_expression expr then
              [%expr [%e next_expression] [%e hooks_ident ~loc]]
            else [%expr [%e next_expression], [%e hooks_ident ~loc]]
          in
          [%expr
            fun [%p hooks_ident_pattern ~loc] ->
              let [%p binding.pvb_pat], [%p hooks_ident_pattern ~loc] =
                [%e binding.pvb_expr] [%e hooks_ident ~loc]
              in
              [%e wrapped_next_expression]]
      | Pexp_let (Recursive, _, _) ->
          Location.raise_errorf ~loc "'let%%hook' may not be recursive"
      | _ -> Location.raise_errorf ~loc "'hook' can only be used with 'let'"
    in
    {
      expansion with
      pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
    }

  let extension =
    Extension.declare "hook" Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc ~path:_ expr -> expand ~loc expr)
end

let declaration_mapper =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      Declaration_ppx.expr e

    method! value_binding binding =
      let binding = super#value_binding binding in
      Declaration_ppx.value_binding binding
  end

let jsx_mapper =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      JSX_ppx.expr e
  end

let () =
  Ppxlib.Driver.register_transformation "component"
    ~impl:declaration_mapper#structure
    ~extensions:
      [
        Declaration_ppx.register `Component;
        Declaration_ppx.register `Native;
        Hooks_ppx.extension;
      ];
  Ppxlib.Driver.register_transformation "JSX" ~impl:jsx_mapper#structure

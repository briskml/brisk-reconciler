module P = Ppxlib.Parsetree
module AT = Ppxlib.Asttypes
module ATH = Ppxlib.Ast_helper

module JSX_ppx = struct
  let rec props_filter_children ~acc =
    P.(
      function
      | [] ->
          List.rev acc
      | (AT.Labelled "children", [%expr []]) :: tail ->
          props_filter_children ~acc tail
      | prop :: tail ->
          props_filter_children ~acc:(prop :: acc) tail)

  let props_filter_children props = props_filter_children ~acc:[] props

  let rewrite_apply ~loc ~attributes:pexp_attributes props =
    let open P in
    let props = props_filter_children props in
    { P.pexp_desc= P.Pexp_apply ([%expr component], props)
    ; pexp_loc= loc
    ; pexp_attributes }

  let is_jsx ({AT.txt}, _) = String.equal txt "JSX"

  let filter_jsx = List.filter is_jsx

  let exists_jsx = List.exists is_jsx

  let rec transform_createElement =
    let open Longident in
    function
    | Ldot (head, "createElement") ->
        Ldot (head, "make")
    | Lapply (left, right) ->
        Lapply (left, transform_createElement right)
    | Lident _ as ident ->
        ident
    | Ldot _ as ldot ->
        ldot

  let expr expr =
    match expr.P.pexp_desc with
    | P.Pexp_apply (fn, args) when exists_jsx expr.pexp_attributes ->
        let attributes = filter_jsx expr.pexp_attributes in
        let args = List.map (fun (label, arg) -> (label, arg)) args in
        let open P in
        let loc = expr.P.pexp_loc in
        let fn =
          match fn.P.pexp_desc with
          | P.Pexp_ident {txt; loc} ->
              let txt = transform_createElement txt in
              {fn with pexp_desc= Pexp_ident {txt; loc}}
          | _ ->
              fn
        in
        [%expr
          let component = [%e fn] in
          [%e rewrite_apply ~attributes ~loc:expr.P.pexp_loc args]]
    | _ ->
        expr
end

module Declaration_ppx = struct
  let func_pattern = Ppxlib.Ast_pattern.(pexp_fun __ __ __ __)

  let match_ pattern ?on_error loc ast_node ~with_ =
    Ppxlib.Ast_pattern.parse pattern ?on_error loc ast_node with_

  let attribute_name = function
    | `Component ->
        "component"
    | `Native ->
        "nativeComponent"

  let transform_component_expr ~useDynamicKey ~attribute ~component_name expr =
    let rec map_component_expression ({P.pexp_loc= loc} as expr) =
      match_ func_pattern loc expr
        ~with_:(fun lbl opt_arg pat child_expression ->
          let make_fun_with_expr ~expr =
            Ppxlib.Ast_builder.Default.pexp_fun ~loc lbl opt_arg pat expr
          in
          let loc = pat.Ppxlib.ppat_loc in
          match (lbl, pat) with
          | (Ppxlib.Labelled _ | Optional _), _ ->
              make_fun_with_expr
                ~expr:(map_component_expression child_expression)
          | Ppxlib.Nolabel, [%pat? ()] ->
              let loc = child_expression.pexp_loc in
              make_fun_with_expr
                ~expr:[%expr component ~key [%e child_expression]]
          | _ ->
              Location.raise_errorf ~loc
                "A labelled argument or () was expected" )
    in
    let open P in
    let loc = expr.P.pexp_loc in
    let create_component_expr =
      match attribute with
      | `Native ->
          [%expr Brisk_jsx_runtime.nativeComponent]
      | `Component ->
          [%expr Brisk_jsx_runtime.component]
    in
    [%expr
      let component =
        [%e create_component_expr]
          ~useDynamicKey:
            [%e Ppxlib.Ast_builder.Default.(ebool ~loc useDynamicKey)]
          [%e component_name]
      in
      fun ?(key = Key.none) -> [%e map_component_expression expr]]

  let declare_attribute ctx typ =
    let open Ppxlib.Attribute in
    declare (attribute_name typ) ctx
      Ppxlib.Ast_pattern.(
        alt_option (single_expr_payload (pexp_ident (lident __'))) (pstr nil))
      (function
        | Some {txt= "useDynamicKey"} ->
            true
        | Some {loc} ->
            Location.raise_errorf ~loc "A labelled argument or () was expected"
        | None ->
            false )

  let expr_attribute_component =
    declare_attribute Ppxlib.Attribute.Context.expression `Component

  let expr_attribute_nativeComponent =
    declare_attribute Ppxlib.Attribute.Context.expression `Native

  let expr_attribute = function
    | `Component ->
        expr_attribute_component
    | `Native ->
        expr_attribute_nativeComponent

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
    | Some (expr, useDynamicKey) ->
        transform ~useDynamicKey `Component expr
    | None -> (
      match consume_attr `Native with
      | Some (expr, useDynamicKey) ->
          transform ~useDynamicKey `Native expr
      | None ->
          unmatched_expr )

  let value_binding_attribute_component =
    declare_attribute Ppxlib.Attribute.Context.value_binding `Component

  let value_binding_attribute_nativeComponent =
    declare_attribute Ppxlib.Attribute.Context.value_binding `Native

  let value_binding_attribute = function
    | `Component ->
        value_binding_attribute_component
    | `Native ->
        value_binding_attribute_nativeComponent

  let value_binding unmatched_value_binding =
    let consume_attr attr =
      Ppxlib.Attribute.consume
        (value_binding_attribute attr)
        unmatched_value_binding
    in
    let transform ~useDynamicKey attribute value_binding =
      let value_binding_loc = value_binding.P.pvb_loc in
      Ppxlib.Ast_pattern.(parse (value_binding ~pat:(ppat_var __) ~expr:__))
        value_binding_loc value_binding (fun var_pat expr ->
          let component_name =
            ATH.Exp.constant ~loc:expr.P.pexp_loc (ATH.Const.string var_pat)
          in
          let component_pat = value_binding.pvb_pat in
          let transformed_expr =
            transform_component_expr ~useDynamicKey ~attribute ~component_name
              expr
          in
          Ppxlib.Ast_builder.Default.(
            value_binding ~pat:component_pat ~loc:value_binding_loc
              ~expr:transformed_expr) )
    in
    match consume_attr `Component with
    | Some (value_binding, useDynamicKey) ->
        transform ~useDynamicKey `Component value_binding
    | None -> (
      match consume_attr `Native with
      | Some (value_binding, useDynamicKey) ->
          transform ~useDynamicKey `Native value_binding
      | None ->
          unmatched_value_binding )
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
    ~impl:declaration_mapper#structure ;
  Ppxlib.Driver.register_transformation "JSX" ~impl:jsx_mapper#structure

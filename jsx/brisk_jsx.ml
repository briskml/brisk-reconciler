module P = Migrate_parsetree.Versions.OCaml_current.Ast.Parsetree
module AT = Migrate_parsetree.Versions.OCaml_current.Ast.Asttypes
module ATM = Migrate_parsetree.Versions.OCaml_current.Ast.Ast_mapper
module ATH = Migrate_parsetree.Versions.OCaml_current.Ast.Ast_helper

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
    { P.pexp_desc=
        P.Pexp_apply ([%expr component], props)
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

  let expr mapper expr =
    match expr.P.pexp_desc with
    | P.Pexp_apply (fn, args) when exists_jsx expr.pexp_attributes ->
        let attributes = filter_jsx expr.pexp_attributes in
        let args =
          List.map
            (fun (label, arg) -> (label, mapper.ATM.expr mapper arg))
            args
        in
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
        ATM.default_mapper.expr mapper expr
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

  let transform_component_expr ~attribute ~component_name mapper expr =
    let rec map_component_expression ~mapper ({P.pexp_loc= loc} as expr) =
      match_ func_pattern loc expr
        ~with_:(fun lbl opt_arg pat child_expression ->
          let make_fun_with_expr ~expr =
            Ppxlib.Ast_builder.Default.pexp_fun ~loc lbl opt_arg pat expr
          in
          let loc = pat.Ppxlib.ppat_loc in
          match (lbl, pat) with
          | (Ppxlib.Labelled _ | Optional _), _ ->
              make_fun_with_expr
                ~expr:(map_component_expression ~mapper child_expression)
          | Ppxlib.Nolabel, [%pat? ()] ->
              let loc = child_expression.pexp_loc in
              make_fun_with_expr
                ~expr:
                  [%expr
                    component ~key
                      [%e ATM.default_mapper.expr mapper child_expression]]
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
      let component = [%e create_component_expr] [%e component_name] in
      fun ?(key = Key.none) -> [%e map_component_expression ~mapper expr]]

  let declare_expr_attribute typ =
    let open Ppxlib.Attribute in
    declare (attribute_name typ) Context.expression Ppxlib.Ast_pattern.__
      ignore

  let expr_attribute_component = declare_expr_attribute `Component

  let expr_attribute_nativeComponent = declare_expr_attribute `Native

  let expr_attribute = function
    | `Component ->
        expr_attribute_component
    | `Native ->
        expr_attribute_nativeComponent

  let expr mapper unmatched_expr =
    let consume_attr attr =
      Ppxlib.Attribute.consume (expr_attribute attr) unmatched_expr
    in
    let transform attribute expr =
      let loc = expr.P.pexp_loc in
      transform_component_expr ~attribute ~component_name:[%expr __LOC__]
        mapper expr
    in
    match consume_attr `Component with
    | Some (expr, ()) ->
        transform `Component expr
    | None -> (
      match consume_attr `Native with
      | Some (expr, ()) ->
          transform `Native expr
      | None ->
          ATM.default_mapper.expr mapper unmatched_expr )

  let declare_value_binding_attribute typ =
    let open Ppxlib.Attribute in
    declare (attribute_name typ) Context.value_binding Ppxlib.Ast_pattern.__
      ignore

  let value_binding_attribute_component =
    declare_value_binding_attribute `Component

  let value_binding_attribute_nativeComponent =
    declare_value_binding_attribute `Native

  let value_binding_attribute = function
    | `Component ->
        value_binding_attribute_component
    | `Native ->
        value_binding_attribute_nativeComponent

  let value_binding mapper unmatched_value_binding =
    let consume_attr attr =
      Ppxlib.Attribute.consume
        (value_binding_attribute attr)
        unmatched_value_binding
    in
    let transform attribute value_binding =
      let value_binding_loc = value_binding.P.pvb_loc in
      Ppxlib.Ast_pattern.(parse (value_binding ~pat:(ppat_var __) ~expr:__))
        value_binding_loc value_binding (fun var_pat expr ->
          let component_name =
            ATH.Exp.constant ~loc:expr.P.pexp_loc (ATH.Const.string var_pat)
          in
          let component_pat = value_binding.pvb_pat in
          let transformed_expr =
            transform_component_expr ~attribute ~component_name mapper expr
          in
          Ppxlib.Ast_builder.Default.(
            value_binding ~pat:component_pat ~loc:value_binding_loc
              ~expr:transformed_expr) )
    in
    match consume_attr `Component with
    | Some (value_binding, ()) ->
        transform `Component value_binding
    | None -> (
      match consume_attr `Native with
      | Some (value_binding, ()) ->
          transform `Native value_binding
      | None ->
          ATM.default_mapper.value_binding mapper unmatched_value_binding )
end

let () =
  let open Migrate_parsetree in
  Driver.register ~name:"component" Versions.ocaml_current
    (fun _config _cookies ->
      let open Declaration_ppx in
      {ATM.default_mapper with expr; value_binding} ) ;
  Driver.register ~name:"JSX" Versions.ocaml_current (fun _config _cookies ->
      let open JSX_ppx in
      {ATM.default_mapper with expr} )

module P = Migrate_parsetree.Versions.OCaml_406.Ast.Parsetree
module AT = Migrate_parsetree.Versions.OCaml_406.Ast.Asttypes
module ATM = Migrate_parsetree.Versions.OCaml_406.Ast.Ast_mapper
module ATH = Migrate_parsetree.Versions.OCaml_406.Ast.Ast_helper

let rec props_filter_key ~key:prev_key ~acc =
  P.(
    function
    | [] ->
        (prev_key, List.rev acc)
    | (AT.Labelled "key", expr) :: tail ->
        props_filter_key ~key:(Some expr) ~acc tail
    | (AT.Labelled "children", [%expr []]) :: tail ->
        props_filter_key ~key:prev_key ~acc tail
    | prop :: tail ->
        props_filter_key ~key:prev_key ~acc:(prop :: acc) tail)

let props_split_key props = props_filter_key ~key:None ~acc:[] props

let rewrite_apply ~loc ~attributes:pexp_attributes ~component_name props =
  let open P in
  let key, props = props_split_key props in
  let component_render =
    { P.pexp_desc=
        P.Pexp_apply ([%expr component.Brisk_jsx_runtime.render], props)
    ; pexp_loc= loc
    ; pexp_attributes }
  in
  match key with
  | None ->
      [%expr
        Brisk_jsx_runtime.element ~debugName:[%e component_name]
          [%e component_render] component]
  | Some key ->
      [%expr
        Brisk_jsx_runtime.element ~key:[%e key] ~debugName:[%e component_name]
          [%e component_render] component]

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
        List.map (fun (label, arg) -> (label, mapper.ATM.expr mapper arg)) args
      in
      let open P in
      let loc = expr.P.pexp_loc in
      let fn, component_name =
        match fn.P.pexp_desc with
        | P.Pexp_ident {txt; loc} ->
            let txt = transform_createElement txt in
            ( {fn with pexp_desc= Pexp_ident {txt; loc}}
            , ATH.Exp.constant ~loc (ATH.Const.string (Longident.last txt)) )
        | _ ->
            (fn, [%expr __LOC__])
      in
      [%expr
        let component = [%e fn] in
        [%e
          rewrite_apply ~attributes ~component_name ~loc:expr.P.pexp_loc args]]
  | _ ->
      ATM.default_mapper.expr mapper expr

  
let is_component ({AT.txt}, _) = String.equal txt "component"

let exists_component = List.exists is_component
let filter_component = List.filter is_component

let rec map_component_declaration_expression ~mapper ({P.pexp_loc = loc; pexp_desc ; pexp_attributes = attrs} as mapped_expression) =
  match pexp_desc with
  | P.Pexp_fun (arg_label, opt_arg, pat, ({pexp_desc= P.Pexp_fun (_, _, _, _)} as next_fn)) ->
      let expr = map_component_declaration_expression ~mapper next_fn in
      let attrs = filter_component attrs in
      ATH.Exp.fun_ ~loc ~attrs arg_label  opt_arg pat expr
  | P.Pexp_fun (_, _, _, _) -> 
      [%expr component([%e mapped_expression])]
  | _ -> ATM.default_mapper.expr mapper mapped_expression



let value_binding mapper binding =
  match binding.P.pvb_pat.ppat_desc, binding.P.pvb_expr.pexp_desc with
  | P.Ppat_var component_name, P.Pexp_fun (_, _, _, _) when exists_component binding.P.pvb_attributes-> 
      let loc = component_name.loc in
      {binding with pvb_expr= 
        [%expr let component = Brisk_jsx_runtime.component [%e ATH.Exp.constant ~loc (ATH.Const.string component_name.txt)] in
        [%e map_component_declaration_expression ~mapper binding.P.pvb_expr] ]
      }
  | _ -> binding


let () =
  Migrate_parsetree.(
    Driver.register ~name:"JSX" Versions.ocaml_406 (fun _config _cookies ->
        {ATM.default_mapper with value_binding} ))

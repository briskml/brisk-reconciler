module P = Migrate_parsetree.Versions.OCaml_406.Ast.Parsetree
module AT = Migrate_parsetree.Versions.OCaml_406.Ast.Asttypes
module ATM = Migrate_parsetree.Versions.OCaml_406.Ast.Ast_mapper

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

let rewrite_apply ~loc ~attributes:pexp_attributes props =
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
      [%expr Brisk_jsx_runtime.element [%e component_render] component]
  | Some key ->
      [%expr
        Brisk_jsx_runtime.element ~key:[%e key] [%e component_render] component]

let is_jsx = (fun ({AT.txt}, _) -> String.equal txt "JSX")
let filter_jsx = List.filter is_jsx
let exists_jsx = List.exists is_jsx

let expr mapper expr =
  match expr.P.pexp_desc with
  | P.Pexp_apply (fn, args) when exists_jsx expr.pexp_attributes ->
      let attributes = filter_jsx expr.pexp_attributes in
      let args =
        List.map (fun (label, arg) -> (label, mapper.ATM.expr mapper arg)) args
      in
      let open P in
      let loc = expr.P.pexp_loc in
      [%expr begin let component = [%e fn] in
      [%e (rewrite_apply ~attributes ~loc:expr.P.pexp_loc args)]
      end
      ]
  | _ ->
      ATM.default_mapper.expr mapper expr

let () =
  Migrate_parsetree.(
    Driver.register ~name:"JSX" Versions.ocaml_406 (fun _config _cookies ->
        {ATM.default_mapper with expr} ))

open! Core
open! Ppxlib
open! Ast_builder.Default

let ppx_namespace = "ppx_template"
let pp_quoted ppf s = Format.fprintf ppf "`%s`" s
let raise_errorf ~loc fmt = Location.raise_errorf ~loc ("%s: " ^^ fmt) ppx_namespace

let constant_nat =
  Extension.declare
    "nat"
    Expression
    Ast_pattern.(
      (* suffix `n` is taken by nativeint *)
      single_expr_payload (pexp_constant (pconst_integer __ none)))
    (fun ~loc ~path:_ int ->
      let int = Int.of_string int in
      if int < 0
      then raise_errorf ~loc "invalid natural number %a." pp_quoted (Int.to_string int)
      else [%expr Ppx_template_runtime.Nat.of_int_unsafe [%e eint ~loc int]])
;;

let () =
  Reserved_namespaces.reserve ppx_namespace;
  Driver.register_transformation ~extensions:[ constant_nat ] ppx_namespace
;;

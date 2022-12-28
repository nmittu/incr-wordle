open Core
open Js_of_ocaml
open Option.Let_syntax

module LocalStorage = struct
  let load_model key ~deserializer =
    let%bind storage = Js.Optdef.to_option Dom_html.window##.localStorage in
    let%bind sexp_js = Js.Opt.to_option (storage##getItem (Js.string key)) in
    let sexp_s = Js.to_string sexp_js in
    let sexp = Sexp.of_string sexp_s in
    deserializer sexp
  ;;

  let save_model key ~model ~serializer =
    let sexp = serializer model in
    let sexp_s = Sexp.to_string sexp in
    let storage = Js.Optdef.to_option Dom_html.window##.localStorage in
    Option.iter storage ~f:(fun storage ->
        storage##setItem (Js.string key) (Js.string sexp_s))
  ;;
end

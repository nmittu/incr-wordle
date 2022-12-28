open! Core
open Incr_dom
open Js_of_ocaml

val view
  :  is_closed:bool
  -> game:Model.NormalGame.t
  -> solution_history:int Model.SolutionMap.t
  -> close_stats:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
  -> Vdom.Node.t

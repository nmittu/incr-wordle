open Core
open Incr_dom
open Js_of_ocaml
module SolutionMap : Map.S with type Key.t = int

val view
  :  is_closed:bool Ui_incr.Incr.t
  -> game:Game_engine.GameEngine.t Ui_incr.Incr.t
  -> solution_history:int SolutionMap.t Ui_incr.Incr.t
  -> close_stats:(Dom_html.mouseEvent Js.t -> unit Ui_effect.t)
  -> Vdom.Node.t Ui_incr.Incr.t

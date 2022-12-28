open! Core
open! Incr_dom
open! Vdom

val gen_keyboard
  :  on_click:(char -> unit Ui_effect.t)
  -> on_delete:(unit -> unit Ui_effect.t)
  -> on_enter:(unit -> unit Ui_effect.t)
  -> Node.t

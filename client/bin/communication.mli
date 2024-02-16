(** The Code to serialize and deserialize the data to send them through the
    wire. Some logic might have sneaked in this module without me noticing. *)

open Js_of_ocaml_lwt.XmlHttpRequest

val push_updates : int -> Code_mirror.Collab.Update.t list -> http_frame Lwt.t
(** Push the update list to the server. The return type is for anyone wanting to
    inspect the code and content of the answer from the server. *)

val recv_updates :
  ((string * Code_mirror.Editor.ChangeSet.t) list -> unit) ->
  (unit -> int) ->
  unit
(** Receive updates, in the form of a callback which will be called each time
    the updates are received. This is where the logic has sneaked in *)

type document = { version : int; document : string; show_id : string }

val getDocument : unit -> document Lwt.t

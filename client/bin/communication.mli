open Js_of_ocaml_lwt.XmlHttpRequest

val push_updates : int -> Code_mirror.Collab.Update.t list -> http_frame Lwt.t

val recv_updates :
  ((string * Code_mirror.Editor.ChangeSet.t) list -> unit) ->
  (unit -> int) ->
  unit

type document = { version : int; document : string; show_id : string }

val getDocument : unit -> document Lwt.t

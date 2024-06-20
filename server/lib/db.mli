val collect_doc : string -> (string * int * string) Lwt.t
val collect_changes : id:string -> version:int -> (int * string) list Lwt.t
val collect_show_doc : string -> (string * int) Lwt.t

val update_doc :
  id:string ->
  changes:(string * string Camlot.Changes.change list) list ->
  from_version:int ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

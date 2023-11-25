let send_updates _version _updates = ()

open Code_mirror

(* Prepare the sending by turning changes into a JSON string *)
let push_updates version fullUpdates =
  let updates =
    List.map
      (fun (u, _) ->
        ( Collab.Update.clientID u,
          Collab.Update.changes u |> Editor.ChangeSet.toJSON |> Brr.Json.encode
          |> Jstr.to_string ))
      fullUpdates
  in
  send_updates version updates

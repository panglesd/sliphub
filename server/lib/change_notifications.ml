open Lwt.Syntax

module Pending = struct
  module Tbl = Hashtbl.Make (String)

  let tbl = Tbl.create 100

  let get id =
    match Tbl.find_opt tbl id with
    | None ->
        let cond = Lwt_condition.create () in
        Tbl.replace tbl id cond;
        cond
    | Some cond -> cond

  let mut = Lwt_mutex.create ()

  let add id version =
    Lwt_mutex.with_lock mut (fun () ->
        let* to_send = Db.Changes.find_above ~id ~version in
        match to_send with
        | _ :: _ as to_send ->
            let to_send =
              List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
            in
            `List to_send |> Yojson.Safe.to_string |> Lwt.return
        | [] ->
            let cond = get id in
            let* () = Lwt_condition.wait ~mutex:mut cond in
            let+ to_send = Db.Changes.find_above ~id ~version in
            let to_send =
              List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
            in
            `List to_send |> Yojson.Safe.to_string)

  let send id =
    let cond = get id in
    Lwt_condition.broadcast cond ()
end

open Common

let receive_changes =
  (* Actually, not a websocket, but a standard post request, despite the [/websocket/push] url... *)
  Dream.post
    Routes.(dream_route receive_changes)
    (fun request ->
      let id = Dream.param request Routes.(parameter receive_changes) in
      let* msg = Dream.body request in
      let client_version, changes = Communication.decode msg in
      let* _document, server_version, _ = Db.collect_doc id in
      Dream.log "Client version is %d, server_version is %d" client_version
        server_version;
      let+ () =
        if server_version <> client_version then Lwt.return ()
        else
          let* _ = Db.update_doc ~id ~changes ~from_version:server_version in
          let () = Pending.send id in
          Lwt.return ()
      in
      Dream.response "")

let send_changes =
  (* Actually, not a websocket, but a standard get request, despite the [/websocket/pull] url... *)
  Dream.get
    Routes.(dream_route send_changes)
    (fun request ->
      let id = Dream.param request Routes.(parameter send_changes) in
      let version =
        int_of_string @@ Dream.param request Routes.(version send_changes)
      in
      let* to_send = Pending.add id version in
      Dream.respond to_send)

let notif_changes = [ send_changes; receive_changes ]

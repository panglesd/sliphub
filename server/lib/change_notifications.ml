open Lwt.Syntax

module Pending = struct
  module Tbl = Hashtbl.Make (String)

  let tbl = Tbl.create 100

  let get id =
    match Tbl.find_opt tbl id with
    | None -> []
    | Some l ->
        Tbl.replace tbl id l;
        l

  let add id ws version closed =
    let l = get id in
    Tbl.replace tbl id ((ws, version, closed) :: l)

  let send id =
    let pendings = get id in
    Dream.log "Sending to %d websockets" (List.length pendings);
    let* () =
      Lwt_list.iter_s
        (fun (_ws, version, _) ->
          Dream.log "This websocket has version %d" !version;
          let* to_send = Db.Changes.find_above ~id ~version:!version in
          Dream.log "and websocket will receive %d changes"
            (List.length to_send);
          let to_send =
            List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
          in
          let to_send = `List to_send |> Yojson.Safe.to_string in
          Dream.log "as of the following string: '%s'" to_send;
          Lwt.return ())
        pendings
    in
    Lwt_list.iter_s
      (fun (ws, version, _) ->
        let* to_send = Db.Changes.find_above ~id ~version:!version in
        let to_send =
          List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
        in
        let to_send = `List to_send |> Yojson.Safe.to_string in
        Dream.send ws to_send)
      pendings
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
          let* () = Pending.send id in
          Lwt.return ()
      in
      Dream.response "")

let send_changes =
  Dream.get
    Routes.(dream_route send_changes)
    (fun request ->
      let id = Dream.param request Routes.(parameter send_changes) in
      Dream.websocket ~close:false (fun websocket ->
          let* recv = Dream.receive websocket in
          match recv with
          | None ->
              Dream.log
                "Some websocket on pull closed before sending their id, \
                 closing it";
              Dream.close_websocket websocket
          | Some i ->
              let version = ref (int_of_string i) in
              let closed = ref false in
              let rec loop () =
                let* recv = Dream.receive websocket in
                match recv with
                | None ->
                    let* () = Dream.close_websocket websocket in
                    closed := true;
                    Lwt.return ()
                | Some i ->
                    version := int_of_string i;
                    loop ()
              in
              Pending.add id websocket version closed;
              (* pending_ws := (version, websocket, closed) :: !pending_ws; *)
              loop ()))

let notif_changes = [ send_changes; receive_changes ]

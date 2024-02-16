open Lwt.Syntax
module Db = Sliphub.Db

let () = Random.self_init ()

module ChangeNotifications = struct
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

  let receive_changes =
    (* Actually, not a websocket, but a standard post request, despite the [/websocket/push] url... *)
    Dream.post "/websocket/push/:document" (fun request ->
        let id = Dream.param request "document" in
        let* msg = Dream.body request in
        let client_version, changes = Sliphub.Communication.decode msg in
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
    Dream.get "/websocket/pull/:document" (fun request ->
        let id = Dream.param request "document" in
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
end

let _ =
  let index_page =
    Dream.get "/" (fun _ ->
        let intro_pres = Assets.(read Intro_pres) in
        let html = Slipshow.convert intro_pres in
        Dream.html html)
  in
  let new_page =
    Dream.get "/new" (fun request ->
        let id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in
        Dream.redirect request ("/" ^ id))
  in
  let js_file =
    Dream.get "/index.js" (fun _ ->
        let response = Dream.response Assets.(read Index_js) in
        Dream.add_header response "charset" "utf-8";
        Dream.add_header response "Content-Type" "text/javascript";
        Lwt.return response)
  in
  let css_file =
    Dream.get "/index.css" (fun _ ->
        let response = Dream.response Assets.(read Index_css) in
        Dream.add_header response "charset" "utf-8";
        Dream.add_header response "Content-Type" "text/css";
        Lwt.return response)
  in
  let send_document =
    Dream.get "/websocket/getDocument/:document" (fun request ->
        let id = Dream.param request "document" in
        Dream.log "queryin document %s" "a";
        Dream.websocket (fun websocket ->
            let* document, version, show_id = Db.collect_doc id in
            let payload =
              Yojson.Safe.to_string
              @@ `List [ `Int version; `String document; `String show_id ]
            in
            Dream.send websocket payload))
  in
  let view_document =
    Dream.get "/view/:document" (fun request ->
        let id = Dream.param request "document" in
        let* document, _version = Db.collect_show_doc id in
        let slipshow = Slipshow.convert document in
        Dream.html slipshow)
  in
  let edit_document =
    Dream.get "/:document" (fun _ -> Dream.html Assets.(read Index_html))
  in
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         index_page;
         new_page;
         js_file;
         css_file;
         ChangeNotifications.receive_changes;
         send_document;
         ChangeNotifications.send_changes;
         view_document;
         edit_document;
       ]

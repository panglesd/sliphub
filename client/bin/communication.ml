open Code_mirror

module Message = struct
  type t = int * Collab.Update.t list

  let to_string (version, fullUpdates) =
    let updates =
      List.map
        (fun u ->
          let a, b =
            ( Collab.Update.clientID u,
              Collab.Update.changes u |> Editor.ChangeSet.toJSON
              |> Brr.Json.encode |> Jstr.to_string )
          in
          `List [ `String a; `String b ])
        fullUpdates
    in
    Yojson.to_string (`List [ `Int version; `List updates ])

  let of_string s =
    match Yojson.Safe.from_string s with
    | `List updates ->
        let updates =
          List.map
            (function
              | `List [ `String id; `String u ] ->
                  ( id,
                    u |> Jstr.of_string |> Brr.Json.decode |> Result.get_ok
                    |> Editor.ChangeSet.fromJSON )
              | _ -> failwith "wrong message format")
            updates
        in
        updates
    | _ -> failwith "wrong message"
end

let uri order =
  let uri = Brr.Window.location Brr.G.window in
  let id =
    let rec tl = function [] -> Jstr.v "aaaa" | [ a ] -> a | _ :: q -> tl q in
    let l = Brr.Uri.path_segments uri |> Result.get_ok in
    tl l
  in
  (* let host = Jstr.to_string @@ Brr.Uri.host uri in *)
  let scheme = Jstr.v "ws" in
  let uri = Brr.Uri.with_uri ~scheme uri |> Result.get_ok in
  let order =
    match order with
    | `GetDoc -> "getDocument"
    | `Push -> "push"
    | `Pull -> "pull"
  in
  let uri =
    Brr.Uri.with_path_segments uri [ Jstr.v "websocket"; Jstr.v order; id ]
  in
  uri |> Result.get_ok

module Comm = struct
  open Brr_io.Websocket

  let send upd =
    let uri = uri `Push in
    let ws = Brr_io.Websocket.create (Brr.Uri.to_jstr uri) in
    let on_open _event = send_string ws (Jstr.v upd) in
    let _open_listener = Brr.Ev.listen Brr.Ev.open' on_open (as_target ws) in
    ws

  let rec recv callback get_version =
    let uri = uri `Pull in
    let ws = Brr_io.Websocket.create (Brr.Uri.to_jstr uri) in
    let on_message event =
      let raw_data : Jstr.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
      let data = Message.of_string (Jstr.to_string raw_data) in
      (* Format.printf "Here is what we received: '%s'%!\n" *)
      (*   (Jstr.to_string raw_data); *)
      callback data;
      let version = get_version () in
      send_string ws (Jstr.v @@ string_of_int version)
    in
    let _message_listener =
      Brr.Ev.listen Brr_io.Message.Ev.message on_message (as_target ws)
    in
    let on_open _event =
      let version = get_version () in
      send_string ws (Jstr.v @@ string_of_int version)
    in
    let _open_listener = Brr.Ev.listen Brr.Ev.open' on_open (as_target ws) in
    let on_close _event = recv callback get_version in
    let _close_listener =
      Brr.Ev.listen Brr_io.Websocket.Ev.close on_close (as_target ws)
    in
    ()
end

let push_updates (version : int) fullUpdates =
  let msg = Message.to_string (version, fullUpdates) in
  Comm.send msg

let recv_updates callback get_version = Comm.recv callback get_version

let getDocument () =
  let open Brr_io.Websocket in
  let uri = uri `GetDoc in
  let ws = create (Brr.Uri.to_jstr uri) in
  let document_of_string s =
    let json = Yojson.Safe.from_string s in
    match json with
    | `List [ `Int version; `String document ] -> (version, document)
    | _ -> failwith "wrong doc received"
  in
  let promise, resolve = Lwt.wait () in
  let on_message event =
    let raw_data : Jstr.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
    let data = document_of_string (Jstr.to_string raw_data) in
    Lwt.wakeup_later resolve data;
    close ws
  in
  let _message_listener =
    Brr.Ev.listen Brr_io.Message.Ev.message on_message (as_target ws)
  in
  promise

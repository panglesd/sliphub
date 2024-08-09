open Code_mirror
open Common

module Message = struct
  (* type t = int * Collab.Update.t list *)

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
  let route_segment =
    let route, version =
      match order with
      | `GetDoc -> (Routes.send_document, None)
      | `Push -> (Routes.receive_changes, None)
      | `PushAndReceive version ->
          (Routes.push_and_receive_changes, Some (string_of_int version))
      | `Pull version -> (Routes.send_changes, Some (string_of_int version))
    in
    let route_segment =
      Routes.route_segments ?version route (Jstr.to_string id)
    in
    List.map Jstr.v route_segment
  in
  let uri = Brr.Uri.with_path_segments uri route_segment in
  uri |> Result.get_ok

module Comm = struct
  let send upd =
    let uri = uri `Push in
    Js_of_ocaml_lwt.XmlHttpRequest.perform_raw_url ~contents:(`String upd)
      (Brr.Uri.to_jstr uri |> Jstr.to_string)

  let send_atomic upd version =
    let open Lwt.Syntax in
    let uri = uri (`PushAndReceive version) in
    let* raw_data =
      Js_of_ocaml_lwt.XmlHttpRequest.perform_raw_url ~contents:(`String upd)
        (Brr.Uri.to_jstr uri |> Jstr.to_string)
    in
    if raw_data.code = 200 then
      let raw_data = raw_data.content in
      let data = Message.of_string raw_data in
      Lwt.return data
    else Lwt.return []

  let recv_atomic version =
    let rec do_ () =
      let open Lwt.Syntax in
      let uri = uri (`Pull version) in
      let* raw_data =
        Js_of_ocaml_lwt.XmlHttpRequest.get
          (Brr.Uri.to_jstr uri |> Jstr.to_string)
      in
      if raw_data.code = 200 then
        let raw_data = raw_data.content in
        let data = Message.of_string raw_data in
        Lwt.return data
      else do_ ()
    in
    do_ ()

  let recv callback get_version =
    let open Lwt.Syntax in
    let rec do_ () =
      let version = get_version () in
      let uri = uri (`Pull version) in
      let raw_data =
        let+ raw_data =
          Js_of_ocaml_lwt.XmlHttpRequest.get
            (Brr.Uri.to_jstr uri |> Jstr.to_string)
        in
        Some raw_data
      in
      let timeout =
        let+ () = Js_of_ocaml_lwt.Lwt_js.sleep 30. in
        None
      in
      let* raw_data = Lwt.pick [ raw_data; timeout ] in
      match raw_data with
      | None ->
          Brr.Console.(log [ "Lost connection, retrying"; Brr.Uri.to_jstr uri ]);
          do_ ()
      | Some raw_data ->
          if raw_data.code = 200 then
            let raw_data = raw_data.content in
            let data = Message.of_string raw_data in
            let () = callback data in
            do_ ()
          else do_ ()
    in
    let _ : unit Lwt.t = do_ () in
    ()
end

let push_updates (version : int) fullUpdates =
  let msg = Message.to_string (version, fullUpdates) in
  Comm.send msg

let push_updates_atomic (version : int) fullUpdates =
  let msg = Message.to_string (version, fullUpdates) in
  Comm.send_atomic msg version

type updates = (string * Code_mirror.Editor.ChangeSet.t) list

let recv_updates_atomic version = Comm.recv_atomic version
let recv_updates callback get_version = Comm.recv callback get_version

type document = { version : int; document : string; show_id : string }

let getDocument () =
  let uri = uri `GetDoc in
  let open Lwt.Syntax in
  let+ { content = raw_data; _ } =
    Js_of_ocaml_lwt.XmlHttpRequest.get (Brr.Uri.to_jstr uri |> Jstr.to_string)
  in
  let document_of_string s =
    let json = Yojson.Safe.from_string s in
    match json with
    | `List [ `Int version; `String document; `String show_id ] ->
        { version; document; show_id }
    | _ -> failwith "wrong doc received"
  in
  let data = document_of_string raw_data in
  data

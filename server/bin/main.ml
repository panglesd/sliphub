module Db = struct
  open Petrol
  open Petrol.Sqlite3
  open Lwt.Syntax

  (* define a new schema *)
  let schema = StaticSchema.init ()

  let conn =
    let open Lwt_result.Syntax in
    (* ... *)
    let* conn = Caqti_lwt.connect (Uri.of_string ("sqlite3://:" ^ "db.db")) in
    let+ () = StaticSchema.initialise schema conn in
    conn
  (* ... *)

  let conn =
    let+ conn in
    match conn with Ok c -> c | Error _ -> failwith "could not connect to db"

  module Document = struct
    (* declare a table *)
    let document_table, Expr.[ id_field; content_field; version_field ] =
      StaticSchema.declare_table schema ~name:"documents"
        Schema.
          [
            field "id" ~ty:Type.text;
            field "content" ~ty:Type.text;
            field "version" ~ty:Type.int;
          ]

    let insert ~id ~content ~version =
      let* db = conn in
      Query.insert ~table:document_table
        ~values:
          Expr.
            [
              id_field := s id;
              content_field := s content;
              version_field := i version;
            ]
      |> Request.make_zero |> Petrol.exec db

    let rec find_opt id =
      let* db = conn in
      Query.select Expr.[ content_field; version_field ] ~from:document_table
      |> Query.where Expr.(id_field = s id)
      |> Request.make_zero_or_one |> Petrol.find_opt db

    let update ~id ~content ~version =
      let* db = conn in
      Query.update ~table:document_table
        ~set:Expr.[ content_field := s content; version_field := i version ]
      |> Request.make_zero |> Petrol.exec db
  end

  module Changes = struct
    (* declare a table *)
    let modification_table, Expr.[ doc_id; modif_number; modif_field ] =
      StaticSchema.declare_table schema ~name:"modifs"
        Schema.
          [
            field "id" ~ty:Type.text;
            field "index" ~ty:Type.int;
            field "change" ~ty:Type.text;
          ]

    let insert ~id ~version ~modif:(mid, change) =
      let* db = conn in
      let change =
        Camlot.Changes.ChangeSet.to_JSON change |> Yojson.Safe.to_string
      in
      let blob =
        `List [ `String mid; `String change ] |> Yojson.Safe.to_string
      in
      Query.insert ~table:modification_table
        ~values:
          Expr.
            [ doc_id := s id; modif_number := i version; modif_field := s blob ]
      |> Request.make_zero |> Petrol.exec db

    let rec find_above ~id ~version =
      let* db = conn in
      let+ result =
        Query.select Expr.[ modif_number; modif_field ] ~from:modification_table
        |> Query.where Expr.(doc_id = s id && modif_number >= i version)
        |> Query.order_by modif_number
        |> Request.make_many |> Petrol.collect_list db
      in
      match result with
      | Error _ -> failwith "no result for find_above"
      | Ok l -> List.map (fun (version, (changes, ())) -> (version, changes)) l
  end

  let init_doc ~id =
    let content = "# An empty slipshow presentation" in
    Document.insert ~id ~content ~version:0

  let rec collect_doc id =
    let* db = conn in
    let* res = Document.find_opt id in
    match res with
    | Ok None ->
        let* _ = init_doc ~id in
        collect_doc id
    | Ok (Some (content, (version, ()))) -> Lwt.return (content, version)
    | Error _ -> failwith "collect"

  let update_doc ~id ~changes ~from_version =
    let* db = conn in
    (* For the side effect that it inits the doc if needed *)
    let* content, version = collect_doc id in
    if version <> from_version then Lwt.return_ok ()
    else
      let content =
        List.fold_left
          (fun doc (_id, change) -> Camlot.Changes.ChangeSet.apply change doc)
          content changes
      in
      let* () =
        Lwt_list.iteri_p
          (fun i modif ->
            let+ res = Changes.insert ~id ~version:(version + i + 1) ~modif in
            match res with
            | Ok () -> ()
            | Error _ -> failwith "failing to add change")
          changes
      in
      Document.update ~id ~content ~version
end

open Lwt.Syntax

(* let document = ref "abcde" *)
(* let server_version = ref 0 *)
(* let server_changes = ref [] *)

let take n l =
  let rec loop acc n l =
    match (n, l) with
    | 0, _ -> acc
    | n, a :: q -> loop (a :: acc) (n - 1) q
    | _ -> assert false
  in
  loop [] n l

module Pending = struct
  module Tbl = Hashtbl.Make (String)

  let tbl = Tbl.create 100

  let get id =
    match Tbl.find_opt tbl id with
    | None -> []
    | Some l ->
        let l = List.filter (fun (_, _, closed) -> not !closed) l in
        Tbl.replace tbl id l;
        l

  let add id ws version closed =
    let l = get id in
    Tbl.replace tbl id ((ws, version, closed) :: l)

  let send id =
    let pendings = get id in
    Lwt_list.iter_p
      (fun (ws, version, _) ->
        let* to_send = Db.Changes.find_above ~id ~version:!version in
        let to_send =
          List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
        in
        let to_send = `List to_send |> Yojson.Safe.to_string in
        Dream.send ws to_send)
      pendings
end

(* let pending_ws = _ *)

(* let send_to_pending id = *)
(*   let new_pending = *)
(*     List.filter *)
(*       (fun (version, websocket, closed) -> *)
(*         if !closed then false *)
(*         else if !version <= !version then true *)
(*         else *)
(*           let data = take (!version - !version) _ in *)
(*           let json = *)
(*             `List *)
(*               (List.map *)
(*                  (fun (id, changes) -> *)
(*                    `List *)
(*                      [ *)
(*                        `String id; *)
(*                        `String *)
(*                          (Yojson.Safe.to_string *)
(*                          @@ Camlot.Changes.ChangeSet.to_JSON changes); *)
(*                      ]) *)
(*                  data) *)
(*           in *)
(*           let to_send = Yojson.Safe.to_string json in *)
(*           let _ = Dream.send websocket to_send in *)
(*           true) *)
(*       !pending_ws *)
(*   in *)
(*   pending_ws := new_pending *)

let () =
  Dream.run
  (* ~interface:"0.0.0.0" *)
  (* @@ Dream.logger *)
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html Data_files.(read Index_html));
         Dream.get "/index.js" (fun _ ->
             let response = Dream.response Data_files.(read Index_js) in
             Dream.add_header response "charset" "utf-8";
             Dream.add_header response "Content-Type" "text/javascript";
             Lwt.return response);
         Dream.get "/index.css" (fun _ ->
             let response = Dream.response Data_files.(read Index_css) in
             Dream.add_header response "charset" "utf-8";
             Dream.add_header response "Content-Type" "text/css";
             Lwt.return response);
         Dream.get "/websocket/push/:document" (fun request ->
             let id = Dream.param request "document" in
             Dream.websocket (fun websocket ->
                 let* recv = Dream.receive websocket in
                 match recv with
                 | Some msg ->
                     let client_version, changes =
                       Sliphub.Communication.decode msg
                     in
                     let* document, server_version = Db.collect_doc id in
                     if server_version <> client_version then Lwt.return ()
                     else
                       let* _ =
                         Db.update_doc ~id ~changes ~from_version:server_version
                       in
                       (* server_version := !server_version + List.length changes; *)
                       (* let new_doc = *)
                       (*   List.fold_left *)
                       (*     (fun doc (_id, change) -> *)
                       (*       Camlot.Changes.ChangeSet.apply change doc) *)
                       (*     !document changes *)
                       (* in *)
                       (* server_changes := List.rev_append changes !server_changes; *)
                       (* document := new_doc; *)
                       (* Dream.log *)
                       (* "new version is %d and new doc is %s, pending_ws has \ *)
                          (*    size %d" *)
                       (*   !server_version !document (List.length !pending_ws); *)
                       let _ = Pending.send id in
                       Lwt.return ()
                 | None ->
                     Lwt.return @@ Dream.log "Received an incomplete message"));
         Dream.get "/websocket/getDocument/:document" (fun request ->
             let id = Dream.param request "document" in
             Dream.websocket (fun websocket ->
                 let* document, version = Db.collect_doc id in
                 let payload =
                   Yojson.Safe.to_string
                   @@ `List [ `Int version; `String document ]
                 in
                 Dream.send websocket payload));
         Dream.get "/websocket/pull/:document" (fun request ->
             let id = Dream.param request "document" in
             Dream.websocket ~close:false (fun websocket ->
                 let* recv = Dream.receive websocket in
                 match recv with
                 | None -> failwith "closed too early"
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
                     loop ()));
       ]

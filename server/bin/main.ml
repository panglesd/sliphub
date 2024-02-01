module Db = struct
  open Petrol
  open Petrol.Sqlite3
  open Lwt.Syntax

  (* schema version 1.0.0 *)
  let version = VersionedSchema.version [ 0; 0; 1 ]

  (* define a schema *)
  let schema = VersionedSchema.init version ~name:"sliphub"

  let conn =
    let open Lwt_result.Syntax in
    (* ... *)
    let* conn =
      Caqti_lwt.connect (Uri.of_string ("sqlite3:///tmp/" ^ "db.db"))
    in
    let+ () = VersionedSchema.initialise schema conn in
    conn
  (* ... *)

  let conn =
    let+ conn = conn in
    match conn with
    | Ok c -> c
    | Error e ->
        let s =
          match e with
          | #Caqti_error.t as e -> Format.asprintf "error: %a" Caqti_error.pp e
          | `Newer_version_than_supported _e ->
              Format.asprintf "Error: Newer version than supported: %a"
                Format.(pp_print_list pp_print_int)
                []
        in
        Dream.log "%s" s;
        Dream.log "%s" (String.map (function ' ' -> '\n' | c -> c) s);
        failwith s

  module Document = struct
    (* declare a table *)
    let ( document_table,
          Expr.[ id_field; show_id_field; content_field; version_field ] ) =
      VersionedSchema.declare_table schema ~name:"documents"
        Schema.
          [
            field "id" ~ty:Type.text;
            field "show_id" ~ty:Type.text;
            field "content" ~ty:Type.text;
            field "version" ~ty:Type.int;
          ]

    let insert ~id ~content ~version ~show_id =
      let* db = conn in
      Query.insert ~table:document_table
        ~values:
          Expr.
            [
              id_field := s id;
              show_id_field := s show_id;
              content_field := s content;
              version_field := i version;
            ]
      |> Request.make_zero |> Petrol.exec db

    let find_opt id =
      let* db = conn in
      Query.select
        Expr.[ content_field; version_field; show_id_field ]
        ~from:document_table
      |> Query.where Expr.(id_field = s id)
      |> Request.make_zero_or_one |> Petrol.find_opt db

    let find_from_show_id_opt id =
      let* db = conn in
      Query.select Expr.[ content_field; version_field ] ~from:document_table
      |> Query.where Expr.(show_id_field = s id)
      |> Request.make_zero_or_one |> Petrol.find_opt db

    let update ~id ~content ~version =
      let* db = conn in
      Query.update ~table:document_table
        ~set:Expr.[ content_field := s content; version_field := i version ]
      |> Query.where Expr.(id_field = s id)
      |> Request.make_zero |> Petrol.exec db
  end

  module Changes = struct
    (* declare a table *)
    let modification_table, Expr.[ doc_id; modif_number; modif_field ] =
      VersionedSchema.declare_table schema ~name:"modifs"
        Schema.
          [
            field "id" ~ty:Type.text;
            field "idx" ~ty:Type.int;
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

    let find_above ~id ~version =
      let* db = conn in
      let+ result =
        Query.select Expr.[ modif_number; modif_field ] ~from:modification_table
        |> Query.where Expr.(doc_id = s id && modif_number > i version)
        |> Query.order_by modif_number
        |> Request.make_many |> Petrol.collect_list db
      in
      match result with
      | Error _ -> failwith "no result for find_above"
      | Ok l -> List.map (fun (version, (changes, ())) -> (version, changes)) l
  end

  let init_doc ~id =
    let content = "# An empty slipshow presentation" in
    let show_id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in
    Document.insert ~id ~content ~version:0 ~show_id

  let rec collect_doc id =
    let* res = Document.find_opt id in
    match res with
    | Ok None ->
        let* _ = init_doc ~id in
        collect_doc id
    | Ok (Some (content, (version, (show_id, ())))) ->
        Lwt.return (content, version, show_id)
    | Error _ -> failwith "collect"

  let rec collect_show_doc id =
    let* res = Document.find_from_show_id_opt id in
    match res with
    | Ok None ->
        let* _ = init_doc ~id in
        collect_show_doc id
    | Ok (Some (content, (version, ()))) -> Lwt.return (content, version)
    | Error _ -> failwith "collect"

  let update_doc ~id ~changes ~from_version =
    (* For the side effect that it inits the doc if needed *)
    let* content, version, _show_id = collect_doc id in
    if version <> from_version then Lwt.return_ok ()
    else
      let content =
        let content = Sliphub.Converter.utf8_to_utf16 content in
        List.fold_left
          (fun doc (_id, change) ->
            let change =
              List.map
                (function
                  | Camlot.Changes.Keep k -> Camlot.Changes.Keep (2 * k)
                  | Replace (i, (l_s, s)) ->
                      Replace
                        (2 * i, (2 * l_s, Sliphub.Converter.utf8_to_utf16 s)))
                change
            in
            let res =
              Camlot.Changes.ChangeSet.apply (* ~is_utf16:true  *) change doc
            in
            res)
          content changes
      in
      let content = Sliphub.Converter.utf16_to_utf8 content in
      Dream.log "Document is '%s'" content;
      let* () =
        Lwt_list.iteri_s
          (fun i modif ->
            let+ res = Changes.insert ~id ~version:(version + i + 1) ~modif in
            match res with
            | Ok () -> ()
            | Error _ -> failwith "failing to add change")
          changes
      in
      Document.update ~id ~content ~version:(version + List.length changes)
end

open Lwt.Syntax

module Pending = struct
  module Tbl = Hashtbl.Make (String)

  let tbl = Tbl.create 100

  let get id =
    match Tbl.find_opt tbl id with
    | None -> []
    | Some l ->
        (* let l = List.filter (fun (_, _, closed) -> not !closed) l in *)
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
        (* Dream.log "One websocket receive %d changes" (List.length to_send); *)
        let to_send =
          List.map (fun (_v, blob) -> Yojson.Safe.from_string blob) to_send
        in
        let to_send = `List to_send |> Yojson.Safe.to_string in
        Dream.send ws to_send)
      pendings
end

let () = Random.self_init ()

let _ =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             let intro_pres = Assets.(read Intro_pres) in
             let html = Slipshow.convert intro_pres in
             Dream.html html);
         Dream.get "/new" (fun request ->
             let id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in
             Dream.redirect request ("/" ^ id));
         Dream.get "/index.js" (fun _ ->
             let response = Dream.response Assets.(read Index_js) in
             Dream.add_header response "charset" "utf-8";
             Dream.add_header response "Content-Type" "text/javascript";
             Lwt.return response);
         Dream.get "/index.css" (fun _ ->
             let response = Dream.response Assets.(read Index_css) in
             Dream.add_header response "charset" "utf-8";
             Dream.add_header response "Content-Type" "text/css";
             Lwt.return response);
         Dream.post "/websocket/push/:document" (fun request ->
             let id = Dream.param request "document" in
             let* msg = Dream.body request in
             let client_version, changes =
               try Sliphub.Communication.decode msg
               with exn ->
                 Dream.log "Exn %s with msg %s" (Printexc.to_string exn) msg;
                 raise exn
             in
             let* _document, server_version, _ = Db.collect_doc id in
             Dream.log "Client version is %d, server_version is %d"
               client_version server_version;
             let+ () =
               if server_version <> client_version then Lwt.return ()
               else
                 let* _ =
                   Db.update_doc ~id ~changes ~from_version:server_version
                 in
                 let _ = Pending.send id in
                 Lwt.return ()
             in
             Dream.response "");
         Dream.get "/websocket/getDocument/:document" (fun request ->
             let id = Dream.param request "document" in
             Dream.log "queryin document %s" "a";
             Dream.websocket (fun websocket ->
                 let* document, version, show_id = Db.collect_doc id in
                 let payload =
                   Yojson.Safe.to_string
                   @@ `List [ `Int version; `String document; `String show_id ]
                 in
                 Dream.send websocket payload));
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
                     loop ()));
         Dream.get "/view/:document" (fun request ->
             let id = Dream.param request "document" in
             let* document, _version = Db.collect_show_doc id in
             let slipshow = Slipshow.convert document in
             Dream.html slipshow);
         Dream.get "/:document" (fun _ -> Dream.html Assets.(read Index_html));
       ]

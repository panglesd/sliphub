module DB_sqlite = struct
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
    let* conn = Caqti_lwt_unix.connect (Uri.of_string "sqlite3:///tmp/db.db") in
    let+ () = VersionedSchema.initialise schema conn in
    conn

  (* let mut = Lwt_mutex.create () *)

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

    let find_all () =
      let* db = conn in
      let+ res =
        Query.select
          Expr.[ id_field; show_id_field; content_field; version_field ]
          ~from:document_table
        |> Query.where
             Expr.(content_field <> s "# An empty slipshow presentation")
        |> Request.make_many |> Petrol.collect_list db
      in
      match res with
      | Error _ -> assert false
      | Ok res ->
          List.map
            (fun (id, (show_id, (content, (version, ())))) ->
              (id, show_id, content, version))
            res

    (* let insert ~id ~content ~version ~show_id = *)
    (*   let* db = conn in *)
    (*   Query.insert ~table:document_table *)
    (*     ~values: *)
    (*       Expr. *)
    (*         [ *)
    (*           id_field := s id; *)
    (*           show_id_field := s show_id; *)
    (*           content_field := s content; *)
    (*           version_field := i version; *)
    (*         ] *)
    (*   |> Request.make_zero |> Petrol.exec db *)

    (* let find_opt id = *)
    (*   let* db = conn in *)
    (*   Query.select *)
    (*     Expr.[ content_field; version_field; show_id_field ] *)
    (*     ~from:document_table *)
    (*   |> Query.where Expr.(id_field = s id) *)
    (*   |> Request.make_zero_or_one |> Petrol.find_opt db *)

    (* let find_from_show_id_opt id = *)
    (*   let* db = conn in *)
    (*   Query.select Expr.[ content_field; version_field ] ~from:document_table *)
    (*   |> Query.where Expr.(show_id_field = s id) *)
    (*   |> Request.make_zero_or_one |> Petrol.find_opt db *)

    (* let update ~id ~content ~version = *)
    (*   let* db = conn in *)
    (*   Query.update ~table:document_table *)
    (*     ~set:Expr.[ content_field := s content; version_field := i version ] *)
    (*   |> Query.where Expr.(id_field = s id) *)
    (*   |> Request.make_zero |> Petrol.exec db *)
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

    let find_all () =
      let* db = conn in
      let+ res =
        Query.select
          Expr.[ doc_id; modif_number; modif_field ]
          ~from:modification_table
        |> Request.make_many |> Petrol.collect_list db
      in
      match res with
      | Error _ -> assert false
      | Ok res ->
          List.map (fun (id, (idx, (change, ()))) -> (id, idx, change)) res

    (* let insert ~id ~version ~modif:(mid, change) = *)
    (*   let* db = conn in *)
    (*   let change = *)
    (*     Camlot.Changes.ChangeSet.to_JSON change |> Yojson.Safe.to_string *)
    (*   in *)
    (*   let blob = *)
    (*     `List [ `String mid; `String change ] |> Yojson.Safe.to_string *)
    (*   in *)
    (*   Query.insert ~table:modification_table *)
    (*     ~values: *)
    (*       Expr. *)
    (*         [ doc_id := s id; modif_number := i version; modif_field := s blob ] *)
    (*   |> Request.make_zero |> Petrol.exec db *)

    (* let find_above ~id ~version = *)
    (*   let* db = conn in *)
    (*   let+ result = *)
    (*     Query.select Expr.[ modif_number; modif_field ] ~from:modification_table *)
    (*     |> Query.where Expr.(doc_id = s id && modif_number > i version) *)
    (*     |> Query.order_by modif_number *)
    (*     |> Request.make_many |> Petrol.collect_list db *)
    (*   in *)
    (*   match result with *)
    (*   | Error _ -> failwith "no result for find_above" *)
    (*   | Ok l -> List.map (fun (version, (changes, ())) -> (version, changes)) l *)
  end
end

module DB_PG = struct
  open Petrol
  open Petrol.Postgres
  open Lwt.Syntax

  (* schema version 1.0.0 *)
  let version = VersionedSchema.version [ 0; 0; 1 ]

  (* define a schema *)
  let schema = VersionedSchema.init version ~name:"sliphub"

  let conn =
    let open Lwt_result.Syntax in
    (* ... *)
    let* conn =
      Caqti_lwt_unix.connect
        (Uri.of_string "postgresql://sliphub:sliphub@localhost/sliphubDb")
    in
    let+ () = VersionedSchema.initialise schema conn in
    conn

  (* let mut = Lwt_mutex.create () *)

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

    let insert_all all =
      let* db = conn in
      let total = List.length all in
      let insert i (id, show_id, content, version) =
        Format.printf "Document %d of %d\n%!" i total;
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
      in
      Lwt_list.mapi_s insert all

    (* let insert ~id ~content ~version ~show_id = *)
    (*   let* db = conn in *)
    (*   Query.insert ~table:document_table *)
    (*     ~values: *)
    (*       Expr. *)
    (*         [ *)
    (*           id_field := s id; *)
    (*           show_id_field := s show_id; *)
    (*           content_field := s content; *)
    (*           version_field := i version; *)
    (*         ] *)
    (*   |> Request.make_zero |> Petrol.exec db *)

    (* let find_opt id = *)
    (*   let* db = conn in *)
    (*   Query.select *)
    (*     Expr.[ content_field; version_field; show_id_field ] *)
    (*     ~from:document_table *)
    (*   |> Query.where Expr.(id_field = s id) *)
    (*   |> Request.make_zero_or_one |> Petrol.find_opt db *)

    (* let find_from_show_id_opt id = *)
    (*   let* db = conn in *)
    (*   Query.select Expr.[ content_field; version_field ] ~from:document_table *)
    (*   |> Query.where Expr.(show_id_field = s id) *)
    (*   |> Request.make_zero_or_one |> Petrol.find_opt db *)

    (* let update ~id ~content ~version = *)
    (*   let* db = conn in *)
    (*   Query.update ~table:document_table *)
    (*     ~set:Expr.[ content_field := s content; version_field := i version ] *)
    (*   |> Query.where Expr.(id_field = s id) *)
    (*   |> Request.make_zero |> Petrol.exec db *)
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

    let insert_all all =
      let* db = conn in
      let total = List.length all in
      let insert i (id, idx, change) =
        Format.printf "Change %d of %d\n%!" i total;
        Query.insert ~table:modification_table
          ~values:
            Expr.
              [ doc_id := s id; modif_number := i idx; modif_field := s change ]
        |> Request.make_zero |> Petrol.exec db
      in
      Lwt_list.mapi_s insert all

    (* let insert ~id ~version ~modif:(mid, change) = *)
    (*   let* db = conn in *)
    (*   let change = *)
    (*     Camlot.Changes.ChangeSet.to_JSON change |> Yojson.Safe.to_string *)
    (*   in *)
    (*   let blob = *)
    (*     `List [ `String mid; `String change ] |> Yojson.Safe.to_string *)
    (*   in *)
    (*   Query.insert ~table:modification_table *)
    (*     ~values: *)
    (*       Expr. *)
    (*         [ doc_id := s id; modif_number := i version; modif_field := s blob ] *)
    (*   |> Request.make_zero |> Petrol.exec db *)

    (* let find_above ~id ~version = *)
    (*   let* db = conn in *)
    (*   let+ result = *)
    (*     Query.select Expr.[ modif_number; modif_field ] ~from:modification_table *)
    (*     |> Query.where Expr.(doc_id = s id && modif_number > i version) *)
    (*     |> Query.order_by modif_number *)
    (*     |> Request.make_many |> Petrol.collect_list db *)
    (*   in *)
    (*   match result with *)
    (*   | Error _ -> failwith "no result for find_above" *)
    (*   | Ok l -> List.map (fun (version, (changes, ())) -> (version, changes)) l *)
  end

  (* let init_doc ~id = *)
  (*   let content = "# An empty slipshow presentation" in *)
  (*   let show_id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in *)
  (*   Document.insert ~id ~content ~version:0 ~show_id *)

  (* let lock_if c f = if c then Lwt_mutex.with_lock mut f else f () *)

  (* let rec collect_doc ~lock id = *)
  (*   lock_if lock @@ fun () -> *)
  (*   let* res = Document.find_opt id in *)
  (*   match res with *)
  (*   | Ok None -> *)
  (*       let* _ = init_doc ~id in *)
  (*       collect_doc ~lock:false id *)
  (*   | Ok (Some (content, (version, (show_id, ())))) -> *)
  (*       Lwt.return (content, version, show_id) *)
  (*   | Error _ -> failwith "collect" *)

  (* let collect_changes ~id ~version = *)
  (*   lock_if true @@ fun () -> Changes.find_above ~id ~version *)

  (* let rec collect_show_doc ~lock id = *)
  (*   lock_if lock @@ fun () -> *)
  (*   let* res = Document.find_from_show_id_opt id in *)
  (*   match res with *)
  (*   | Ok None -> *)
  (*       let* _ = init_doc ~id in *)
  (*       collect_show_doc ~lock:false id *)
  (*   | Ok (Some (content, (version, ()))) -> Lwt.return (content, version) *)
  (*   | Error _ -> failwith "collect" *)

  (* let update_doc ~id ~changes ~from_version = *)
  (*   lock_if true @@ fun () -> *)
  (*   (\* For the side effect that it inits the doc if needed *\) *)
  (*   let* content, version, _show_id = collect_doc ~lock:false id in *)
  (*   if version <> from_version then Lwt.return_ok () *)
  (*   else *)
  (*     let content = *)
  (*       let content = Sliphub.Converter.utf8_to_utf16 content in *)
  (*       List.fold_left *)
  (*         (fun doc (_id, change) -> *)
  (*           let change = *)
  (*             List.map *)
  (*               (function *)
  (*                 | Camlot.Changes.Keep k -> Camlot.Changes.Keep (2 * k) *)
  (*                 | Replace (i, (l_s, s)) -> *)
  (*                     Replace *)
  (*                       (2 * i, (2 * l_s, Sliphub.Converter.utf8_to_utf16 s))) *)
  (*               change *)
  (*           in *)
  (*           let res = *)
  (*             Camlot.Changes.ChangeSet.apply (\* ~is_utf16:true  *\) change doc *)
  (*           in *)
  (*           res) *)
  (*         content changes *)
  (*     in *)
  (*     let content = Sliphub.Converter.utf16_to_utf8 content in *)
  (*     Dream.log "Document is '%s'" content; *)
  (*     let* () = *)
  (*       Lwt_list.iteri_s *)
  (*         (fun i modif -> *)
  (*           let+ res = Changes.insert ~id ~version:(version + i + 1) ~modif in *)
  (*           match res with *)
  (*           | Ok () -> () *)
  (*           | Error _ -> failwith "failing to add change") *)
  (*         changes *)
  (*     in *)
  (*     Document.update ~id ~content ~version:(version + List.length changes) *)

  (* let collect_doc = collect_doc ~lock:true *)
  (* let collect_show_doc = collect_show_doc ~lock:true *)
end

let main () =
  let open Lwt.Syntax in
  let* documents = DB_sqlite.Document.find_all () in
  let* _ = DB_PG.Document.insert_all documents in
  print_endline "Documents: Done";
  let* changes = DB_sqlite.Changes.find_all () in
  let+ _ = DB_PG.Changes.insert_all changes in
  print_endline "Done"

let () = Lwt_main.run (main ())

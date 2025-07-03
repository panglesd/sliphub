open Lwt.Syntax
open Sliphub

let () = Random.self_init ()

open Common

let _ =
  let get_asset route asset mime =
    Dream.get route (fun _ ->
        let response = Dream.response Assets.(read asset) in
        Dream.add_header response "charset" "utf-8";
        Dream.add_header response "Content-Type" mime;
        Lwt.return response)
  in
  let index_page =
    Dream.get Routes.index_page (fun _ ->
        let intro_pres = Assets.(read Intro_pres) in
        let html = Slipshow.convert ~width:1440 ~height:1080 intro_pres in
        Dream.html html)
  in
  let new_page =
    Dream.get Routes.new_page (fun request ->
        let id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in
        Dream.redirect request ("/" ^ id))
  in
  let js_file = get_asset Routes.js_file Assets.Index_js "text/javascript" in
  let css_file = get_asset Routes.css_file Assets.Index_css "text/css" in
  let send_document =
    Dream.get
      Routes.(dream_route send_document)
      (fun request ->
        let id = Dream.param request Routes.(parameter send_document) in
        Dream.log "queryin document %s" id;
        let* document, version, show_id = Db.collect_doc id in
        Dream.log "document is %s" document;
        let payload =
          Yojson.Safe.to_string
          @@ `List [ `Int version; `String document; `String show_id ]
        in
        Dream.respond payload)
  in
  let view_document =
    Dream.get
      Routes.(dream_route view_document)
      (fun request ->
        let id = Dream.param request Routes.(parameter view_document) in
        let* document = Db.collect_show_doc id in
        let document =
          match document with
          | None -> "No document at this location"
          | Some (doc, _) -> doc
        in
        let slipshow = Slipshow.convert ~width:1440 ~height:1080 document in
        Dream.html slipshow)
  in
  let edit_document =
    Dream.get "/:document" (fun _ -> Dream.html Assets.(read Index_html))
  in
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       (Change_notifications.notif_changes
       @ [
           index_page;
           new_page;
           js_file;
           css_file;
           view_document;
           edit_document;
           send_document;
         ])

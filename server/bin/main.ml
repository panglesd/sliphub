open Lwt.Syntax
open Sliphub

let () = Random.self_init ()

open Common

let _ =
  let index_page =
    Dream.get Routes.index_page (fun _ ->
        let intro_pres = Assets.(read Intro_pres) in
        let html = Slipshow.convert intro_pres in
        Dream.html html)
  in
  let new_page =
    Dream.get Routes.new_page (fun request ->
        let id = String.init 10 (fun _ -> Char.chr (97 + Random.int 26)) in
        Dream.redirect request ("/" ^ id))
  in
  let js_file =
    Dream.get Routes.js_file (fun _ ->
        let response = Dream.response Assets.(read Index_js) in
        Dream.add_header response "charset" "utf-8";
        Dream.add_header response "Content-Type" "text/javascript";
        Lwt.return response)
  in
  let css_file =
    Dream.get Routes.css_file (fun _ ->
        let response = Dream.response Assets.(read Index_css) in
        Dream.add_header response "charset" "utf-8";
        Dream.add_header response "Content-Type" "text/css";
        Lwt.return response)
  in
  let send_document =
    Dream.get
      Routes.(dream_route send_document)
      (fun request ->
        let id = Dream.param request Routes.(parameter send_document) in
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
    Dream.get
      Routes.(dream_route view_document)
      (fun request ->
        let id = Dream.param request Routes.(parameter view_document) in
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

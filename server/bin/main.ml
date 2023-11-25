module State = struct
  module Docs = Hashtbl.Make (String)

  let docs = Docs.create 100

  let _get_doc id =
    match Docs.find_opt docs id with
    | Some d -> d
    | None ->
        Docs.add docs id "";
        id

  let _set_doc id value = Docs.replace docs id value
end

open Lwt.Syntax

let () =
  Dream.run @@ Dream.logger
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
         Dream.get "/websocket" (fun _ ->
             Dream.websocket ~close:false (fun websocket ->
                 let rec loop () =
                   let* recv = Dream.receive websocket in
                   match recv with
                   | Some "Hello?" ->
                       let _ = Dream.send websocket "Good-bye!" in
                       loop ()
                   | _ -> (* Dream.close_websocket websocket *) Lwt.return ()
                 in
                 loop ()));
       ]

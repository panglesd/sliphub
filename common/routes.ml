type no_parameter_route = string
type route_with_doc_id = string list

let route_segments ?version route id = route @ [ id ] @ Option.to_list version
let parameter _route = "document"
let version _route = "version"
let index_page = "/"
let new_page = "/new"
let js_file = "/index.js"
let css_file = "/index.css"
let send_document = [ "longpolling"; "getDocument" ]
let receive_changes = [ "longpolling"; "push" ]
let push_and_receive_changes = [ "longpolling"; "push_and_receive" ]
let send_changes = [ "longpolling"; "pull" ]
let view_document = [ "view" ]

let dream_route route =
  match route with
  | [ "longpolling"; "pull" ] -> "/longpolling/pull/:document/:version"
  | [ "longpolling"; "push_and_receive" ] ->
      "/longpolling/push_and_receive/:document/:version"
  | _ -> "/" ^ String.concat "/" route ^ "/" ^ ":document"

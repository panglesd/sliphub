type no_parameter_route = string
type route_with_doc_id = string list

let route_segments route id = route @ [ id ]
let parameter _route = "document"
let dream_route route = "/" ^ String.concat "/" route ^ "/" ^ ":document"
let index_page = "/"
let new_page = "/new"
let js_file = "/index.js"
let css_file = "/index.css"
let send_document = [ "websocket"; "getDocument" ]
let receive_changes = [ "websocket"; "push" ]
let send_changes = [ "websocket"; "pull" ]
let view_document = [ "view" ]

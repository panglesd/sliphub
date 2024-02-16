type no_parameter_route = string
type route_with_doc_id

val route_segments : route_with_doc_id -> string -> string list
val parameter : route_with_doc_id -> string
val dream_route : route_with_doc_id -> string
val index_page : no_parameter_route
val new_page : no_parameter_route
val js_file : no_parameter_route
val css_file : no_parameter_route
val send_document : route_with_doc_id
val receive_changes : route_with_doc_id
val send_changes : route_with_doc_id
val view_document : route_with_doc_id

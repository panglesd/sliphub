type file = Index_js

let string_of_file = function Index_js -> "index.js"

let read f =
  Data_contents.read (string_of_file f) |> function
  | Some c -> c
  | None -> assert false

type file = Index_js | Index_html | Index_css | Intro_pres

let string_of_file = function
  | Index_js -> "index.js"
  | Index_html -> "index.html"
  | Index_css -> "index.css"
  | Intro_pres -> "intro_presentation.md"

let read f =
  Data_contents.read (string_of_file f) |> function
  | Some c -> c
  | None -> assert false

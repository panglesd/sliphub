open Code_mirror

let create_nav_bar show_id view =
  let downLoadSource () =
    let open Editor in
    let content =
      let state = View.state view in
      let text = State.doc state in
      let lines =
        Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
      in
      String.concat "\n" lines
    in
    Jv.apply
      (Jv.get Jv.global "download")
      [| Jv.of_string "source.md"; Jv.of_string content |]
  in
  Jv.set Jv.global "downLoadSource" (Jv.callback ~arity:1 downLoadSource);
  let downloadPresentation () =
    let open Editor in
    let content =
      let state = View.state view in
      let text = State.doc state in
      let lines =
        Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
      in
      String.concat "\n" lines
    in
    let content = Slipshow.convert content in
    Jv.apply
      (Jv.get Jv.global "download")
      [| Jv.of_string "presentation.html"; Jv.of_string content |]
  in
  Jv.set Jv.global "downLoadPresentation"
    (Jv.callback ~arity:1 downloadPresentation);
  let () =
    let uri = Brr.Window.location Brr.G.window in
    let id =
      Brr.Uri.with_path_segments uri [ Jstr.v "view"; Jstr.v show_id ]
      |> Result.get_ok |> Brr.Uri.to_jstr
    in
    let a =
      Brr.El.find_first_by_selector (Jstr.v "#startPresentation") |> function
      | Some a -> a
      | None -> assert false
    in
    Brr.El.set_at Brr.At.Name.href (Some id) a
  in
  ()

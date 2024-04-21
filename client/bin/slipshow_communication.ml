open Code_mirror

let update_slipshow state view =
  let open Editor in
  let content =
    let state = View.state view in
    let text = State.doc state in
    let lines =
      Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
    in
    String.concat "\n" lines
  in
  Previewer.preview state content

let slipshow_plugin =
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let state =
        Previewer.create_previewer
          (Brr.El.find_first_by_selector (Jstr.v "#right-panel") |> Option.get)
      in
      update_slipshow state view;
      let update upd =
        if View.Update.docChanged upd then update_slipshow state view else ()
      in
      let destruct () = () in
      { update; destruct })

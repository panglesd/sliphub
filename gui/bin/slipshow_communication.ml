open Code_mirror

let update_slipshow previewer view =
  let open Editor in
  let content =
    let state = View.state view in
    let text = State.doc state in
    let lines =
      Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
    in
    String.concat "\n" lines
  in
  let slipshow = Slipshow.delayed ~width:1440 ~height:1080 content in
  Previewer.preview_compiled previewer slipshow

let slipshow_plugin =
  let open Editor in
  let root =
    Brr.El.find_first_by_selector (Jstr.v "#right-panel") |> Option.get
  in
  let previewer = Previewer.create_previewer root in
  View.ViewPlugin.define (fun view ->
      update_slipshow previewer view;
      let update upd =
        if View.Update.docChanged upd then update_slipshow previewer view
        else ()
      in
      let destruct () = () in
      { update; destruct })

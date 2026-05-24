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
  let entry_point = Fpath.v "-" in
  let read_file f =
    if Fpath.equal entry_point f then Ok (Some content) else Ok None
  in
  let slipshow, warnings =
    Slipshow.delayed ~has_speaker_view:true ~read_file entry_point
  in
  let warnings =
    let config =
      { Grace_ansi_renderer.Config.default with use_ansi = Some false }
    in
    List.map
      (Format.asprintf "%a@.@."
         (Grace_ansi_renderer.pp_diagnostic ?config:(Some config)
            ~code_to_string:Diagnosis.to_code))
      warnings
    |> String.concat ""
  in
  Previewer.preview_compiled previewer (slipshow, warnings)

let slipshow_plugin =
  let open Editor in
  let root =
    Brr.El.find_first_by_selector (Jstr.v "#right-panel") |> Option.get
  in
  (* TODO: do *)
  let previewer =
    Previewer.create_previewer ~errors_el:(Brr.El.div []) ~steal_focus:false
      ~include_speaker_view:false root
  in
  View.ViewPlugin.define (fun view ->
      update_slipshow previewer view;
      let update upd =
        if View.Update.docChanged upd then update_slipshow previewer view
        else ()
      in
      let destruct () = () in
      { update; destruct })

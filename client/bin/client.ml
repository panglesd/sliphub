open Code_mirror
open Lwt.Syntax

let view_and_show_id =
  let open Editor in
  let open Protocol in
  let+ { Communication.version; document; show_id } =
    Communication.getDocument ()
  in
  let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv in
  let dark_mode =
    let dark = Jv.get Jv.global "__CM__dark" in
    let oneDark = Jv.get dark "oneDark" in
    Extension.of_jv oneDark
  in
  let markdown_extension =
    Jv.apply (Jv.get Jv.global "__CM__markdown") [||] |> Extension.of_jv
  in
  let config =
    State.Config.create ~doc:(Jstr.v document)
      ~extensions:
        [|
          basic_setup;
          markdown_extension;
          dark_mode;
          Collab_protocol.collab version;
          Collab_protocol.peer_plugin;
          Slipshow_communication.slipshow_plugin;
        |]
      ()
  in
  Jv.set Jv.global "show_id" (Jv.of_string show_id);
  let state = State.create ~config () in
  let parent = Brr.El.find_first_by_selector (Jstr.v "#editor") |> Option.get in
  let opts = Editor.View.opts ~state ~parent () in
  (Editor.View.create ~opts (), show_id)

open Lwt.Infix

let view, show_id = (view_and_show_id >|= fst, view_and_show_id >|= snd)

let _ : unit Lwt.t =
  let* view = view in
  let+ show_id = show_id in
  Top_bar.create_nav_bar show_id view

open Code_mirror
open Lwt.Syntax

module Msg = struct
  type kind = State_update of int list | Ready
  type msg = string * kind

  let of_jv m : msg =
    let id = Jv.get m "id" |> Jv.to_string in
    let data = Jv.get m "data" in
    Jv.get m "kind" |> Jv.to_string |> function
    | "state" ->
        let data =
          Jv.to_string data |> String.split_on_char ','
          |> List.map int_of_string
        in
        (id, State_update data)
    | "ready" -> (id, Ready)
    | _ -> assert false
end

module PreviewState = struct
  let stage = ref [ 0 ]
  let string_of_stage stage = List.map string_of_int stage |> String.concat ", "

  let panel1 =
    Brr.El.find_first_by_selector (Jstr.v "#right-panel1") |> Option.get

  let panel2 =
    Brr.El.find_first_by_selector (Jstr.v "#right-panel2") |> Option.get

  let panels = [| panel1; panel2 |]
  let ids = [| "p1"; "p2" |]
  let index = ref 0

  let _ =
    Brr.Ev.listen Brr_io.Message.Ev.message
      (fun event ->
        let raw_data : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
        (* Brr.Console.(log [ raw_data ]); *)
        let msg = Msg.of_jv raw_data in
        match msg with
        | id, Msg.State_update new_stage when id = ids.(!index) ->
            print_endline @@ "updating stage from: " ^ string_of_stage !stage
            ^ " to new stage: " ^ string_of_stage new_stage;
            stage := new_stage
        | "p1", Msg.Ready ->
            print_endline "p1 is ready";
            index := 0;
            Brr.El.set_class (Jstr.v "active_panel") true panels.(!index);
            Brr.El.set_class (Jstr.v "active_panel") false panels.(1 - !index)
        | "p2", Msg.Ready ->
            print_endline "p2 is ready";
            index := 1;
            Brr.El.set_class (Jstr.v "active_panel") true panels.(!index);
            Brr.El.set_class (Jstr.v "active_panel") false panels.(1 - !index)
        | _ -> ())
      (Brr.Window.as_target Brr.G.window)

  let unused () = 1 - !index

  (* let set_stage new_stage = stage := new_stage *)
  let get_starting_state () =
    print_endline @@ "Get_starting_state = " ^ string_of_stage !stage ^ "; "
    ^ ids.(unused ());
    (!stage, ids.(unused ()))

  let set_srcdoc slipshow =
    print_endline @@ "Set_srcdoc = " ^ ids.(unused ());
    Jv.set (Brr.El.to_jv panels.(unused ())) "srcdoc" (Jv.of_string slipshow)
end

let update_slipshow view =
  let open Editor in
  let content =
    let state = View.state view in
    let text = State.doc state in
    let lines =
      Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
    in
    String.concat "\n" lines
  in
  let starting_state = PreviewState.get_starting_state () in
  let slipshow = Slipshow.convert ~starting_state content in
  PreviewState.set_srcdoc slipshow

let slipshow_plugin =
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let update upd =
        if View.Update.docChanged upd then update_slipshow view else ()
      in
      let destruct () = () in
      { update; destruct })

let state_and_show_id =
  let open Editor in
  let+ start_version, doc, show_id = Communication.getDocument () in
  (* Format.printf "START VERSION IS %d\n%!" start_version; *)
  let config = Collab.config ~start_version () in
  let collab = Collab.collab ~config () in
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
    State.Config.create ~doc:(Jstr.v doc)
      ~extensions:
        [|
          collab;
          Client_collab.peer_plugin;
          basic_setup;
          slipshow_plugin;
          markdown_extension;
          dark_mode;
        |]
      ()
  in
  Jv.set Jv.global "show_id" (Jv.of_string show_id);
  (State.create ~config (), show_id)

open Lwt.Infix

let state, show_id = (state_and_show_id >|= fst, state_and_show_id >|= snd)
let parent = Brr.El.find_first_by_selector (Jstr.v "#editor") |> Option.get

let view =
  let+ state = state in
  let opts = Editor.View.opts ~state ~parent () in
  Editor.View.create ~opts ()

let _ =
  let* show_id = show_id in
  let+ view = view in
  update_slipshow view;
  let _ = Client_collab.pull view in
  let _ = Jv.set Jv.global "view" (Editor.View.to_jv view) in
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

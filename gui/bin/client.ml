open Code_mirror
open Brr
(* open Brr_lwd *)

module State = struct
  let path = Lwd.var None
  let need_save = Lwd.var true
end

let edit_state_plugin =
  Editor.View.ViewPlugin.define (fun _view ->
      let update upd =
        if Editor.View.Update.docChanged upd then Lwd.set State.need_save true
        else ()
      in
      let destruct () = () in
      { update; destruct })

let state initial_content =
  let open Editor in
  let document = initial_content in
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
    State.Config.create ~doc:document
      ~extensions:
        [|
          basic_setup;
          markdown_extension;
          dark_mode;
          Slipshow_communication.slipshow_plugin;
          edit_state_plugin;
        |]
      ()
  in
  State.create ~config ()

let save_button view =
  let open Fut.Syntax in
  let res =
    El.input ~at:[ At.type' (Jstr.v "button"); At.value (Jstr.v "Save") ] ()
  in
  let save _ev =
    let _ =
      let save path =
        let contents =
          let state = Editor.View.state view in
          let doc = Editor.State.doc state in
          let array = Text.to_jstr_array doc in
          let list = Array.to_list array in
          Jstr.concat ~sep:Jstr.nl list
        in
        let+ res = Tauri_api.Fs.write_text_file ~path ~contents () in
        match res with
        | Error _ -> failwith "error when writing"
        | Ok () ->
            Lwd.set State.need_save false;
            Lwd.set State.path (Some path);
            ()
      in
      match Lwd.peek State.path with
      | None -> (
          let* new_path = Tauri_api.Dialog.save () in
          match new_path with
          | Ok None -> Fut.return ()
          | Ok (Some new_path) -> save new_path
          | Error _ -> failwith "____")
      | Some path -> save path
    in
    ()
  in
  ignore @@ Ev.listen Ev.click save (El.as_target res);
  res

let open_button view =
  let open Fut.Syntax in
  let res =
    El.input ~at:[ At.type' (Jstr.v "button"); At.value (Jstr.v "Open") ] ()
  in
  let open_ _ev =
    ignore
    @@ let* new_path = Tauri_api.Dialog.open_ () in
       match new_path with
       | Ok [ file_path ] -> (
           let+ content = Tauri_api.Fs.read_text_file ~file_path () in
           match content with
           | Error _ -> ()
           | Ok content ->
               Lwd.set State.need_save false;
               Lwd.set State.path (Some file_path);
               let state = state content in
               Editor.View.set_state view state)
       | _ -> Fut.return ()
  in
  ignore @@ Ev.listen Ev.click open_ (El.as_target res);
  res

let new_button view =
  let res =
    El.input ~at:[ At.type' (Jstr.v "button"); At.value (Jstr.v "New") ] ()
  in
  let new_ _ev =
    Lwd.set State.path None;
    Lwd.set State.need_save true;
    let state = state Jstr.empty in
    Editor.View.set_state view state
  in
  ignore @@ Ev.listen Ev.click new_ (El.as_target res);
  res

let make_top_bar view =
  let root_elem =
    Brr.El.find_first_by_selector (Jstr.v "#toolbar") |> Option.get
  in
  let open Brr in
  let open Brr_lwd in
  let need_save = Lwd.get State.need_save in
  let current_path = Lwd.get State.path in
  let ui =
    let open Lwd_infix in
    (* Console.(log [ "yooo" ]); *)
    (* State.need_save $= true; *)
    let title =
      let attr =
        let$ need_save = need_save in
        if need_save then At.class' (Jstr.v "need-save")
        else At.class' (Jstr.v "saved")
      in
      let str =
        let$ current_path = current_path in
        match current_path with
        | Some x -> El.txt x
        | None -> El.txt' "Untitled"
      in
      Elwd.span ~at:[ `R attr ] [ `R str ]
    in
    let res =
      (* if need_save then Elwd.div [ `P (El.txt' "yo") ] *)
      (* else Elwd.div [ `P (El.txt' "ya") ] *)
      Elwd.div
        [
          `R title;
          `P (new_button view);
          `P (save_button view);
          `P (open_button view);
        ]
    in
    (* let res = Elwd.div [ `R Elwd.txt ] in *)
    res
  in
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    ignore @@ G.request_animation_frame
    @@ fun _ ->
    ignore
    @@
    let ui = Lwd.quick_sample ui in
    El.set_children root_elem [ ui ]
  in

  let on_load _ =
    El.append_children root_elem [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore @@ Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

let _ =
  let state = state Jstr.empty in
  let parent = Brr.El.find_first_by_selector (Jstr.v "#editor") |> Option.get in
  let opts = Editor.View.opts ~state ~parent () in
  let view = Editor.View.create ~opts () in
  let () = make_top_bar view in
  view

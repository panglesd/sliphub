open Code_mirror

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
      update_slipshow view;
      let update upd =
        if View.Update.docChanged upd then update_slipshow view else ()
      in
      let destruct () = () in
      { update; destruct })

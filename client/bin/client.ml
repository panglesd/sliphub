open Code_mirror

let push view =
  let open Editor in
  let state = View.state view in
  let updates = Collab.sendableUpdates state in
  if List.is_empty updates then ()
  else
    let version = Collab.getSyncedVersion state in
    let updates = List.map fst updates in
    let _ = Communication.push_updates version updates in
    ()

let peer_plugin =
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let push () = push view in
      let update upd =
        if View.Update.docChanged upd then
          let _ = push () in
          ()
        else ()
      in
      let destruct () = () in
      { update; destruct })

open Lwt.Syntax

let update_slipshow state view =
  let open Editor in
  let result =
    match Brr.El.find_first_by_selector (Jstr.v "#right-panel") with
    | None ->
        print_endline "rate";
        assert false
    | Some x -> x
  in
  let content =
    let state = View.state view in
    let text = State.doc state in
    let lines =
      Text.to_jstr_array text |> Array.map Jstr.to_string |> Array.to_list
    in
    String.concat "\n" lines
  in
  let slipshow =
    try Slip_of_mark.convert ?starting_state:state content
    with Jsoo_runtime.Error.Exn exn ->
      Brr.Console.(log [ exn ]);
      "error"
  in
  (* Brr.Console.(log [ slipshow ]); *)
  let () = Jv.set (Brr.El.to_jv result) "srcdoc" (Jv.of_string slipshow) in
  let _ =
    match state with
    | None -> ()
    | Some state ->
        (* let window = *)
        (*   Jv.get (Brr.El.to_jv result) "contentWindow" |> Brr.Window.of_jv *)
        (* in *)
        Brr.Console.(log [ ("sending state", state) ])
    (* ; *)
    (* Brr_io.Message.window_post window state *)
  in
  (* Brr.Window Jv.call result "contentWindow" *)
  ()

module Msg = struct
  type kind = State_update | Request_for_state
  type msg = { kind : kind; data : Jv.t }

  let () = ignore Request_for_state

  let of_jv m =
    let kind =
      Jv.get m "kind" |> Jv.to_string |> function
      | "state" -> State_update
      | _ -> assert false
    in
    let data = Jv.get m "data" in
    { kind; data }
end

let slipshow_plugin =
  let state = ref None in
  let _listen_for_state_change =
    Brr.Ev.listen Brr_io.Message.Ev.message
      (fun event ->
        (* ignore event *)
        let raw_data : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
        Brr.Console.(log [ raw_data ]);
        let { Msg.kind = _; data } = Msg.of_jv raw_data in
        Brr.Console.(log [ ("receiving state", data) ]);
        let data =
          Jv.to_string data |> String.split_on_char ','
          |> List.map int_of_string
        in
        state := Some data)
      (Brr.Window.as_target Brr.G.window)
  in
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let update upd =
        (* try *)
        if View.Update.docChanged upd then update_slipshow !state view else ()
        (* with _ -> *)
        (*   print_endline "yooooooo"; *)
        (*   () *)
      in
      let destruct () = () in
      { update; destruct })

let state =
  let open Editor in
  let+ start_version, doc = Communication.getDocument () in
  (* Format.printf "START VERSION IS %d\n%!" start_version; *)
  let config = Collab.config ~start_version () in
  let collab = Collab.collab ~config () in
  let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv in
  let markdown_extension =
    Jv.apply (Jv.get Jv.global "__CM__markdown") [||] |> Extension.of_jv
  in
  let config =
    State.Config.create ~doc:(Jstr.v doc)
      ~extensions:
        [|
          collab; peer_plugin; basic_setup; slipshow_plugin; markdown_extension;
        |]
      ()
  in
  State.create ~config ()

let parent = Brr.El.find_first_by_selector (Jstr.v "#editor") |> Option.get

let view =
  let+ state = state in
  let opts = Editor.View.opts ~state ~parent () in
  Editor.View.create ~opts ()

let get_version () =
  let+ view = view in
  let state = Editor.View.state view in
  let get_version = Collab.getSyncedVersion state in
  Brr.Console.(log [ ("version is ", get_version) ])
(* Format.printf "version is %d\n%!" get_version *)

let pull () =
  let+ view = view in
  let get_version () =
    let state = Editor.View.state view in
    Collab.getSyncedVersion state
  in
  Communication.recv_updates
    (fun changes ->
      (* Format.printf "receiving changes!%!\n"; *)
      let _ =
        let update =
          List.map (fun (id, change) -> Collab.Update.make change id) changes
        in
        (* List.iter (fun upd -> Brr.Console.(log [ upd ])) update; *)
        let state = Editor.View.state view in

        let transaction = Collab.receiveUpdates state update in
        (* Brr.Console.(log [ transaction ]); *)
        let _ = Editor.View.dispatch view [ transaction ] in
        ()
      in
      ())
    get_version

let _ =
  let+ view = view in
  update_slipshow None view;
  let _ = pull () in
  let _ = Jv.set Jv.global "view" (Editor.View.to_jv view) in
  ()

let _ = Jv.set Jv.global "get_version" (Jv.callback ~arity:1 get_version)
let _ = Jv.set Jv.global "pull" (Jv.callback ~arity:1 pull)

(* let goto_somewhere () = *)
(*   let result = *)
(*     match Brr.El.find_first_by_selector (Jstr.v "#right-panel") with *)
(*     | None -> *)
(*         print_endline "rate"; *)
(*         assert false *)
(*     | Some x -> x *)
(*   in *)
(*   let _ = *)
(*     match Some (Jstr.v "0,3") with *)
(*     | None -> () *)
(*     | Some state -> *)
(*         (\* let window = *\) *)
(*         (\*   Jv.get (Brr.El.to_jv result) "contentWindow" |> Brr.Window.of_jv *\) *)
(*         (\* in *\) *)
(*         (\* Brr.Console.(log [ "sending state"; state; "to"; window ]) *\) *)
(*     (\* Brr_io.Message.window_post window state *\) *)
(*   in *)

(*   (\* Brr.Window Jv.call result "contentWindow" *\) *)
(*   () *)

(* let _ = Jv.set Jv.global "goto_somewhere" (Jv.callback ~arity:1 goto_somewhere) *)

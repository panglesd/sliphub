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

let update_slipshow view =
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
  let slipshow = Slip_of_mark.convert content in
  let () = Jv.set (Brr.El.to_jv result) "srcdoc" (Jv.of_string slipshow) in
  ()

let slipshow_plugin =
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let update upd =
        if View.Update.docChanged upd then update_slipshow view else ()
      in
      let destruct () = () in
      { update; destruct })

let state =
  let open Editor in
  let+ start_version, doc = Communication.getDocument () in
  Format.printf "START VERSION IS %d\n%!" start_version;
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
  let+ state in
  let opts = Editor.View.opts ~state ~parent () in
  Editor.View.create ~opts ()

let get_version () =
  let+ view in
  let state = Editor.View.state view in
  let get_version = Collab.getSyncedVersion state in
  Format.printf "version is %d\n%!" get_version

let pull () =
  let+ view in
  let get_version () =
    let state = Editor.View.state view in
    Collab.getSyncedVersion state
  in
  Communication.recv_updates
    (fun changes ->
      Format.printf "receiving changes!%!\n";
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
  let+ view in
  update_slipshow view;
  let _ = pull () in
  let _ = Jv.set Jv.global "view" (Editor.View.to_jv view) in
  ()

let _ = Jv.set Jv.global "get_version" (Jv.callback ~arity:1 get_version)
let _ = Jv.set Jv.global "pull" (Jv.callback ~arity:1 pull)

let () =
  let open Brr_io.Websocket in
  let ws = create (Jstr.v "ws://localhost:8080/websocket") in

  let i = ref 0 in
  let _listener =
    Brr.Ev.listen Brr_io.Message.Ev.message
      (fun event ->
        Format.printf "i is %d\n%!" !i;
        incr i;
        let str : Jstr.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
        print_endline (Jstr.to_string str))
      (as_target ws)
  in
  let when_open _ =
    let send () =
      let open Brr_io.Websocket in
      print_endline "state is";
      print_endline
      @@
      if ready_state ws = Ready_state.open' then "open"
      else if ready_state ws = Ready_state.closed then "closed"
      else if ready_state ws = Ready_state.connecting then "connecting"
      else (* if ready_state ws = Ready_state.closing then *) "closing";
      if ready_state ws = Ready_state.open' then
        send_string ws (Jstr.v "Hello?")
    in
    for _ = 0 to 10 do
      send ()
    done
  in
  let _open_listener = Brr.Ev.listen Brr.Ev.open' when_open (as_target ws) in
  ()

open Code_mirror

let push view =
  let open Editor in
  let state = View.state view in
  let updates = Collab.sendableUpdates state in
  if List.is_empty updates then ()
  else
    let version = Collab.getSyncedVersion state in
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

let slipshow_plugin =
  let result =
    match Brr.El.find_first_by_selector (Jstr.v "#right-panel") with
    | None ->
        print_endline "rate";
        assert false
    | Some x -> x
  in
  let open Editor in
  View.ViewPlugin.define (fun view ->
      let update upd =
        if View.Update.docChanged upd then
          let content =
            let state = View.state view in
            let text = State.doc state in
            let lines =
              Text.to_jstr_array text |> Array.map Jstr.to_string
              |> Array.to_list
            in
            String.concat "\n" lines
          in
          let slipshow = Slip_of_mark.convert content in
          let () =
            Jv.set (Brr.El.to_jv result) "srcdoc" (Jv.of_string slipshow)
          in
          ()
        else ()
      in
      let destruct () = () in
      { update; destruct })

let state =
  let open Editor in
  let collab = Collab.collab () in
  let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv in
  let markdown_extension =
    Jv.apply (Jv.get Jv.global "__CM__markdown") [||] |> Extension.of_jv
  in
  let config =
    State.Config.create
      ~extensions:
        [|
          collab; peer_plugin; basic_setup; slipshow_plugin; markdown_extension;
        |]
      ()
  in
  State.create ~config ()

let parent = Brr.El.find_first_by_selector (Jstr.v "#editor") |> Option.get

let _view =
  let opts = Editor.View.opts ~state ~parent () in
  Editor.View.create ~opts ()

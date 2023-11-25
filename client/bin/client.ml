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

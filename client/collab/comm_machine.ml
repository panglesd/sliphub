(* Never sends another request while waiting for a response *)

(*

  When there is no data to send, wait for the first of those events to happen:
  - There is some data to send (signalled via a condition),
  - There is some data to receive (via some response to a long-polling request).

  If there is some data to send, cancel the long polling request. Send the following request:
    « Here is my updates. Send me the newest updates »

  If there is some data to receive, 



 *)
open Code_mirror
open Code_mirror.Editor

let start view cond : unit Lwt.t =
  let rec do_ () =
    let open Lwt.Syntax in
    let version () =
      let state = View.state view in
      Collab.getSyncedVersion state
    in
    let data_from_server =
      let version = version () in
      let+ nd = Communication.recv_updates_atomic version in
      `FromServer nd
    in
    let data_from_client =
      let rec wait_for_updates () =
        let state = View.state view in
        let updates = Collab.sendableUpdates state in
        if List.is_empty updates then
          let* () = Lwt_condition.wait cond in
          wait_for_updates ()
        else Lwt.return (`FromClient updates)
      in
      wait_for_updates ()
    in
    let* res = Lwt.pick [ data_from_client; data_from_server ] in
    let from_server updates =
      let updates =
        List.map (fun (id, change) -> Collab.Update.make change id) updates
      in
      let state = Editor.View.state view in
      let transaction = Collab.receiveUpdates state updates in
      let _ = Editor.View.dispatch view [ transaction ] in
      ()
    in
    let* () =
      match res with
      | `FromClient updates ->
          let state = Editor.View.state view in
          let version = Collab.getSyncedVersion state in
          let updates = List.map fst updates in
          let+ updates = Communication.push_updates_atomic version updates in
          (* Next, push_updates will send back the  *)
          from_server updates
      | `FromServer updates -> Lwt.return @@ from_server updates
    in
    do_ ()
  in
  do_ ()

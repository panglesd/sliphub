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
      let pull view =
        let get_version () =
          let state = Editor.View.state view in
          Collab.getSyncedVersion state
        in
        Communication.recv_updates
          (fun changes ->
            let update =
              List.map
                (fun (id, change) -> Collab.Update.make change id)
                changes
            in
            let state = Editor.View.state view in
            let transaction = Collab.receiveUpdates state update in
            let _ = Editor.View.dispatch view [ transaction ] in
            ())
          get_version
      in
      let () = pull view in
      let push () = push view in
      let update upd =
        if View.Update.docChanged upd then
          let _ = push () in
          ()
        else ()
      in
      let destruct () = () in
      { update; destruct })

let collab start_version =
  let config = Collab.config ~start_version () in
  let collab = Collab.collab ~config () in
  collab

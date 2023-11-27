let decode msg =
  let json = Yojson.Safe.from_string msg in
  match json with
  | `List [ `Int version; `List updates ] ->
      let changes =
        List.map
          (function
            | `List [ `String id; `String json ] ->
                let json = Yojson.Safe.from_string json in
                (id, Camlot.Changes.ChangeSet.fromJSON json)
            | _ -> failwith "wrong conversion")
          updates
      in
      (version, changes)
  | _ -> failwith "wrong conversion2"

let recode ?nln ?encoding out_encoding
    (src : [ `Channel of in_channel | `String of string ])
    (dst : [ `Channel of out_channel | `Buffer of Buffer.t ]) =
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u ->
        ignore (Uutf.encode e u);
        loop d e
    | `End -> ignore (Uutf.encode e `End)
    | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep));
        loop d e
    | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e

let utf8_to_utf16 src =
  let out_encoding = `UTF_16 in
  let dst = Buffer.create 100 in
  recode out_encoding (`String src) (`Buffer dst);
  Buffer.contents dst

let utf16_to_utf8 src =
  let out_encoding = `UTF_8 in
  let dst = Buffer.create 100 in
  recode out_encoding (`String src) (`Buffer dst);
  Buffer.contents dst

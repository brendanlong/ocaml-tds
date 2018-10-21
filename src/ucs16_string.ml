(** A UCS-16 encoded string in network byte order *)
open Sexplib.Conv

module String = StringLabels

type encoding = Uutf.encoding

let sexp_of_encoding e =
  (match e with
   | `UTF_16 -> "UTF-16"
   | `UTF_16BE -> "UTF-16BE"
   | `UTF_16LE -> "UTF-16LE"
   | `UTF_8 -> "UTF-8")
  |> sexp_of_string

exception Malformed_unicode of
    { index : int
    ; str : string
    ; encoding : encoding }
[@@deriving sexp]

type t = string

let empty = ""

let length = String.length

let length_uint16 t =
  length t
  |> Uint16.of_int

let to_bytes t = t
let of_bytes t = t

let fold_bytes_le ~init ~f t =
  (* Why is there no String.fold_left..? *)
  let acc = ref init in
  String.iter t ~f:(fun c ->
    acc := f !acc c);
  !acc

let write_to_buffer buffer t =
  Buffer.add_string buffer t

let to_utf8_string t =
  let buffer = Buffer.create (length t) in
  Uutf.String.fold_utf_16le (fun () index -> function
    | `Malformed str ->
      raise (Malformed_unicode { encoding = `UTF_16BE ; index ; str })
    | `Uchar c ->
      Uutf.Buffer.add_utf_8 buffer c)
    () t;
  Buffer.contents buffer

let of_utf8_string s =
  let buffer = Buffer.create (String.length s * 2) in
  Uutf.String.fold_utf_8 (fun () index -> function
    | `Malformed str ->
      raise (Malformed_unicode { encoding = `UTF_8 ; index ; str })
    | `Uchar c ->
      Uutf.Buffer.add_utf_16le buffer c)
    () s;
  Buffer.contents buffer

let sexp_of_t t =
  to_utf8_string t
  |> sexp_of_string

let%test_module _ =
  (module struct

    let compare_string = compare

    let%test_unit "utf-8 round-trip" =
      let expect = "this is a test string" in
      expect
      |> of_utf8_string
      |> to_utf8_string
      |> [%test_result: string] ~expect

    let%test_unit "of_utf8" =
      let expect = "t\x00h\x00i\x00s\x00 \x00i\x00s\x00 \x00a\x00 \x00t\x00e\x00s\x00t\x00 \x00s\x00t\x00r\x00i\x00n\x00g\x00" in
      "this is a test string"
      |> of_utf8_string
      |> [%test_result: string] ~expect

  end)

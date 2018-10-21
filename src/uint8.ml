open Unsigned
include UInt8

module List = ListLabels

let sexp_of_t t =
  to_string t
  |> Sexplib.Conv.sexp_of_string

let of_uint16 n =
  UInt16.to_int n
  |> of_int

let of_int32 n =
  Signed.Int32.to_int n
  |> of_int

let of_uint32 n =
  UInt32.to_int64 n
  |> of_int64

let of_uint64 n =
  UInt64.to_int n
  |> of_int

let of_char c =
  Char.code c
  |> of_int

let to_char t =
  to_int t
  |> Char.unsafe_chr

let fold_bytes ~init ~f t =
  f init (to_char t)

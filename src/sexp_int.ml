open Unsigned
open Signed
open Sexplib.Conv

let sexp_of_int32 n =
  Int32.to_string n |> sexp_of_string

let sexp_of_uint64 n =
  UInt64.to_string n |> sexp_of_string

let sexp_of_uint8 n =
  UInt8.to_string n |> sexp_of_string

let sexp_of_uint16 n =
  UInt16.to_string n |> sexp_of_string

let sexp_of_uint32 n =
  UInt32.to_string n |> sexp_of_string

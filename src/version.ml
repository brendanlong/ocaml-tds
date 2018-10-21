open Unsigned
open Sexp_int

exception Unknown_tds_version of uint32
[@@deriving sexp_of]

type t =
  | V7_4
[@@deriving sexp_of]

let to_uint32 = function
  | V7_4 -> UInt32.of_int 0x74000004

let of_uint32 n =
  match UInt32.to_int n with
  | 0x74000004 -> V7_4
  | _ -> raise (Unknown_tds_version n)

let fold_bytes_le ~init ~f t =
  to_uint32 t
  |> Uint32.fold_bytes_le ~init ~f

open Unsigned
include UInt16

module List = ListLabels

let sexp_of_t t =
  to_string t
  |> Sexplib.Conv.sexp_of_string

let fold_bytes ~init ~f t order =
  order
  |> List.fold_left ~init ~f:(fun acc shift_bytes ->
    shift_right t (shift_bytes * 8)
    |> Uint8.of_uint16
    |> Uint8.fold_bytes ~f ~init:acc)

let fold_bytes_be ~init ~f t =
  [ 1 ; 0 ]
  |> fold_bytes ~init ~f t

let fold_bytes_le ~init ~f t =
  [ 0 ; 1 ]
  |> fold_bytes ~init ~f t

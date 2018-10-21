(** A UCS-16 encoded string in network byte order *)

type encoding = Uutf.encoding

val sexp_of_encoding : encoding -> Sexplib.Sexp.t

exception Malformed_unicode of
    { index : int
    ; str : string
    ; encoding : encoding }
[@@deriving sexp_of]

type t
[@@deriving sexp_of]

val empty : t
val to_bytes : t -> string
val of_bytes : string -> t
val length : t -> int
val length_uint16 : t -> Uint16.t

val fold_bytes_le : init:'a -> f:('a -> char -> 'a) -> t -> 'a
val write_to_buffer : Buffer.t -> t -> unit

val to_utf8_string : t -> string
val of_utf8_string : string -> t

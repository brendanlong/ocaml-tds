open Unsigned

(* FIXME: Make this load from the .opam file or something? *)
let name = "ocaml-tds"

let version = UInt32.one

let name_ucs16 = Ucs16_string.of_utf8_string name

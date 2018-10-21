(* See https://msdn.microsoft.com/en-us/library/dd304019.aspx *)
open Unsigned

module List = ListLabels
module String = StringLabels

type t =
  { tds_version : Version.t
  ; packet_size : uint32
  ; library_version : uint32
  ; process_id : uint32
  ; connection_id : uint32
  (* Note: We intentionally don't support the options to change byte order,
     character encoding to EBDIC, or float representation
     Other option flags should be supported at some point *)
  ; host_name : Ucs16_string.t
  ; user_name : Ucs16_string.t
  ; password : Ucs16_string.t
  ; library_name : Ucs16_string.t
  ; server_name : Ucs16_string.t
  ; initial_language : Ucs16_string.t option
  ; initial_database : Ucs16_string.t option }

let make_normal_login ?(tds_version=Version.V7_4) ?(packet_size=UInt32.max_int)
      ?(library_version=Config.version) ?(process_id=UInt32.zero)
      ~connection_id ~host_name ~user_name ~password
      ~server_name ?(library_name=Config.name_ucs16) ?initial_language
      ?initial_database () =
  { tds_version ; packet_size ; library_version ; process_id
  ; connection_id ; host_name ; user_name ; password
  ; server_name ; library_name ; initial_language ; initial_database }

let encode_password password =
  (* From the notes at the bottom of
     https://msdn.microsoft.com/en-us/library/dd304019.aspx:
     Before submitting a password from the client to the server, for every byte
     in the password buffer starting with the position pointed to by ibPassword
     or ibChangePassword, the client SHOULD first swap the four high bits with
     the four low bits and then do a bit-XOR with 0xA5 (10100101). *)
  Ucs16_string.to_bytes password
  |> String.map ~f:(fun c ->
    let open Uint8 in
    let open Uint8.Infix in
    let n = of_char c in
    (shift_left n 4 + shift_right n 4)
    |> logxor (of_int 0xA5)
    |> to_char)
  |> Ucs16_string.of_bytes

let fixed_length = 92

let dynamic_length t =
  [ t.host_name
  ; t.user_name
  ; t.password
  ; t.library_name
  ; t.server_name
  ; Option.value ~default:Ucs16_string.empty t.initial_language
  ; Option.value ~default:Ucs16_string.empty  t.initial_database ]
  |> List.fold_left ~init:0 ~f:(fun total str ->
    total + Ucs16_string.length str)

let total_length t =
  fixed_length + dynamic_length t

let fold_bytes ~init ~f t =
  let data_length = total_length t |> UInt16.of_int in
  let init =
    Packet_header.(make ~data_length ~packet_type:Type.Login
                   |> fold_bytes ~init ~f)
  in
  (* FIXME: Split packets that don't fit in one packet *)
  let init = Uint32.fold_bytes_le ~f ~init (Uint16.to_int data_length |> Uint32.of_int) in
  let init = Version.fold_bytes_le ~f ~init t.tds_version in
  let init = Uint32.fold_bytes_le ~f ~init t.packet_size in
  let init = Uint32.fold_bytes_le ~f ~init t.library_version in
  let init = Uint32.fold_bytes_le ~f ~init t.process_id in
  let init = Uint32.fold_bytes_le ~f ~init t.connection_id in
  (* OptionFlags1 *)
  let init = Uint8.fold_bytes ~f ~init Uint8.zero in
  (* OptionFlags2 *)
  let init = Uint8.fold_bytes ~f ~init Uint8.zero in
  (* TypeFlags *)
  let init = Uint8.fold_bytes ~f ~init Uint8.zero in
  (* OptionFlags3 *)
  let init = Uint8.fold_bytes ~f ~init Uint8.zero in
  (* Client timezone *)
  let init = Int32.fold_bytes_le ~f ~init Int32.zero in
  (* Client locale *)
  let init = Uint32.fold_bytes_le ~f ~init UInt32.zero in
  let strings =
    [ t.host_name
    ; t.user_name
    ; encode_password t.password
    ; t.library_name
    ; t.server_name
    ; Ucs16_string.empty (* ibUnused *)
    ; Ucs16_string.empty (* ibCltIntName *)
    ; Option.value ~default:Ucs16_string.empty t.initial_language
    ; Option.value ~default:Ucs16_string.empty t.initial_database ]
  in
  let init =
    strings
    |> List.fold_left ~init:(fixed_length, init) ~f:(fun (offset, init) str ->
      let length = Ucs16_string.length str in
      let init = Uint16.fold_bytes_le ~init ~f (Uint16.of_int offset) in
      let init = Uint16.fold_bytes_le ~init ~f (Uint16.of_int (length / 2))
      and offset = offset + Ucs16_string.length str in
      offset, init)
    |> snd
  in
  (* ClientID - 6 bytes
     ibSSPI - 4 bytes (len + offset)
     ibAtchDBFile - 4 bytes (len + offset)
     ibChangePassword - 4 bytes (len + offset)
     cbSSPILong - 4 bytes *)
  let init =
    List.init ~len:20 ~f:(fun _ -> '\x00')
    |> List.fold_left ~init ~f
  in
  strings
  |> List.fold_left ~init ~f:(fun init str ->
    Ucs16_string.fold_bytes_le ~init ~f str)

let to_buffer t =
  let buffer = Buffer.create (total_length t + Packet_header.length) in
  fold_bytes t ~init:buffer ~f:(fun buffer c ->
    Buffer.add_char buffer c;
    buffer)

let to_bytes t =
  to_buffer t
  |> Buffer.contents

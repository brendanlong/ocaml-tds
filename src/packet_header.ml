(* https://msdn.microsoft.com/en-us/library/dd340948.aspx *)
open Unsigned

module Type = struct
  type t =
    | Batch
    | Rpc
    | Tabular_result
    | Attention
    | Bulk_load_data
    | Federated_authentication_token
    | Transaction_manager_request
    | Login
    | Sspi
    | Pre_login
  [@@deriving sexp_of]

  let to_uint8 = function
    | Batch -> UInt8.one
    | Rpc -> UInt8.of_int 3
    | Tabular_result -> UInt8.of_int 4
    | Attention -> UInt8.of_int 6
    | Bulk_load_data -> UInt8.of_int 7
    | Federated_authentication_token -> UInt8.of_int 8
    | Transaction_manager_request -> UInt8.of_int 14
    | Login -> UInt8.of_int 16
    | Sspi -> UInt8.of_int 17
    | Pre_login -> UInt8.of_int 18
end

type t =
  { packet_type : Type.t
  ; status : uint8
  ; length : uint16
  ; server_process_id : uint16
  ; packet_id : uint8
  ; window : uint8 }

let length = 8

let make ~packet_type ~data_length =
  { packet_type ; length = UInt16.(Infix.(data_length + of_int 8))
  ; status = UInt8.one (* end of message *)
  ; server_process_id = UInt16.zero (* for debug purposes *)
  ; packet_id = UInt8.zero (* currently ignored by server *)
  ; window = UInt8.zero (* MUST be zero *) }

let fold_bytes ~init ~f t =
  let init = Uint8.fold_bytes ~f ~init (Type.to_uint8 t.packet_type) in
  let init = Uint8.fold_bytes ~f ~init  t.status in
  let init = Uint16.fold_bytes_be ~init ~f t.length in
  let init = Uint16.fold_bytes_be ~init ~f t.server_process_id in
  let init = Uint8.fold_bytes ~f ~init t.packet_id in
  Uint8.fold_bytes ~f ~init t.window

let to_buffer t =
  let buffer = Buffer.create length in
  fold_bytes t ~init:buffer ~f:(fun buffer byte ->
    Buffer.add_char buffer byte;
    buffer)

let to_string t =
  to_buffer t
  |> Buffer.contents

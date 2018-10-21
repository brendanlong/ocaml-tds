open Core
open Async
open Unsigned

let host_name = Unix.gethostname ()

let login server_name user_name password () =
  Host_and_port.create ~host:server_name ~port:1433
  |> Tcp.Where_to_connect.of_host_and_port
  |> Fn.flip Tcp.with_connection (fun _ _reader writer ->
    let server_name = Tds.Ucs16_string.of_utf8_string server_name
    and user_name = Tds.Ucs16_string.of_utf8_string user_name
    and password = Tds.Ucs16_string.of_utf8_string password
    and host_name = Tds.Ucs16_string.of_utf8_string host_name in
    Tds.Login7.make_normal_login ~server_name ~user_name ~password
      ~host_name ~connection_id:UInt32.one ()
    |> Tds.Login7.to_bytes
    |> Writer.write writer;
    Writer.flushed writer)

let () =
  Command.async_spec
    ~summary:"Examples for the ocaml-tds library"
    Command.Spec.(
      empty
      +> anon ("<server_name>" %: string)
      +> anon ("<user_name>" %: string)
      +> anon ("<password>" %: string)
    )
    login
  |> Command.run

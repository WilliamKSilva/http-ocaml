open Unix

type socket =
  | Socket of { addr : Unix.sockaddr; file_descr : Unix.file_descr }
  | Error of { error : error }

exception Generic of string

(*
  All this pattern matching is kind of pointless in this case (The exceptions are very clear and they are the raised in the right flow),
  I'am just doing to learn a bit
*)

let socket_create addr port : socket =
  try
    let file_descr = Unix.socket PF_INET SOCK_STREAM 0 in
    let address = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
    Socket { addr = address; file_descr }
  with Unix_error (error, _, _) -> ( match error with _ -> Error { error })

let listen socket =
  let () = print_string "Listening on socket" in
  match socket with
  | Error _ -> raise (Generic "Invalid socket provided")
  | Socket socket ->
      let () =
        let () = Unix.bind socket.file_descr socket.addr in
        Unix.listen socket.file_descr 1000
      in
      ()

let read_data socket =
  match socket with
  | Error _ -> raise (Generic "Invalid socket")
  | Socket socket -> (
      let io = Unix.accept socket.file_descr in
      let data : bytes = Bytes.create 2000 in
      let send = Unix.recv (fst io) data 0 2000 [] in
      match send with
      | 0 -> Unix.shutdown (fst io) SHUTDOWN_RECEIVE
      | _ ->
          let () = print_bytes data in
          let () = Unix.shutdown (fst io) SHUTDOWN_RECEIVE in
          ())

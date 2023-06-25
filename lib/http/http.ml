open Unix

type socket = { addr : Unix.sockaddr; file_descr : Unix.file_descr }

let socket_create addr port =
  let file_descr = Unix.socket PF_INET SOCK_STREAM 0 in
  let address = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  { addr = address; file_descr }

let listen socket =
  let () = print_string "Listening on socket" in
  let () = Unix.bind socket.file_descr socket.addr in
  let () = Unix.listen socket.file_descr 1000 in
  ()

let read_data socket =
  let io = Unix.accept socket.file_descr in
  let data : bytes = Bytes.create 2000 in
  let send = Unix.recv (fst io) data 0 2000 [] in
  if send = 0 then Unix.shutdown (fst io) SHUTDOWN_RECEIVE
  else
    let () = print_bytes data in
    Unix.shutdown (fst io) SHUTDOWN_RECEIVE

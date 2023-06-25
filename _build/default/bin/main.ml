let socket =
  let addr = "127.0.0.1" in
  let port = 3001 in
  Http.socket_create addr port

let () =
  while true do
    let () = Http.listen socket in
    Http.read_data socket
  done

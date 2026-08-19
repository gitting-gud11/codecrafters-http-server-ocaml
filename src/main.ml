open Unix

let () =
  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)

  (* Create a TCP server socket *)
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 4221));
  listen server_socket 1;


  let message="HTTP/1.1 200 OK\r\n\r\n" in
  let message_bytes=Bytes.of_string message in
  let num_bytes=Bytes.length message_bytes in

  let (client_socket, _) = accept server_socket in
  let _ =send client_socket message_bytes 0 num_bytes [] in
  close client_socket;
  close server_socket
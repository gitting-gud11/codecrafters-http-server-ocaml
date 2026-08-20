let ( let* ) = Option.bind
let server_buffer_length = 8192
let crlf = "\r\n"

module StringSet = Set.Make (String)

let supported_http_methods = StringSet.of_list [ "GET"; "POST" ]
let supported_http_versions = StringSet.singleton "HTTP/1.1"

type http_request_line = {
  http_method : string;
  path : string;
  version : string;
}

type http_request = {
  request_line : http_request_line;
  headers : (string * string) list; (*Consider modifying to an array, or a map*)
  request_body : string;
}

let parse_http_request_line request_line =
  match String.split_on_char ' ' request_line with
  | [ request_method; request_path; request_version ] ->
      Some
        {
          http_method = request_method;
          path = request_path;
          version = request_version;
        }
  | _ -> None

let parse_http_headers headers_segment =
  let headers_list =
    String.split_all ~sep:crlf ~drop:String.is_empty headers_segment
  in
  let split_header_fields_fn = String.split_on_char ':' in
  let split_headers_list = List.map split_header_fields_fn headers_list in
  let attempt_to_build_header header_tokens =
    match header_tokens with
    | [ header_name; header_value ] -> Some (header_name, header_value)
    | _ -> None
  in
  let headers_option_list =
    List.map attempt_to_build_header split_headers_list
  in
  if List.exists Option.is_none headers_option_list then None
  else
    let built_header_list = List.map Option.get headers_option_list in
    let formatted_header_list =
      List.map
        (fun (name, value) -> (name, String.trim value ^ crlf))
        built_header_list
    in
    Some formatted_header_list

let parse_http_request client_message =
  let* client_request_line, suffix =
    String.split_first ~sep:crlf client_message
  in
  let* parsed_request_line = parse_http_request_line client_request_line in
  let* headers_segment, client_request_body =
    String.split_last ~sep:(crlf ^ crlf) client_message
  in
  let* parsed_http_headers = parse_http_headers headers_segment in
  Some
    {
      request_line = parsed_request_line;
      headers = parsed_http_headers;
      request_body = client_request_body;
    }

let communicate_with_client client_socket response_message =
  let message_bytes = Bytes.of_string response_message in
  let _ =
    Unix.send client_socket message_bytes 0 (Bytes.length message_bytes) []
  in
  ()

let communicate_error_404 =
 fun client_socket ->
  communicate_with_client client_socket "HTTP/1.1 404 Not Found\r\n\r\n"

let communicate_200_ok =
 fun client_socket ->
  communicate_with_client client_socket "HTTP/1.1 200 OK\r\n\r\n"

let respond_to_client client_socket client_message =
  match parse_http_request client_message with
  | None -> communicate_error_404 client_socket
  | Some client_http_request -> ()

let main () =
  (* Create a TCP server socket *)
  let server_socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt server_socket SO_REUSEADDR true;
  Unix.bind server_socket
    (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 4221));
  Unix.listen server_socket 1;

  let server_read_buffer = Bytes.create server_buffer_length in

  let message = "HTTP/1.1 200 OK\r\n\r\n" in
  let message_bytes = Bytes.of_string message in
  let num_bytes = Bytes.length message_bytes in

  let client_socket, _ = Unix.accept server_socket in
  let _ = Unix.send client_socket message_bytes 0 num_bytes [] in
  let bytes_read =
    Unix.recv client_socket server_read_buffer 0 server_buffer_length []
  in
  if bytes_read > 0 then
    respond_to_client client_socket
      (Bytes.sub_string server_read_buffer 0 bytes_read)
  else ();
  Unix.close client_socket;
  Unix.close server_socket

let () = main ()

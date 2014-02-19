
type t = {
  message_buffer: string;
  length_buffer: string;
  mutable len: int;
}

let make () =
  let message_buffer_length = 1024 in
  let length_buffer_length = 2 in
  let message_buffer = String.make message_buffer_length '\000' in
  let length_buffer = String.make length_buffer_length '\000' in
  let len = 0 in
  { message_buffer; length_buffer; len }

let marshal t x =
  let len = Marshal.to_buffer t.message_buffer 0 (String.length t.message_buffer) x [ Marshal.Closures ] in
  t.length_buffer.[0] <- char_of_int (len / 256);
  t.length_buffer.[1] <- char_of_int (len mod 256);
  t.len <- len

let unmarshal t =
  Marshal.from_string t.message_buffer 0

module type ENV = sig
  type 'a t
  type channel
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val fail: exn -> 'a t
  val write: channel -> string -> int -> int -> int t
  val read: channel -> string -> int -> int -> int t
end

module Channel(ENV: ENV) = struct
  open ENV
  let complete op fd buf ofs len =
    let rec loop acc fd buf ofs len =
      op fd buf ofs len >>= fun n ->
      let len' = len - n in
      let acc' = acc + n in
      if len' = 0 || n = 0
      then return acc'
      else loop acc' fd buf (ofs + n) len' in
    loop 0 fd buf ofs len >>= fun n ->
    if n = 0 && len <> 0
    then fail End_of_file
    else return ()

  let send t fd =
    complete write fd t.length_buffer 0 (String.length t.length_buffer)
    >>= fun () ->
    complete write fd t.message_buffer 0 t.len

  let recv t fd =
    complete read fd t.length_buffer 0 (String.length t.length_buffer)
    >>= fun () ->
    let len = (int_of_char t.length_buffer.[0]) * 256 + (int_of_char t.length_buffer.[1]) in
    complete read fd t.message_buffer 0 len
end
  
module Unix_env = struct
  type 'a t = 'a
  type channel = Unix.file_descr
  let ( >>= ) m f = f m
  let return x = x
  let fail = raise
  let read = Unix.read
  let write = Unix.write
end

module Unix = Channel(Unix_env)

module Lwt_unix_env = struct
  type 'a t = 'a Lwt.t
  type channel = Lwt_unix.file_descr
  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return
  let fail = Lwt.fail
  let read = Lwt_unix.read
  let write = Lwt_unix.write
end

module Lwt_unix = Channel(Lwt_unix_env)


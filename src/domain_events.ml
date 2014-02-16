(* Simple demo program showing how to receive domain events.
   Usage: domain_events [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   (C) Copyright 2013 Citrix Inc
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module E = Libvirt.Event
module N = Libvirt.Network

module Any = struct
  type t =
  | Lifecycle of E.Lifecycle.t
  | Reboot of E.Reboot.t
  | RtcChange of E.Rtc_change.t
  | Watchdog of E.Watchdog.t
  | IOError of E.Io_error.t
  | Graphics of E.Graphics.t
  | IOErrorReason of E.Io_error.t
  | ControlError of E.Control_error.t
  | BlockJob of E.Block_job.t
  | DiskChange of E.Disk_change.t
  | TrayChange of E.Tray_change.t
  | PMWakeUp of E.PM_wakeup.t
  | PMSuspend of E.PM_suspend.t
  | BalloonChange of E.Balloon_change.t
  | PMSuspendDisk of E.PM_suspend_disk.t

  let to_string = function
  | Lifecycle x -> E.Lifecycle.to_string x
  | Reboot x -> E.Reboot.to_string x
  | RtcChange x -> E.Rtc_change.to_string x
  | Watchdog x -> E.Watchdog.to_string x
  | IOError x -> E.Io_error.to_string x
  | Graphics x -> E.Graphics.to_string x
  | IOErrorReason x -> E.Io_error.to_string x
  | ControlError x -> E.Control_error.to_string x
  | BlockJob x -> E.Block_job.to_string x
  | DiskChange x -> E.Disk_change.to_string x
  | TrayChange x -> E.Tray_change.to_string x
  | PMWakeUp x -> E.PM_wakeup.to_string x
  | PMSuspend x -> E.PM_suspend.to_string x
  | BalloonChange x -> E.Balloon_change.to_string x
  | PMSuspendDisk x -> E.PM_suspend_disk.to_string x

end

let string_of_state = function
  | D.InfoNoState -> "no state"
  | D.InfoRunning -> "running"
  | D.InfoBlocked -> "blocked"
  | D.InfoPaused -> "paused"
  | D.InfoShutdown -> "shutdown"
  | D.InfoShutoff -> "shutoff"
  | D.InfoCrashed -> "crashed"

let printd dom fmt =
  let prefix dom =
    let id = D.get_id dom in
    try
      let name = D.get_name dom in
      let info = D.get_info dom in
      let state = string_of_state info.D.state in
      sprintf "%8d %-20s %s " id name state
  with _ ->
      sprintf "%8d " id in
  let write x =
    output_string stdout (prefix dom);
    output_string stdout x;
    output_string stdout "\n";
    flush stdout in
  Printf.ksprintf write fmt

let string_option = function
  | None -> "None"
  | Some x -> "Some " ^ x

let string_of_graphics_address (family, node, service) =
  Printf.sprintf "{ family=%d; node=%s; service=%s }" family (string_option node) (string_option service)

let string_of_graphics_subject_identity (ty, name) =
  Printf.sprintf "{ type=%s; name=%s }" (string_option ty) (string_option name)

let string_of_graphics_subject xs = String.concat "; " (List.map string_of_graphics_subject_identity (Array.to_list xs))

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let with_all_events name f =
  try
    E.register_default_impl ();
    let conn = C.connect_readonly ?name () in

    let spinner = [| '|'; '/'; '-'; '\\' |] in

    let timeouts = ref 0 in
    (* Check add/remove works *)
    let id = E.add_timeout conn 250 (fun () -> Printf.printf "This callback is immediately deregistered\n%!") in
    E.remove_timeout conn id;

    let (_: E.timer_id) = E.add_timeout conn 250 (* ms *)
        (fun () ->
            incr timeouts;
            Printf.printf "\r%c  %d timeout callbacks%!" (spinner.(!timeouts mod (Array.length spinner))) !timeouts;
            (* Check for GC errors: *)
            Gc.compact ()
        ) in

    let open Any in

    let (_: E.callback_id) = E.register_any conn (E.Lifecycle (fun dom e ->
        printd dom "Lifecycle %s" (E.Lifecycle.to_string e);
        f dom (Lifecycle e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Reboot (fun dom e ->
        printd dom "Reboot %s" (E.Reboot.to_string e);
        f dom (Reboot e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.RtcChange (fun dom e ->
        printd dom "RtcChange %s" (E.Rtc_change.to_string e);
        f dom (RtcChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Watchdog (fun dom e ->
        printd dom "Watchdog %s" (E.Watchdog.to_string e);
        f dom (Watchdog e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOError (fun dom e ->
        printd dom "IOError %s" (E.Io_error.to_string e);
        f dom (IOError e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOErrorReason (fun dom e ->
        printd dom "IOErrorReason %s" (E.Io_error.to_string e);
        f dom (IOErrorReason e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Graphics (fun dom e ->
        printd dom "Graphics %s" (E.Graphics.to_string e);
        f dom (Graphics e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.ControlError (fun dom e ->
        printd dom "ControlError %s" (E.Control_error.to_string e);
        f dom (ControlError e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BlockJob (fun dom e ->
        printd dom "BlockJob %s" (E.Block_job.to_string e);
        f dom (BlockJob e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.DiskChange (fun dom e ->
        printd dom "DiskChange %s" (E.Disk_change.to_string e);
        f dom (DiskChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.TrayChange (fun dom e ->
        printd dom "TrayChange %s" (E.Tray_change.to_string e);
        f dom (TrayChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMWakeUp (fun dom e ->
        printd dom "PMWakeup %s" (E.PM_wakeup.to_string e);
        f dom (PMWakeUp e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspend (fun dom e ->
        printd dom "PMSuspend %s" (E.PM_suspend.to_string e);
        f dom (PMSuspend e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BalloonChange (fun dom e ->
        printd dom "BalloonChange %s" (E.Balloon_change.to_string e);
        f dom (BalloonChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspendDisk (fun dom e ->
        printd dom "PMSuspendDisk %s" (E.PM_suspend_disk.to_string e);
        f dom (PMSuspendDisk e)
    )) in
    C.set_keep_alive conn 5 3;
    while true do
	E.run_default_impl ()
    done
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let event_buffer_length = 1024
let length_buffer_length = 2

let open_events () =
  let reader, writer = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then begin
    Unix.close reader;
    let buf = String.make event_buffer_length '\000' in
    let len_buf = String.make length_buffer_length '\000' in
    with_all_events None (fun d e ->
      let len = Marshal.to_buffer buf 0 event_buffer_length (d, e) [] in
      len_buf.[0] <- char_of_int (len / 256);
      len_buf.[1] <- char_of_int (len mod 256);
      let n = Unix.write writer len_buf 0 length_buffer_length in
      if n <> length_buffer_length then failwith "event pipe: short write";
      let n = Unix.write writer buf 0 len in
      if n <> len then failwith "event pipe: short write"
    );
    exit 1
  end;
  Unix.close writer;
  reader

open Lwt

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

let read_events reader =
  let fd = Lwt_unix.of_unix_file_descr reader in
  let buf = String.make event_buffer_length '\000' in
  let len_buf = String.make length_buffer_length '\000' in
  let rec loop () =
    complete Lwt_unix.read fd len_buf 0 length_buffer_length >>= fun () ->
    let len = (int_of_char len_buf.[0]) * 256 + (int_of_char len_buf.[1]) in
    complete Lwt_unix.read fd buf 0 len >>= fun () ->
    let d, e = Marshal.from_string buf 0 in
    loop () in
  loop ()

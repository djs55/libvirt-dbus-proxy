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

open Event

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

    List.iter
      (fun (dom, info) -> f dom (Lifecycle (`Started `Booted)))
      (Domain.get_domains_and_infos conn [Domain.ListActive]);
    List.iter
      (fun (dom, info) -> f dom (Lifecycle (`Stopped `Shutdown)))
      (Domain.get_domains_and_infos conn [Domain.ListInactive]);

    let (_: E.callback_id) = E.register_any conn (E.Lifecycle (fun dom e ->
        f dom (Lifecycle e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Reboot (fun dom e ->
        f dom (Reboot e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.RtcChange (fun dom e ->
        f dom (RtcChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Watchdog (fun dom e ->
        f dom (Watchdog e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOError (fun dom e ->
        f dom (IOError e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.IOErrorReason (fun dom e ->
        f dom (IOErrorReason e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.Graphics (fun dom e ->
        f dom (Graphics e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.ControlError (fun dom e ->
        f dom (ControlError e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BlockJob (fun dom e ->
        f dom (BlockJob e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.DiskChange (fun dom e ->
        f dom (DiskChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.TrayChange (fun dom e ->
        f dom (TrayChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMWakeUp (fun dom e ->
        f dom (PMWakeUp e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspend (fun dom e ->
        f dom (PMSuspend e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.BalloonChange (fun dom e ->
        f dom (BalloonChange e)
    )) in
    let (_: E.callback_id) = E.register_any conn (E.PMSuspendDisk (fun dom e ->
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

type event = {
  id: int;
  state: Domain.state option;
  name: string option;
  payload: Event.Any.t;
}

let open_events name =
  let reader, writer = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then begin
    Unix.close reader;
    let buf = String.make event_buffer_length '\000' in
    let len_buf = String.make length_buffer_length '\000' in
    with_all_events name (fun d e ->
      let id = D.get_id d in
      let name, state =
        try
          let name = D.get_name d in
          let info = D.get_info d in
          Some name, Some info.D.state
        with _ ->
          None, None in
      let event = { id; state; name; payload = e } in
      let len = Marshal.to_buffer buf 0 event_buffer_length event [] in
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

let read_events reader f =
  let fd = Lwt_unix.of_unix_file_descr reader in
  let buf = String.make event_buffer_length '\000' in
  let len_buf = String.make length_buffer_length '\000' in
  let rec loop () =
    complete Lwt_unix.read fd len_buf 0 length_buffer_length >>= fun () ->
    let len = (int_of_char len_buf.[0]) * 256 + (int_of_char len_buf.[1]) in
    complete Lwt_unix.read fd buf 0 len >>= fun () ->
    let event : event = Marshal.from_string buf 0 in
    Lwt_io.fprintlf Lwt_io.stdout "%d %s" event.id (Sexplib.Sexp.to_string_hum (Event.Any.sexp_of_t event.payload)) >>= fun () ->
    f event >>= fun () ->
    loop () in
  loop ()

let vm_bus_name = "org.xenserver.vm1"

let domainManager_path = [ "org"; "xenserver"; "DomainManager" ]

let vm_path = [ "org"; "xenserver"; "vm" ]

let domain_CreateXML xml flags =
  fail (Failure "domain_CreateXML")

let domain_Create obj =
  fail (Failure "domain_Create")

let domain_Destroy obj =
  fail (Failure "domain_Destroy")

let domain_Shutdown obj =
  fail (Failure "domain_Shutdown")

open Vm

let domainManager_intf = Org_libvirt_DomainManager1.(make {
  m_CreateXML = (fun obj (xml, flags) -> domain_CreateXML xml flags);
})

let domain_intf = Org_libvirt_Domain1.(make {
    m_Create = (fun obj () -> domain_Create obj);
    m_Destroy = (fun obj () -> domain_Destroy obj);
    m_Shutdown = (fun obj () -> domain_Shutdown obj);
  })

let export_dbus_objects reader =
  OBus_bus.session () >>= fun bus ->
  OBus_bus.request_name bus vm_bus_name >>= fun _ ->
  let obj = OBus_object.make ~interfaces:[domainManager_intf] domainManager_path in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  read_events reader
    (fun event -> match event.name with
    | None -> return ()
    | Some name ->
      let obj = OBus_object.make ~interfaces:[domain_intf] (vm_path @ [ name ]) in
      OBus_object.attach obj ();
      OBus_object.export bus obj;
      return ()
    )

let _ =
  let reader = open_events (Some "qemu:///system") in
  Lwt_main.run (export_dbus_objects reader)

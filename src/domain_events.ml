(* Simple demo program showing how to receive domain events.
   Usage: domain_events [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   (C) Copyright 2013 Citrix Inc
   http://libvirt.org/
 *)

open Printf

module C = Libvirt_connect
module E = struct
  include Libvirt.Event
  include Libvirt_event
end
module N = Libvirt.Network
module D = Libvirt_domain

let with_all_events f =
  try
    E.register_default_impl ();
    let conn = C.const (C.connect ()) in

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

    let open E.Any in

    List.iter
      (fun (dom, info) -> f dom (Lifecycle (`Started `Booted)))
      (D.get_domains_and_infos conn [D.ListActive]);
    List.iter
      (fun (dom, info) -> f dom (Lifecycle (`Stopped `Shutdown)))
      (D.get_domains_and_infos conn [D.ListInactive]);

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

type event = {
  id: int;
  state: D.state option;
  name: string option;
  payload: E.Any.t;
}

let open_events () =
  let reader, writer = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then begin
    Unix.close reader;
    let buffer = IO.make () in
    with_all_events (fun d e ->
      let id = D.get_id d in
      let name, state =
        try
          let name = D.get_name d in
          let info = D.get_info d in
          Some name, Some info.D.state
        with _ ->
          None, None in
      let event = { id; state; name; payload = e } in
      IO.marshal buffer event;
      IO.Unix.send buffer writer;
    );
    exit 1
  end;
  Unix.close writer;
  reader

open Lwt

type vm = {
  uuid: string Lwt_react.S.t;
  set_uuid: string -> unit;
  id: int Lwt_react.S.t;
  set_id: int -> unit;
  name: string Lwt_react.S.t;
  set_name: string -> unit;
}

let make id uuid name =
  let uuid, set_uuid = React.S.create uuid in
  let id, set_id = React.S.create id in
  let name, set_name = React.S.create name in
  { uuid; set_uuid; id; set_id; name; set_name }

module StringMap = Map.Make(String)

let vms = ref StringMap.empty

let update_signals name =
  let c = C.connect () in
  let d = D.lookup_by_name c name in
  let id = D.get_id d in
  let name = D.get_name d in
  let uuid = D.get_uuid_string d in
  let vm =
    if StringMap.mem name !vms
    then StringMap.find name !vms
    else
      let vm = make id uuid name in
      vms := StringMap.add name vm !vms;
      vm in
  vm.set_id id;
  vm.set_uuid uuid;
  vm.set_name name

let read_events reader f =
  let fd = Lwt_unix.of_unix_file_descr reader in
  let buffer = IO.make () in
  let rec loop () =
    IO.Lwt_unix.recv buffer fd >>= fun () ->
    let event : event = IO.unmarshal buffer in
    Lwt_io.fprintlf Lwt_io.stdout "%d %s" event.id (Sexplib.Sexp.to_string_hum (E.Any.sexp_of_t event.payload)) >>= fun () ->
    f event >>= fun () ->
    loop () in
  loop ()

let vm_bus_name = "org.xenserver.vm1"

let domainManager_path = [ "org"; "xenserver"; "DomainManager" ]

let vm_path = [ "org"; "xenserver"; "vm" ]

let domain_CreateXML xml flags =
  fail (Failure "domain_CreateXML")

let name_of_obj obj =
  let path = OBus_object.path obj in
  List.hd (List.rev path)

let vm_of_obj obj =
  let name = name_of_obj obj in
  if not(StringMap.mem name !vms) then begin
    eprintf "FATAL: vm %s not in signal map" name;
    failwith (Printf.sprintf "vm_of_obj %s" name);
  end;
  StringMap.find name !vms

let operate_on_domain obj f =
  let name = name_of_obj obj in
  Lwt_preemptive.detach
    (fun () ->
      try
        let c = C.connect () in
        let d = D.lookup_by_name c name in
        f d
      with Libvirt.Virterror err ->
        eprintf "error: %s\n" (Libvirt.Virterror.to_string err);
        failwith (Libvirt.Virterror.to_string err)
    ) ()

let domain_Create obj = operate_on_domain obj D.create
let domain_Destroy obj = operate_on_domain obj D.destroy
let domain_Shutdown obj = operate_on_domain obj D.shutdown
let domain_Reboot obj = operate_on_domain obj D.reboot
let domain_id obj = operate_on_domain obj D.get_id

open Vm

let domainManager_intf = Org_libvirt_DomainManager1.(make {
  m_CreateXML = (fun obj (xml, flags) -> domain_CreateXML xml flags);
})

let domain_intf = Org_libvirt_Domain1.(make {
    m_Create = (fun obj () -> domain_Create obj);
    m_Destroy = (fun obj () -> domain_Destroy obj);
    m_Shutdown = (fun obj () -> domain_Shutdown obj);
    m_Reboot = (fun obj () -> domain_Reboot obj);
    p_id = (fun obj -> Lwt_react.S.bind (vm_of_obj obj).id (fun x -> Lwt_react.S.return (Int32.of_int x)));
    p_name = (fun obj -> (vm_of_obj obj).name);
    p_uuid = (fun obj -> (vm_of_obj obj).uuid);
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
      Lwt_preemptive.detach update_signals name >>= fun () ->
      let obj = OBus_object.make ~interfaces:[domain_intf] (vm_path @ [ name ]) in
      OBus_object.attach obj ();
      OBus_object.export bus obj;
      return ()
    )

let _ =
  let reader = open_events () in
  Lwt_main.run (export_dbus_objects reader)

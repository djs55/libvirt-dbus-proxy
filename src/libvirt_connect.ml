(* Simple demo program showing how to receive domain events.
   Usage: domain_events [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   (C) Copyright 2013 Citrix Inc
   http://libvirt.org/
 *)

include Libvirt.Connect

let name = ref ""

let with_lock m f =
  Mutex.lock m;
  try
    let result = f () in
    Mutex.unlock m;
    result
  with e ->
    Mutex.unlock m;
    raise e

let connect =
  let cache = ref None in
  let m = Mutex.create () in
  fun () ->
    with_lock m
      (fun () ->
        match !cache with
        | Some x -> x
        | None ->
          let c = connect ~name:(!name) () in
          cache := Some c;
          c
      )

let with_connection f =
  Lwt_preemptive.detach
    (fun () ->
      try
        let c = connect () in
        f c
      with Libvirt.Virterror err ->
        Printf.eprintf "error: %s\n" (Libvirt.Virterror.to_string err);
        failwith (Libvirt.Virterror.to_string err)
    ) ()


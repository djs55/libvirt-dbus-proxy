(* This file should be autogenerated somehow: the aim is to generate
   the to/from sexp functions as if we had typed 'with sexp' in
   Libvirt.Event.* *)

open Sexplib.Std

module Mixin_sexplib = struct
  module Defined = struct
    type t = [
    | `Added
    | `Updated
    | `Unknown of int
    ] with sexp
  end
  module Undefined = struct
    type t = [
    | `Removed
    | `Unknown of int
    ] with sexp
  end
  module Started = struct
    type t = [
    | `Booted
    | `Migrated
    | `Restored
    | `FromSnapshot
    | `Wakeup
    | `Unknown of int
    ] with sexp
  end
  module Suspended = struct
    type t = [
    | `Paused
    | `Migrated
    | `IOError
    | `Watchdog
    | `Restored
    | `FromSnapshot
    | `APIError
    | `Unknown of int (* newer libvirt *)
    ] with sexp
  end
  module Resumed = struct
    type t = [
    | `Unpaused
    | `Migrated
    | `FromSnapshot
    | `Unknown of int (* newer libvirt *)
    ] with sexp
  end
  module Stopped = struct
    type t = [
    | `Shutdown
    | `Destroyed
    | `Crashed
    | `Migrated
    | `Saved
    | `Failed
    | `FromSnapshot
    | `Unknown of int
    ] with sexp
  end
  module PM_suspended = struct
    type t = [
    | `Memory
    | `Disk
    | `Unknown of int (* newer libvirt *)
    ] with sexp
  end
  module Lifecycle = struct
    type t = [
    | `Defined of Defined.t
    | `Undefined of Undefined.t
    | `Started of Started.t
    | `Suspended of Suspended.t
    | `Resumed of Resumed.t
    | `Stopped of Stopped.t
    | `Shutdown (* no detail defined yet *)
    | `PMSuspended of PM_suspended.t
    | `Unknown of int (* newer libvirt *)
    ] with sexp
  end
  module Reboot = struct
    type t = unit with sexp
  end
  module Rtc_change = struct
    type t = int64 with sexp
  end
  module Watchdog = struct
    type t = [
    | `None
    | `Pause
    | `Reset
    | `Poweroff
    | `Shutdown
    | `Debug
    | `Unknown of int
    ] with sexp
  end
  module Io_error = struct
    type action = [
    | `None
    | `Pause
    | `Report
    | `Unknown of int (* newer libvirt *)
    ] with sexp
    open Libvirt.Event.Io_error

let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let src_path_field = ref None and dev_alias_field = ref None
        and action_field = ref None and reason_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "src_path" ->
                     (match !src_path_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in src_path_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "dev_alias" ->
                     (match !dev_alias_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in dev_alias_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "action" ->
                     (match !action_field with
                      | None ->
                          let fvalue = action_of_sexp _field_sexp
                          in action_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "reason" ->
                     (match !reason_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in reason_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!src_path_field), (!dev_alias_field),
                       (!action_field), (!reason_field))
                with
                | (Some src_path_value, Some dev_alias_value,
                   Some action_value, Some reason_value) ->
                    {
                      src_path = src_path_value;
                      dev_alias = dev_alias_value;
                      action = action_value;
                      reason = reason_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !src_path_field None), "src_path");
                        ((Pervasives.( = ) !dev_alias_field None),
                         "dev_alias");
                        ((Pervasives.( = ) !action_field None), "action");
                        ((Pervasives.( = ) !reason_field None), "reason") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp

    let sexp_of_t : t -> Sexplib.Sexp.t =
      fun {
        src_path = v_src_path;
        dev_alias = v_dev_alias;
        action = v_action;
        reason = v_reason
      } ->
      let bnds = [] in
      let arg = sexp_of_option sexp_of_string v_reason in
      let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "reason"; arg ] in
      let bnds = bnd :: bnds in
      let arg = sexp_of_action v_action in
      let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "action"; arg ] in
      let bnds = bnd :: bnds in
      let arg = sexp_of_option sexp_of_string v_dev_alias in
      let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "dev_alias"; arg ] in
      let bnds = bnd :: bnds in
      let arg = sexp_of_option sexp_of_string v_src_path in
      let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "src_path"; arg ] in
      let bnds = bnd :: bnds in Sexplib.Sexp.List bnds

  end
  module Graphics_address = struct
    type family = [
    | `Ipv4
    | `Ipv6
    | `Unix
    | `Unknown of int (* newer libvirt *)
    ] with sexp

    open Libvirt.Event.Graphics_address
let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let family_field = ref None and node_field = ref None
        and service_field = ref None and duplicates = ref []
        and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "family" ->
                     (match !family_field with
                      | None ->
                          let fvalue = family_of_sexp _field_sexp
                          in family_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "node" ->
                     (match !node_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in node_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "service" ->
                     (match !service_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in service_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!family_field), (!node_field), (!service_field)) with
                | (Some family_value, Some node_value, Some service_value) ->
                    {
                      family = family_value;
                      node = node_value;
                      service = service_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !family_field None), "family");
                        ((Pervasives.( = ) !node_field None), "node");
                        ((Pervasives.( = ) !service_field None), "service") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp
  
let _ = t_of_sexp
  
let sexp_of_t : t -> Sexplib.Sexp.t =
  fun { family = v_family; node = v_node; service = v_service } ->
    let bnds = [] in
    let arg = sexp_of_option sexp_of_string v_service in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "service"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_node in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "node"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_family v_family in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "family"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds

  end
  module Graphics_subject = struct
    open Libvirt.Event.Graphics_subject

let identity_of_sexp : Sexplib.Sexp.t -> identity =
  let _tp_loc = "foo.ml.identity"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let ty_field = ref None and name_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "ty" ->
                     (match !ty_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in ty_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "name" ->
                     (match !name_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in name_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!ty_field), (!name_field)) with
                | (Some ty_value, Some name_value) ->
                    { ty = ty_value; name = name_value; }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !ty_field None), "ty");
                        ((Pervasives.( = ) !name_field None), "name") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let sexp_of_identity : identity -> Sexplib.Sexp.t =
  fun { ty = v_ty; name = v_name } ->
    let bnds = [] in
    let arg = sexp_of_option sexp_of_string v_name in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "name"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_ty in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "ty"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds

    type t = identity list with sexp
  end
  module Graphics = struct
    type phase = [
    | `Connect
    | `Initialize
    | `Disconnect
    | `Unknown of int (** newer libvirt *)
    ] with sexp

    open Libvirt.Event.Graphics
let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let phase_field = ref None and local_field = ref None
        and remote_field = ref None and auth_scheme_field = ref None
        and subject_field = ref None and duplicates = ref []
        and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "phase" ->
                     (match !phase_field with
                      | None ->
                          let fvalue = phase_of_sexp _field_sexp
                          in phase_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "local" ->
                     (match !local_field with
                      | None ->
                          let fvalue = Graphics_address.t_of_sexp _field_sexp
                          in local_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "remote" ->
                     (match !remote_field with
                      | None ->
                          let fvalue = Graphics_address.t_of_sexp _field_sexp
                          in remote_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "auth_scheme" ->
                     (match !auth_scheme_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in auth_scheme_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "subject" ->
                     (match !subject_field with
                      | None ->
                          let fvalue = Graphics_subject.t_of_sexp _field_sexp
                          in subject_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!phase_field), (!local_field), (!remote_field),
                       (!auth_scheme_field), (!subject_field))
                with
                | (Some phase_value, Some local_value, Some remote_value,
                   Some auth_scheme_value, Some subject_value) ->
                    {
                      phase = phase_value;
                      local = local_value;
                      remote = remote_value;
                      auth_scheme = auth_scheme_value;
                      subject = subject_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !phase_field None), "phase");
                        ((Pervasives.( = ) !local_field None), "local");
                        ((Pervasives.( = ) !remote_field None), "remote");
                        ((Pervasives.( = ) !auth_scheme_field None),
                         "auth_scheme");
                        ((Pervasives.( = ) !subject_field None), "subject") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp
let sexp_of_t : t -> Sexplib.Sexp.t =
  fun
    {
      phase = v_phase;
      local = v_local;
      remote = v_remote;
      auth_scheme = v_auth_scheme;
      subject = v_subject
    } ->
    let bnds = [] in
    let arg = Graphics_subject.sexp_of_t v_subject in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "subject"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_auth_scheme in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "auth_scheme"; arg ] in
    let bnds = bnd :: bnds in
    let arg = Graphics_address.sexp_of_t v_remote in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "remote"; arg ] in
    let bnds = bnd :: bnds in
    let arg = Graphics_address.sexp_of_t v_local in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "local"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_phase v_phase in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "phase"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds
  end
  module Control_error = struct
    type t = unit with sexp
  end
  module Block_job = struct
    type ty = [
    | `KnownUnknown
    | `Pull
    | `Copy
    | `Commit
    | `Unknown of int (* newer libvirt *)
    ] with sexp
    type status = [
    | `Completed
    | `Failed
    | `Cancelled
    | `Ready
    | `Unknown of int
    ] with sexp

    open Libvirt.Event.Block_job

let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let disk_field = ref None and ty_field = ref None
        and status_field = ref None and duplicates = ref []
        and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "disk" ->
                     (match !disk_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in disk_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "ty" ->
                     (match !ty_field with
                      | None ->
                          let fvalue = ty_of_sexp _field_sexp
                          in ty_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "status" ->
                     (match !status_field with
                      | None ->
                          let fvalue = status_of_sexp _field_sexp
                          in status_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!disk_field), (!ty_field), (!status_field)) with
                | (Some disk_value, Some ty_value, Some status_value) ->
                    {
                      disk = disk_value;
                      ty = ty_value;
                      status = status_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !disk_field None), "disk");
                        ((Pervasives.( = ) !ty_field None), "ty");
                        ((Pervasives.( = ) !status_field None), "status") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp

let sexp_of_t : t -> Sexplib.Sexp.t =
  fun { disk = v_disk; ty = v_ty; status = v_status } ->
    let bnds = [] in
    let arg = sexp_of_status v_status in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "status"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_ty v_ty in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "ty"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_disk in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "disk"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds
  end
  module Disk_change = struct
    type reason = [
    | `MissingOnStart
    | `Unknown of int
    ] with sexp

    open Libvirt.Event.Disk_change

let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let old_src_path_field = ref None and new_src_path_field = ref None
        and dev_alias_field = ref None and reason_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "old_src_path" ->
                     (match !old_src_path_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in old_src_path_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "new_src_path" ->
                     (match !new_src_path_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in new_src_path_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "dev_alias" ->
                     (match !dev_alias_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in dev_alias_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "reason" ->
                     (match !reason_field with
                      | None ->
                          let fvalue = reason_of_sexp _field_sexp
                          in reason_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!old_src_path_field), (!new_src_path_field),
                       (!dev_alias_field), (!reason_field))
                with
                | (Some old_src_path_value, Some new_src_path_value,
                   Some dev_alias_value, Some reason_value) ->
                    {
                      old_src_path = old_src_path_value;
                      new_src_path = new_src_path_value;
                      dev_alias = dev_alias_value;
                      reason = reason_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !old_src_path_field None),
                         "old_src_path");
                        ((Pervasives.( = ) !new_src_path_field None),
                         "new_src_path");
                        ((Pervasives.( = ) !dev_alias_field None),
                         "dev_alias");
                        ((Pervasives.( = ) !reason_field None), "reason") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp
let sexp_of_t : t -> Sexplib.Sexp.t =
  fun
    {
      old_src_path = v_old_src_path;
      new_src_path = v_new_src_path;
      dev_alias = v_dev_alias;
      reason = v_reason
    } ->
    let bnds = [] in
    let arg = sexp_of_reason v_reason in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "reason"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_dev_alias in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "dev_alias"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_new_src_path in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "new_src_path"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_old_src_path in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "old_src_path"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds
  end
  module Tray_change = struct
    type reason = [
    | `Open
    | `Close
    | `Unknown of int
    ] with sexp
    open Libvirt.Event.Tray_change

let __t_of_sexp__ : Sexplib.Sexp.t -> t =
  let _tp_loc = "foo.ml.t"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let dev_alias_field = ref None and reason_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List
               ([ Sexplib.Sexp.Atom field_name; _field_sexp ]) :: tail ->
               ((match field_name with
                 | "dev_alias" ->
                     (match !dev_alias_field with
                      | None ->
                          let fvalue =
                            option_of_sexp string_of_sexp _field_sexp
                          in dev_alias_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "reason" ->
                     (match !reason_field with
                      | None ->
                          let fvalue = reason_of_sexp _field_sexp
                          in reason_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name ]) :: tail ->
               ((match field_name with
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _ as sexp)) :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _tp_loc !duplicates
               sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _tp_loc !extra sexp
             else
               (match ((!dev_alias_field), (!reason_field)) with
                | (Some dev_alias_value, Some reason_value) ->
                    { dev_alias = dev_alias_value; reason = reason_value; }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
                      [ ((Pervasives.( = ) !dev_alias_field None),
                         "dev_alias");
                        ((Pervasives.( = ) !reason_field None), "reason") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
  
let _ = __t_of_sexp__
  
let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp -> __t_of_sexp__ sexp
let sexp_of_t : t -> Sexplib.Sexp.t =
  fun { dev_alias = v_dev_alias; reason = v_reason } ->
    let bnds = [] in
    let arg = sexp_of_reason v_reason in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "reason"; arg ] in
    let bnds = bnd :: bnds in
    let arg = sexp_of_option sexp_of_string v_dev_alias in
    let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "dev_alias"; arg ] in
    let bnds = bnd :: bnds in Sexplib.Sexp.List bnds
  end
  module PM_wakeup = struct
    type reason = [
    | `Unknown of int
    ] with sexp

    type t = reason with sexp
  end
  module PM_suspend = struct
    type reason = [
    | `Unknown of int
    ] with sexp

    type t = reason with sexp
  end
  module Balloon_change = struct
    type t = int64 with sexp
  end
  module PM_suspend_disk = struct
    type reason = [
    | `Unknown of int
    ] with sexp

    type t = reason with sexp
  end
end

module E = struct
  module Lifecycle = struct
    include Libvirt.Event.Lifecycle
    let t_of_sexp = Mixin_sexplib.Lifecycle.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Lifecycle.sexp_of_t
  end
  module Reboot = struct
    include Libvirt.Event.Reboot
    let t_of_sexp = Mixin_sexplib.Reboot.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Reboot.sexp_of_t
  end
  module Rtc_change = struct
    include Libvirt.Event.Rtc_change
    let t_of_sexp = Mixin_sexplib.Rtc_change.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Rtc_change.sexp_of_t
  end
  module Watchdog = struct
    include Libvirt.Event.Watchdog
    let t_of_sexp = Mixin_sexplib.Watchdog.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Watchdog.sexp_of_t
  end
  module Io_error = struct
    include Libvirt.Event.Io_error
    let t_of_sexp = Mixin_sexplib.Io_error.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Io_error.sexp_of_t
  end
  module Graphics = struct
    include Libvirt.Event.Graphics
    let t_of_sexp = Mixin_sexplib.Graphics.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Graphics.sexp_of_t
  end
  module Control_error = struct
    include Libvirt.Event.Control_error
    let t_of_sexp = Mixin_sexplib.Control_error.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Control_error.sexp_of_t
  end
  module Block_job = struct
    include Libvirt.Event.Block_job
    let t_of_sexp = Mixin_sexplib.Block_job.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Block_job.sexp_of_t
  end
  module Disk_change = struct
    include Libvirt.Event.Disk_change
    let t_of_sexp = Mixin_sexplib.Disk_change.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Disk_change.sexp_of_t
  end
  module Tray_change = struct
    include Libvirt.Event.Tray_change
    let t_of_sexp = Mixin_sexplib.Tray_change.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Tray_change.sexp_of_t
  end
  module PM_wakeup = struct
    include Libvirt.Event.PM_wakeup
    let t_of_sexp = Mixin_sexplib.PM_wakeup.t_of_sexp
    let sexp_of_t = Mixin_sexplib.PM_wakeup.sexp_of_t
  end
  module PM_suspend = struct
    include Libvirt.Event.PM_suspend
    let t_of_sexp = Mixin_sexplib.PM_suspend.t_of_sexp
    let sexp_of_t = Mixin_sexplib.PM_suspend.sexp_of_t
  end
  module Balloon_change = struct
    include Libvirt.Event.Balloon_change
    let t_of_sexp = Mixin_sexplib.Balloon_change.t_of_sexp
    let sexp_of_t = Mixin_sexplib.Balloon_change.sexp_of_t
  end
  module PM_suspend_disk = struct
    include Libvirt.Event.PM_suspend_disk
    let t_of_sexp = Mixin_sexplib.PM_suspend_disk.t_of_sexp
    let sexp_of_t = Mixin_sexplib.PM_suspend_disk.sexp_of_t
  end
end
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
  with sexp

end

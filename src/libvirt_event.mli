open Sexplib.Std

module Any : sig
  type t =
  | Lifecycle of Libvirt.Event.Lifecycle.t
  | Reboot of Libvirt.Event.Reboot.t
  | RtcChange of Libvirt.Event.Rtc_change.t
  | Watchdog of Libvirt.Event.Watchdog.t
  | IOError of Libvirt.Event.Io_error.t
  | Graphics of Libvirt.Event.Graphics.t
  | IOErrorReason of Libvirt.Event.Io_error.t
  | ControlError of Libvirt.Event.Control_error.t
  | BlockJob of Libvirt.Event.Block_job.t
  | DiskChange of Libvirt.Event.Disk_change.t
  | TrayChange of Libvirt.Event.Tray_change.t
  | PMWakeUp of Libvirt.Event.PM_wakeup.t
  | PMSuspend of Libvirt.Event.PM_suspend.t
  | BalloonChange of Libvirt.Event.Balloon_change.t
  | PMSuspendDisk of Libvirt.Event.PM_suspend_disk.t
  with sexp

end

include Unix

external waitpids : int -> int array -> int * MinUnix.process_status
  = "onlyWin32_waitpids_ml"

external create_process_chdir : string -> string -> string option ->
  file_descr -> file_descr -> file_descr -> string option -> int
    = "onlyWin32_create_process_chdir"
  "onlyWin32_create_process_chdir_native"

type fileinfo = {
  dwFileAttributes : int;
  ftCreationTime : float; (* in Unix seconds *)
  ftLastAccessTime : float; (* in Unix seconds *)
  ftLastWriteTime : float; (* in Unix seconds *)
  dwVolumeSerialNumber : int;
  nFileSize : int64;
  nNumberOfLinks : int;
  nFileIndex : int64;
}

external getFileInformationByHandle : MinUnix.file_descr -> fileinfo
  = "onlyWin32_getFileInformationByHandle_ml"

external getFileInformationByName : string -> fileinfo
  = "onlyWin32_getFileInformationByName_ml"

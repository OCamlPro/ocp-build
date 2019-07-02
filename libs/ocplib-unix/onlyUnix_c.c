/**************************************************************************/
/*                                                                        */
/*   Typerex Libraries                                                    */
/*                                                                        */
/*   Copyright 2011-2017 OCamlPro SAS                                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>

#ifdef _WIN32

#include <winsock2.h>
#include <windows.h>
#include <sys/types.h>

#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

#ifndef CAML_UNIXSUPPORT_H
#include <caml/unixsupport.h>
#define CAML_UNIXSUPPORT_H
#endif

#ifdef _WIN32
#define UNAVAILABLE_PRIM(primname) \
  CAMLprim value primname(value unit_v) \
  { \
    caml_failwith("Unix primitive not available: " #primname); \
    return Val_unit; \
  }

#if !( OCAML_VERSION_OCP == 4010001 )
UNAVAILABLE_PRIM(unix_fork)
UNAVAILABLE_PRIM(unix_wait)
UNAVAILABLE_PRIM(unix_nice)
UNAVAILABLE_PRIM(unix_ftruncate)
UNAVAILABLE_PRIM(unix_truncate)
UNAVAILABLE_PRIM(unix_fchmod)
UNAVAILABLE_PRIM(unix_umask)
UNAVAILABLE_PRIM(unix_chown)
UNAVAILABLE_PRIM(unix_fchown)
UNAVAILABLE_PRIM(unix_chroot)
UNAVAILABLE_PRIM(unix_mkfifo)
#if (OCAML_VERSION < 4030)
UNAVAILABLE_PRIM(unix_readlink)
UNAVAILABLE_PRIM(unix_symlink)
#endif
#endif

UNAVAILABLE_PRIM(unix_clear_close_on_exec)
UNAVAILABLE_PRIM(unix_closedir)

UNAVAILABLE_PRIM(unix_getppid)
#if (OCAML_VERSION < 4060)
UNAVAILABLE_PRIM(unix_isatty)
#endif
UNAVAILABLE_PRIM(unix_opendir)
UNAVAILABLE_PRIM(unix_readdir)
UNAVAILABLE_PRIM(unix_rewinddir)
UNAVAILABLE_PRIM(unix_set_close_on_exec)

#endif

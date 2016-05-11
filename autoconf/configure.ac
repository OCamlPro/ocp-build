AC_INIT(ocp-build,1.99.16-beta)
CONFIGURE_ARGS=$*

# As stated in the GNU manuals, variables are not expansed when
# substituted. The reason is that they are supposed to be still
# available for the user to override them when calling "make".

AC_COPYRIGHT(Copyright 2015 OcamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_ENABLE(version-check,
 [AS_HELP_STRING([--disable-version-check],[do not check OCaml version])],
 [VERSION_CHECK="${enableval}"],
 [VERSION_CHECK="yes"])

AC_PROG_CC
AC_PROG_OCAML

if test "${OCAMLC}" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "${OCAMLVERSION}" = ""; then
   AC_MSG_ERROR([No OCaml version. ocamlc -version is not parsable.])
fi

AX_SPLIT_VERSION(OCAMLVERSION)
OCAML_MAJOR_VERSION=${AX_MAJOR_VERSION}
OCAML_MINOR_VERSION=${AX_MINOR_VERSION}
OCAML_POINT_VERSION=${AX_POINT_VERSION}

# Check that OCaml version is greater or equal to 4.00.0
if test "${VERSION_CHECK}" = "yes" ; then

  AX_COMPARE_VERSION( [${OCAMLVERSION}], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: ${OCAMLVERSION} is not supported]))
fi

HAS_BYTES=3.12.1
AX_COMPARE_VERSION( [${OCAMLVERSION}], [lt], [4.02.0],, HAS_BYTES=4.02.0)
AX_COMPARE_VERSION( [${OCAMLVERSION}], [lt], [4.03.0],, HAS_BYTES=4.03.0)


ocamldir='${libdir}/ocaml'
metadir='${ocamldir}'

AC_PROG_FINDLIB
if test "${OCAMLFIND}" != "no"; then
  metadir="$(${OCAMLFIND} -printconf destdir)"
  echo "initializing metadir from ocamlfind config"
fi

if test "x${prefix}" = "xNONE"; then
  if test "x${OPAM_PREFIX}" != "x"; then
    echo inheriting OPAM settings from OPAM_PREFIX
    prefix="${OPAM_PREFIX}"
    ocamldir='${libdir}'
    metadir="${OPAM_PREFIX}/lib"
  else
    prefix="$(dirname ${OCAMLLIB})"
    base="$(basename ${prefix})"
    if test "x${base}" = "xlib"; then
      prefix="$(dirname ${prefix})"
    fi
  fi
fi

AC_ARG_WITH(ocamldir,
 [AS_HELP_STRING([--with-ocamldir=DIRNAME],
               [root storage of the OCaml distribution])],
 [ocamldir="${withval}"],
 [])

AC_ARG_WITH(metadir,
 [AS_HELP_STRING([--with-metadir=DIRNAME],
               [store meta files in DIRNAME])],
 [metadir="${withval}"],
 [])

AC_SUBST(ocamldir)
AC_SUBST(metadir)

AC_SUBST(OCAML_MAJOR_VERSION)
AC_SUBST(OCAML_MINOR_VERSION)
AC_SUBST(OCAML_POINT_VERSION)
AC_SUBST(CONFIGURE_ARGS)
#AC_SUBST(VERSION)
AC_SUBST(OCAMLVERSION)
AC_SUBST(HAS_BYTES)
AC_CONFIG_FILES(Makefile.config config.ocpgen)
AC_OUTPUT

echo
echo "Summary:"
echo "prefix=${prefix}"
echo "exec_prefix=${exec_prefix}"
echo "bindir=${bindir}"
echo "   (Executables will be installed there)"
echo "libdir=${libdir}"
echo "ocamldir=${ocamldir}"
echo "   (OCaml files will be installed there)"
echo "metadir=${metadir}"
echo "   (META files will be installed there)"
echo "datarootdir=${datarootdir}"
echo "mandir=${mandir}"
echo "   (Manual pages will be installed there)"


#### autoconf defaults:
# prefix=NONE
# exec_prefix=NONE
# bindir=${exec_prefix}/bin
# libdir=${exec_prefix}/lib
# mandir=${datarootdir}/man
# datarootdir=${prefix}/share
# datadir=${datarootdir}

#### Corresponding substitutions (note that prefix is set)
# prefix=/usr/local
# exec_prefix=${prefix}
# bindir=${exec_prefix}/bin
# libdir=${exec_prefix}/lib
# mandir=${datarootdir}/man
# datarootdir=${prefix}/share
# datadir=${datarootdir}


# ocp-autoconf

A simple tool to manage basic configure.ac files (autoconf) for OCaml projects.

## Basic usage

`ocp-autoconf` will create a `configure' file and a 'autoconf/'
directory at the root of your project, so you should make sure such
files do not already exist. The `configure` file is just a script to
call the real `configure` script inside the `autoconf/` directory.

First, ask `ocp-autoconf` to create basic configuration files:

```
user@host: ocp-autoconf --save-template
Error: "ocp-autoconf.config" does not exist.
Saving template file.
zsh: exit 2
```

This will create two configuration files for `ocp-autoconf`:
* `ocp-autoconf.config`: a file containing options to describe your project
* `ocp-autoconf.ac`: a file that you can use to insert `autoconf` instructions
    inside the `autoconf/configure.ac` file that will be generated.

Now, you can edit the `ocp-autoconf.config` file to describe your project:

```
user@host: emacs ocp-autoconf.config &
```

When you are done, just call again `ocp-autoconf`:

```
user@host: ocp-autoconf
Saving template files...
* autoconf/m4/ax_compare_version.m4 saved
* autoconf/m4/ocaml.m4 saved
* autoconf/Makefile.rules saved
* autoconf/.gitignore saved
Warning: ./configure, creating one.
Warning: no ocp-autoconf.ac, creating one.
Calling aclocal -I m4...
Calling autoconf...
Now, you should call ./configure
```

Your project is ready, you can call the new `configure` script:

```
user@host: ./configure
checking for gcc... gcc
<...TRUNCATED...>
config.status: creating Makefile.config
config.status: creating config.ocpgen
config.status: creating ocaml-config.h

Summary:
prefix=/home/user/.opam/4.02.1
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
   (Executables will be installed there)
libdir=${exec_prefix}/lib
ocamldir=${libdir}/ocaml
   (OCaml files will be installed there)
metadir=/home/user/.opam/4.02.1/lib
   (META files will be installed there)
datarootdir=${prefix}/share
mandir=${datarootdir}/man
   (Manual pages will be installed there)
```

As shown by the output, by default, `./configure` will create 3 configuration
files that you can directly use in your project:
* `autoconf/Makefile.config`: it contains options that can be used directly
  by a Makefile (add `include autoconf/Makefile.config`)
* `autoconf/config.ocpgen`: it contains options that can be used directly
  by `ocp-build` (add `if include "autoconf/config.ocpgen" then {} else {}"`)
* `autoconf/ocaml-config.h`: an include file that can be included in
  your C files.

## Configuration Options (`ocp-autoconf.config`)

The following options can be set in the `ocp-autoconf.config` file:


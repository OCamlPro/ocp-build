# ocp-autoconf

A simple tool to manage standard project files for OCaml projects.
It can manage:
* `./configure`: basic configure.ac files (autoconf) to detect OCaml and
  its libraries, and set variables to be used in Makefiles and `ocp-build`
  files.
* `opam`: generate a standard opam file for your project, and a script to
  upload new versions of your project to Github
* Travis files: standard Travis files to test pull-requests on Github

## Basic Usage

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
More files can be added by using the `extra_config_files` option (filenames
should be related to the `autoconf/` directory).

## Advanced Usage

`ocp-autoconf` is able to manage different project files. By default,
it will manage all files it is able to manage, that are not yet
present in your project. This list of files will be stored in an
option `manage_files`, that you can modify to add or remove files.
Typically, you can use `ocp-autoconf` to generate basic files, and
then remove them from `manage_files` to be able to modify them
manually.

### The `autoconf` directory

`ocp-autoconf` generates a file `autoconf/configure.ac`, and several
m4 files in `autoconf/m4/`. These files are then processed by
`aclocal` and `autoconf` to generate a script `autoconf/configure`
(this is done automatically by `ocp-autoconf`).

`ocp-autoconf` uses the information provided by the
`ocp-autoconf.config` file to create theses files. It will also use a
file `ocp-autoconf.ac` and a files specified by the option
`extra_m4_files`. These files/options are particularly useful if you
are using other languages than OCaml, in which case you can put there
what you need.

Once you have used `ocp-autoconf`, you should call the configure
script and check the options that have been generated in the files
`autoconf/Makefile.config`, `autoconf/config.ocpgen` and 
`autoconf/ocaml-config.h`.

### The `./configure` script

This file is a very short script, that calls `autoconf/configure`.

### The `Makefile` file

This is a simple `Makefile` working for projects using `ocp-build`.
It uses variables from `autoconf/Makefile.config` and rules from
`autoconf/Makefile.rules`.

### The `build.ocp` file

This is a simple root file for `ocp-build`, that should work with most
projects, where source files are stored in sub-directories.

### The `opam` file

`ocp-autoconf` can generate a basic `opam` file for your project,
using only information from the `ocp-autoconf.config` file, and adding
the content of `opam.trailer` at the end.

### The `push-opam.sh` file

### The Travis files

## Configuration Options (`ocp-autoconf.config`)

The following options can be set in the `ocp-autoconf.config` file:


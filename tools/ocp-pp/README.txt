ocp-pp
======

A simple preprocessor for OCaml

`ocp-pp` is a simple preprocessor for OCaml. It is built on top of
compiler-libs, to be able to use the standard lexer.

The following directives are available:

#include "filename"
#ifdef XXX
#elif defined(YYY)
#else
#endif
#define XXX
#undef XXX

Versions and strings
--------------------

`ocp-pp` has two different types for strings and versions.

#define S "toto"

defines a macro "S" with value "toto" of type string

#define V version("4.01.0")

defines a macro "V" with value "4.01.0" of type version.

In #if, comparison operators are available, and have a different
meaning for strings and versions. Strings are compared
lexicographically, whereas versions are compared with a semantics of
version numbers: numbers are compared numerically, whereas non-numbers
are compared lexicographically. The '~' char in a version preceeds
an item that will be interpreted negatively (i.e. 4~1 is before 4).

Default macros
--------------

By default, the following macros are defined:

OCAML_VERSION: the version of OCaml used to compile `ocp-pp`

Usage
-----

ocp-pp [OPTIONS] FILENAMES

`ocp-pp` will read FILENAMES and print them on its standard input.

Available OPTIONS are:

-D MACRO : set MACRO as defined
-D MACRO=STRING : set MACRO as the string STRING
-V MACRO=VERSION : set MACRO as the version VERSION
-U MACRO : set MACRO as undefined

Limitations
-----------


# ocplib-file

A library to help with file operations. It contains support for both
filenames as strings (module `FileString`) and filenames as an
abstract type `File.t`.

The library is organized as follows:

* module `FileSig`: defines the two main documented signatures for file
    operations. These signatures are used in the interfaces of other modules.
* module `FileOS`: OS specific constants

* module `FileChannel`: file content operations on `in_channel` and
  `out_channel`
* module `FileString`: file operations with filenames as `string`, as in the
    `Filename` module of the standard library.

* module `FileLines`: some operations on file lines, manipulated as lists
    (they are manipulated as arrays in other modules)

* module `File`: file operations with an abstract type for filenames.
* module `Dir`: directory operations with filenames as `File.t`

* module `DirPath`: operations on paths


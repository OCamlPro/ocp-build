

type library = {
    lib_kind : string;
    mutable lib_names : string list;
    mutable lib_public_name : string option;
    mutable lib_requires : string list;
    mutable lib_flags : string list;
    mutable lib_linkflags : string list;
    mutable lib_wrapped : bool;
    mutable lib_has_byte : bool;
    mutable lib_has_asm : bool;
    mutable lib_modules : string list;
  }

type action = {
    mutable action_args : string list;
    mutable action_stdout : string option;
    mutable action_chdir : string option;
  }

type rule = {
    mutable rule_targets : string list;
    mutable rule_deps : string list;
    mutable rule_actions : action list;
  }

type element =
  | Rule of rule
  | Library of library

val parse : string -> DuneParser.sexp list -> element list

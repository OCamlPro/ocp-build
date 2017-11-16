(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open OcpCompat

open MetaTypes

(* let verbose = DebugVerbosity.verbose ["B"] "BuildOCamlMeta" *)


open BuildEngineTypes

open BuildOCamlTypes
open BuildOCPTypes
open BuildValue.TYPES

let set_field env var_name preds field value =
  let fields =
    var_name ::
      (List.map (fun (name, positive) ->
        if positive then name else "-" ^ name
       ) preds) @ [field]
  in
  env := BuildValue.set_deep_field !env fields (VString (value, StringRaw))

let add_META pj ocamllib meta_dirname meta_filename =
  (*
  Printf.eprintf "dirname=%S\n%!" meta_dirname;
  Printf.eprintf "filename=%S\n%!" meta_filename;
   *)
  try
    let p = MetaFile.meta_of_file meta_filename in
    if verbose 4 then
      Printf.eprintf "Loaded %S\n%!" meta_filename;

    let rec add_meta meta_dirname pj path name p =
      if verbose 4 then
        Printf.eprintf "add_meta %S %S\n%!" path name;
(*      let meta = MetaFile.meta_of_raw p in *)

      let dirname =
        let dir = MetaFile.directory p in
        match dir with
        | []
        | "" :: _
          -> meta_dirname
        | dirname :: _ ->
          if dirname.[0] = '^' || dirname.[0] = '+' then
            Filename.concat ocamllib
              (String.sub dirname 1 (String.length dirname-1))
          else
            if Filename.is_relative dirname then
              Filename.concat meta_dirname dirname
            else
              dirname
      in

      if verbose 4 then
        Printf.eprintf "dirname=%S\n%!" dirname;
      let exists =
          List.for_all (fun filename ->
            let proof_filename = Filename.concat dirname filename in
            if not (Sys.file_exists proof_filename) then begin
              if verbose 4 then
                Printf.eprintf
                  "Warning: proof of package %S does not exist\n%!"
                  proof_filename;
              false
              end else true)
                       (MetaFile.exists_if p)
      in
      if exists then
          (*
            let name =
            match meta.meta_name with
            TODO: meta_name is the archive name !!
            None -> name
            | Some name ->  (* lowercase to handle 'camlimages' *)
            String.lowercase name
            in  *)
        let fullname = path ^ name in

        let has_byte = MetaFile.archive p MetaFile.preds_byte in
        let has_asm = MetaFile.archive p MetaFile.preds_asm in

        (*
        let archive = match !has_asm, !has_byte with
            None, None -> None
          | Some asm_archive, Some byte_archive ->
            if asm_archive = byte_archive then
              Some byte_archive
            else begin
              Printf.eprintf "Warning: no common name for asm and byte in %S\n%!" fullname;
              None
            end
          | archive, None
          | None , archive -> archive
        in
        *)

        (*
        let requires = ref [] in
        StringMap.iter (fun _ var ->
          match var.metavar_preds with
          | [] ->
            requires := List.map (fun s ->
              match s with
                "camlp4" -> "camlp4lib"
              | s -> s
            ) var.metavar_value

            (*
              | [ "byte", true ] ->
              has_byte := Some var.metavar_value
              | [ "native", true ] ->
              has_asm := Some var.metavar_value
            *)
          | _ -> ()
        ) meta.meta_requires;
         *)

        let requires = MetaFile.requires p MetaFile.preds_none in

          (* for objects, we should set   pk.package_sources <- source_files; *)


        let create_package p_option fullname kind requires
            byte_targets asm_targets =

          let options = BuildValue.empty_env in

          let options = BuildValue.set options
            "requires" (VList (List.map (fun (s, link) ->
              let link =
                if Filename.check_suffix s ".syntax" then false else link in
              VTuple [VString (s, StringRaw);
                      VObject (BuildValue.set_bool BuildValue.empty_env "tolink" link)]
            ) requires)) in
          let options = BuildValue.set_bool options "generated" true in

          let stub_targets = List.filter (fun file ->
                                 Filename.check_suffix file ".o"
                                 || Filename.check_suffix file ".a")
                                         byte_targets in
          let byte_targets, asm_targets =
            match stub_targets with
            | [] -> byte_targets, asm_targets
            | _ ->
               let byte_targets =
                 List.filter (fun file ->
                     not (List.mem file stub_targets)) byte_targets in
               let asm_targets =
                 List.filter (fun file ->
                     not (List.mem file stub_targets)) asm_targets in
               byte_targets, asm_targets
          in
          let options =
            match byte_targets, asm_targets, stub_targets with
              | [], [], [] ->
                 BuildValue.set_bool options "meta" true
              | _ ->
                 let options = BuildValue.set_strings
                                 options "asm_targets" asm_targets in
                 let options = BuildValue.set_strings
                                 options "byte_targets" byte_targets in
                 BuildValue.set_strings options "stub_targets" stub_targets
          in

          let options = match p_option with
            | None -> options
            | Some p ->
              let env = ref BuildValue.empty_env in
              StringMap.iter (fun var_name var ->
                List.iter (fun (preds, value) ->
                  set_field env var_name preds "value" value
                ) var.var_assigns;
                List.iter (fun (preds, value) ->
                  set_field env var_name preds "append" value
                ) var.var_additions;
              ) p.p_variables;
              BuildValue.set options "META" (VObject !env)
          in

          let opk = BuildOCamlOCP2.add_ocaml_package
            (BuildValue.noloc fullname)
            pj
            {
              config_dirname = dirname;
              config_state = BuildValue.empty_config_state ();
              config_filename = meta_filename;
                (* matters only for non-installed packages *)
              config_filenames = [meta_filename, None];
              config_env = options;
            }
            fullname
            kind
          in
          let pk = opk.opk_package in
          pk.package_source_kind <- "meta";

            (* this package has already been generated *)

          begin
            match MetaFile.version p with
            | version :: _ ->
               opk.opk_version <- version
            | _ -> ()
          end;


            (* We don't check packages now.
               This will be done later, in BuildOCP.verify_packages

               BuildOCP.check_package pk;
            *)
          if verbose 5 then begin
            let s = BuildOCPPrinter.string_of_package (fun _ _ _ -> ()) pk in
            Printf.eprintf "Translation of %S:\n" meta_filename;
            Printf.eprintf "%s\n%!" s;
          end;
          ()
        in


          (* For syntaxes, we need to do some black magic, since we
             need to create two to three different packages.  - 2
             packages if (archive = None, syntax = Some _) || (archive
             = syntax = Some _) - 3 packages if archive = Some x,
             syntax = Some y, x <> y TODO: we should do a pass, before
             verify_packages, to fix problems introduced by this
             heuristic. In particular, since one META package can
             generate several OCP packages, we must discriminate the
             dependencies of other META packages to choose between the
             OCP packages.

             TODO: I am not completely happy with this behavior. We
             might want to have a more aggressive behavior, based on
             using a combination of META and ocamlobjinfo, to fix
             information from META.  *)
(*        begin match !has_syntax with
          | None -> *)
          create_package (Some p) fullname BuildOCPTypes.LibraryPackage
            (List.map (fun l -> (l,true)) requires)
            has_byte has_asm;
        (*
        | Some syntax_archive ->
          match archive with
          | None ->
            create_package None (fullname ^ ".ocp-syntax-library")
              BuildOCPTypes.LibraryPackage
              (List.map (fun l -> (l,true)) !requires)
              (Some syntax_archive);
            create_package (Some p) fullname BuildOCPTypes.SyntaxPackage
              [fullname ^ ".ocp-syntax-library", true] None;
          | Some archive ->
            create_package (Some p) fullname BuildOCPTypes.LibraryPackage
              (List.map (fun l -> (l,true)) !requires) (Some archive);
            create_package None (fullname ^ ".ocp-syntax-library")
              BuildOCPTypes.LibraryPackage
              (List.map (fun l -> (l,true)) !requires)
              (Some syntax_archive);
            create_package None (fullname ^ ".ocp-syntax")
              BuildOCPTypes.SyntaxPackage
              [fullname ^ ".ocp-syntax-library", true] None;
          end; *)
        List.iter (fun (name, pp) ->
          add_meta dirname pj (fullname ^ ".") name pp) p.p_packages

    in
    let name = MetaParser.name_of_META meta_filename in
    add_meta meta_dirname pj "" name p

  with e ->
    Printf.eprintf "Warning: exception %S while loading %S\n%!"
      (Printexc.to_string e) meta_filename


let load_META_files pj ocamllib top_dirname =
  if verbose 4 then
    Printf.eprintf "Loading METAs from %S\n%!" top_dirname;
    let files = try Sys.readdir top_dirname
  with _ ->
    Printf.eprintf "Warning: could not read files from META dir %S\n%!"
         top_dirname;
  [||]
  in
  Array.iter (fun basename ->
    let filename = Filename.concat top_dirname basename in
    if OcpString.starts_with basename ~prefix:"META." then
      add_META pj ocamllib top_dirname filename
    else
      if Sys.is_directory filename then
        let meta_filename = Filename.concat filename "META" in
        if Sys.file_exists meta_filename then
          add_META pj ocamllib filename meta_filename
  ) files

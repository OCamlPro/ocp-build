(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'man_block man_page = {
  man_name : string;   (* "OPAM" "GCC" *)
  man_section : int;
  man_date : string;
  man_release : string; (* "SOFT VERSION" *)
  man_org : string; (* GNU, OPAM Manual *)
  man_text :'man_block list;
}

module CMDLINER = struct

type man_block =
| S of string
| P of string
| I of string * string
| NOBLANK

type pager =
| PLAIN
| PAGER
| GROFF

(*---------------------------------------------------------------------------
  Copyright (c) 2011-2012 Daniel C. Bünzli. All rights reserved.
  Distributed under a BSD3 license, see license at the end of the file.
  cmdliner release 0.9.3
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let pr = Format.fprintf
let pr_str = Format.pp_print_string
let pr_char = Format.pp_print_char

let pr_to_temp_file pr v = try
                             let exec = Filename.basename Sys.argv.(0) in
                             let file, oc = Filename.open_temp_file exec "out" in
                             let ppf = Format.formatter_of_out_channel oc in
                             pr ppf v; Format.pp_print_flush ppf (); close_out oc;
                             Some file
  with Sys_error _ -> None

let p_indent = 7                                  (* paragraph indentation. *)
let l_indent = 4                                      (* label indentation. *)
let escape subst esc buf s =
  let subst s =
    let len = String.length s in
    if not (len > 1 && s.[1] = ',') then (subst s) else
      if len = 2 then "" else
        esc s.[0] (String.sub s 2 (len - 2))
  in
  Buffer.clear buf; Buffer.add_substitute buf subst s;
  let s = Buffer.contents buf in (* twice for $(i,$(mname)). *)
  Buffer.clear buf; Buffer.add_substitute buf subst s;
  Buffer.contents buf


let pr_tokens ?(groff = false) ppf s =
  let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  try while (true) do
      while (!i < len && is_space s.[!i]) do incr i done;
    let start = !i in
    if start = len then raise Exit;
    while (!i < len && not (is_space s.[!i]) && not (s.[!i] = '-')) do
      incr i
    done;
    pr_str ppf (String.sub s start (!i - start));
    if !i = len then raise Exit;
    if s.[!i] = '-' then
      (incr i; if groff then pr_str ppf "\\-" else pr_char ppf '-');
    if (!i < len && is_space s.[!i]) then
      (if groff then pr_char ppf ' ' else Format.pp_print_space ppf ())
    done with Exit -> ()

  (* Plain text output *)

let plain_esc c s = match c with 'g' -> "" (* groff specific *) | _ ->  s
let pr_indent ppf c = for _i = 1 to c do pr_char ppf ' ' done
let pr_plain_blocks subst ppf ts =
  let buf = Buffer.create 1024 in
  let escape t = escape subst plain_esc buf t in
  let pr_tokens ppf t = pr_tokens ppf (escape t) in
  let rec aux = function
    | [] -> ()
    | t :: ts ->
      begin match t with
      | NOBLANK -> ()
      | P s -> pr ppf "%a@[%a@]@," pr_indent p_indent pr_tokens s
      | S s -> pr ppf "%a" pr_tokens s
      | I (label, s) ->
        let label = escape label in
        let ll = String.length label in
        pr ppf "@[%a@[%a@]" pr_indent p_indent pr_tokens label;
        if s = "" then () else
          if ll < l_indent then
            pr ppf "%a@[%a@]@]@," pr_indent (l_indent - ll) pr_tokens s
          else
            pr ppf "@\n%a@[%a@]@]@,"
              pr_indent (p_indent + l_indent) pr_tokens s
      end;
      begin match ts with
      | NOBLANK :: ts -> aux ts
      | ts -> Format.pp_print_cut ppf (); aux ts
      end
  in
  aux ts

let pr_plain_page subst ppf p =
  pr ppf "@[<v>%a@]" (pr_plain_blocks subst) p.man_text

  (* Groff output *)

let groff_esc c s = match c with
  | 'i' -> (str "\\fI%s\\fR" s)
  | 'b' -> (str "\\fB%s\\fR" s)
  | 'p' -> "" (* plain text specific *)
  | _ -> s

let pr_groff_blocks subst ppf text =
  let buf = Buffer.create 1024 in
  let escape t = escape subst groff_esc buf t in
  let pr_tokens ppf t = pr_tokens ~groff:true ppf (escape t) in
  let pr_block = function
    | P s -> pr ppf "@\n.P@\n%a" pr_tokens s
    | S s -> pr ppf "@\n.SH %a" pr_tokens s
    | NOBLANK -> pr ppf "@\n.sp -1"
    | I (l, s) -> pr ppf "@\n.TP 4@\n%a@\n%a" pr_tokens l pr_tokens s
  in
  List.iter pr_block text

let pr_groff_page subst ppf p =
  pr ppf ".\\\" Pipe this output to groff -man -Tutf8 | less@\n\
          .\\\"@\n\
          .TH \"%s\" %d \"%s\" \"%s\" \"%s\"@\n\
          .\\\" Disable hyphenantion and ragged-right@\n\
          .nh@\n\
     .ad l\
     %a@?"
    p.man_name
    p.man_section
    p.man_date
    p.man_release
    p.man_org
    (pr_groff_blocks subst) p.man_text

(* Printing to a pager *)

let find_cmd cmds =
  let cmd c = Sys.command (str "type %s 1>/dev/null 2>/dev/null" c) = 0 in
  try Some (List.find cmd cmds) with Not_found -> None

let pr_to_pager print ppf v =
  let pager =
    let cmds = ["less"; "more"] in
    let cmds = try (Sys.getenv "PAGER") :: cmds with Not_found -> cmds in
    find_cmd cmds
  in
  match pager with
  | None -> print PLAIN ppf v
  | Some pager ->
    let cmd = match (find_cmd ["groff"; "nroff"]) with
      | None ->
        begin match pr_to_temp_file (print PLAIN) v with
        | None -> None
        | Some f -> Some (str "%s < %s" pager f)
        end
      | Some c ->
        begin match pr_to_temp_file (print GROFF) v with
        | None -> None
        | Some f ->
       (* TODO use -Tutf8, but annoyingly maps U+002D to U+2212. *)
          let xroff = if c = "groff" then c ^ " -Tascii -P-c" else c in
          Some (str "%s -man < %s | %s" xroff f pager)
        end
    in
    match cmd with
    | None -> print PLAIN ppf v
    | Some cmd -> if (Sys.command cmd) <> 0 then print PLAIN ppf v

let rec print ?(subst = fun x -> x) fmt ppf (page : 'a man_page) =
  match fmt with
  | PAGER -> pr_to_pager (print ~subst) ppf page
  | PLAIN -> pr_plain_page subst ppf page
  | GROFF -> pr_groff_page subst ppf page

end

module RAW = struct

  type div =
  | SH of span list
  | LI of span list * span list
  | P of span list
  | P2 of span list

  and span =
  | S of string
  | B of string
  | I of string


  let rec groff_div div =
    match div with
    | P spans ->
      [ ".P";
        groff_spans spans
      ]
    | P2 spans ->
      [
        ".sp -1";
        ".P";
        groff_spans spans
      ]
    | SH spans ->
      [ ".SH " ^ groff_spans spans ]
    | LI (title_spans, text_spans) ->
      [ ".TP 4";
        groff_spans title_spans;
        groff_spans text_spans
      ]

  and groff_spans spans =
    String.concat "" (List.map groff_span spans)

  and groff_span span =
    match span with
    | S s -> s
    | B s -> Printf.sprintf "\\fB%s\\fR" s
    | I s -> Printf.sprintf "\\fI%s\\fR" s

  let groff_page p =
    String.concat "\n" ([
      ".\\\" Pipe this output to groff -man -Tutf8 | less";
      ".\\\"";
      Printf.sprintf ".TH %S %d %S %S %S"
        p.man_name
        p.man_section
        p.man_date
        p.man_release
        p.man_org;
      ".\\\" Disable hyphenantion and ragged-right";
      ".nh";
      ".ad l";
    ] @
                          (List.flatten (List.map groff_div p.man_text))
    )

end

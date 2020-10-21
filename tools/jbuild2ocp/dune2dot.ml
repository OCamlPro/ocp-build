open StringCompat
open DuneInterp.TYPES
open Ocamldot.TYPES

type t = {
  name : string;
  public_name : string;
  requires : string list;
  shape :  Ocamldot.TYPES.node_attributes;
}

let libraries = ref StringMap.empty

let shape_of_lib lib =
  match lib.lib_kind with
  | "executable"
  | "program"
    -> NodeShape Ellipse
  | "library" -> NodeShape Box
  | _ -> NodeShape Box

let register _file eles =

  List.iter (function
      | Rule _r -> ()
      | Library lib ->
        let names =
          match lib.lib_public_names with
          | [] -> lib.lib_names
          | names -> names
        in
        List.iteri (fun i public_name ->
            let name = List.nth lib.lib_names i in
            let v = {
              public_name;
              name;
              requires = lib.lib_requires;
              shape = shape_of_lib lib;
            } in
            libraries := StringMap.add name v !libraries;
            libraries := StringMap.add public_name v !libraries;
          ) names
    ) eles

let save dirname =

  FileString.make_dir ~p:true dirname;

  begin

    let graph = Ocamldot.create "Project"
        [ GraphSize (11.7, 8.3);
          Ratio RatioFill
        ]
    in

    let libs = ref StringMap.empty in
    StringMap.iter (fun name v ->
        if name = v.public_name then
          let attrs = [] in
          let attrs = v.shape :: attrs in
          let node = Ocamldot.node graph v.public_name attrs in
          libs := StringMap.add v.public_name (Some v, node) !libs;
          libs := StringMap.add v.name (None, node) !libs;
      ) !libraries;

    StringMap.iter (fun _name (opt, node1) ->
        match opt with
        | None -> ()
        | Some v ->
          List.iter (fun name ->
              let node2 =
                try
                  let (_, node2)  = StringMap.find name !libs in
                  node2
                with Not_found ->
                  let attrs = [] in
                  let attrs = NodeShape Diamond :: attrs in
                  let node = Ocamldot.node graph name attrs in
                  libs := StringMap.add name (None, node) !libs;
                  node
              in
              let _edge = Ocamldot.edge node1 node2 [] in
              ()
            ) v.requires

      ) !libs;

    let filename = Filename.concat dirname "project.dot" in
    Ocamldot.save graph filename;
    Ocamldot.dot2pdf filename (Filename.chop_extension filename ^ ".pdf");
  end;

  StringMap.iter (fun name v ->
      if name = v.public_name then
        let graph = Ocamldot.create "Project"
            [ GraphSize (11.7, 8.3);
              Ratio RatioFill
            ]
        in

        let libs = ref StringMap.empty in

        let rec iter v =

          let attrs = [] in
          let attrs = v.shape :: attrs in
          let node1 = Ocamldot.node graph v.public_name attrs in
          libs := StringMap.add v.name (None, node1) !libs;
          libs := StringMap.add v.public_name (Some v, node1) !libs;

          List.iter (fun name ->
              let node2 =
                try
                  let (_, node2)  = StringMap.find name !libs in
                  node2
                with Not_found ->
                try
                  let v = StringMap.find name !libraries in
                  iter v
                with Not_found ->
                  let attrs = [] in
                  let attrs = NodeShape Diamond :: attrs in
                  let node = Ocamldot.node graph name attrs in
                  libs := StringMap.add name (None, node) !libs;
                  node
              in
              let _edge = Ocamldot.edge node1 node2 [] in
              ()
            ) v.requires;
          node1

        in
        let _node = iter v in

        let filename = Filename.concat dirname name ^ ".dot" in
        Ocamldot.save graph filename;
        Ocamldot.dot2pdf filename (Filename.chop_extension filename ^ ".pdf");


    ) !libraries

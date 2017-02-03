
type state = {
  unit : unit;
}

module OCP_arg = struct

  type context = state

  let filesubst = BuildSubst.create_substituter []
  let parse_error () = exit 2
  let new_file ctx filename digest = ()
  let define_package loc ctx config ~name ~kind =
    ()

  end

module EvalOCP2 = BuildOCP2Interp.Eval(OCP_arg)

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let arg = Sys.argv.(i) in
    let state = { unit = () } in
    let config = BuildValue.empty_config () in
    let ( _ : BuildValue.Types.config) =
      EvalOCP2.read_ocamlconf arg state config in
    ()
  done

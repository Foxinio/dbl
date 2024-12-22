(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

type fname = string

let parse_file ?pos ~use_prelude fname =
  let imports, prog = File.parse_defs ?pos fname in
  let make data = { prog with data } in
  let prog = make (Lang.Surface.EDefs(prog.data, make Lang.Surface.EUnit)) in
  Import.prepend_imports ~use_prelude imports prog

let make_nowhere data =
  { Lang.Surface.pos  = Position.nowhere
  ; Lang.Surface.data = data
  }

let repl ~use_prelude =
  if use_prelude then
    let imported, prelude_defs = Import.import_prelude () in
    let repl_expr = make_nowhere (Lang.Surface.ERepl (Repl.repl_seq imported)) in
    make_nowhere (Lang.Surface.EDefs(prelude_defs, repl_expr))
  else
    make_nowhere (Lang.Surface.ERepl (Repl.repl_seq Import.import_set_empty))

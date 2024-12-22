(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** File with functionality of REPL Instrs *)

module S = Lang.Surface

(** Produce the string representation of a relative path. *)
let string_of_rel_path (p, n) =
  List.fold_right (fun n' id -> n' ^ "/" ^ id) p n

(** Produce the string representation of an absolute path. *)
let string_of_abs_path p =
  "/" ^ string_of_rel_path p

let make_nowhere data =
  { Lang.Surface.pos  = Position.nowhere
  ; Lang.Surface.data = data
  }

(** Contains help message, describing features of this functionality *)
let help_message =
  {| help message here |}

let show_functionality =
  [ "implicits"; "datas"; "ctors"; "vars"; "var_vals"; "modules"; "open_mods" ]

let handle_show ~imported arg =
  if not @@ List.mem arg show_functionality then
    Error.fatal (Error.unknown_show_command arg);
  match arg with
  | _ ->
    (* TODO : Add this functionality *)
    S.REPLI_Show arg

let handle_set var value =
  (* TODO : Add this functionality *)
  failwith "unimplemented"

let parse_instr ~lexbuf ~imported instr =
  match instr with
  | "exit" | "quit" ->
    Printf.printf "\n%!";
    exit 0
  | "help" | "h"  ->
    Printf.printf "%s\n%!" help_message;
    S.REPLI_Handled
  | "type" | "t" ->
    let e = YaccParser.repl_expr Lexer.token lexbuf in
    let e' = Desugar.tr_expr e in
    S.REPLI_Type e'
  | "kind" | "k" ->
    let tp = YaccParser.repl_ty_expr Lexer.token lexbuf in
    let tp' = Desugar.tr_type_expr tp in
    S.REPLI_Kind tp'
  | "methods" | "m" ->
    let tp = YaccParser.repl_ty_expr Lexer.token lexbuf in
    let tp' = Desugar.tr_type_expr tp in
    S.REPLI_Methods tp'
  | "signature" | "s" ->
    let e = YaccParser.repl_expr Lexer.token lexbuf in
    let poly = Desugar.tr_poly_expr e in
    S.REPLI_Sig poly
  | "module" ->
    let path = YaccParser.repl_import_path Lexer.token lexbuf in
    let path_name =
      match path with
      | Raw.IPAbsolute(p, n) -> string_of_abs_path (p, n)
      | Raw.IPRelative(p, n) -> string_of_rel_path (p, n)
    in
    let _, defs = Import.import_one
      Import.import_set_empty (make_nowhere (Raw.IImportOpen path))
    in S.REPLI_Module (path_name, defs)
  | "cd" ->
    let path = YaccParser.repl_string Lexer.token lexbuf in  
    begin try Sys.chdir path with
    | Sys_error err -> Error.fatal (Error.change_directory_failed err)
    end;
    S.REPLI_Handled
  | "show" ->
    let arg = YaccParser.repl_string Lexer.token lexbuf in
      handle_show ~imported arg
  | "set" ->
    let (option, value) = YaccParser.repl_string2 Lexer.token lexbuf in
    handle_set option value;
    S.REPLI_Handled
  | "dump" ->
    let e = YaccParser.repl_expr Lexer.token lexbuf in
    let e' = Desugar.tr_expr e in
    S.REPLI_Dump e'
  | "sh" ->
    let str = YaccParser.repl_string Lexer.token lexbuf in
    begin match Unix.system str with
    | Unix.WEXITED n -> S.REPLI_Handled
    | Unix.WSIGNALED n -> S.REPLI_Handled
    | Unix.WSTOPPED n -> S.REPLI_Handled
    end
  | _ -> Error.fatal (Error.unknown_repl_instruction instr)

(* ========================================================================= *)

let parse_instr_content ~(lexbuf : Lexing.lexbuf) ~imported (instr : string) =
  make_nowhere (S.DReplInstr (parse_instr ~lexbuf ~imported instr))

(* ========================================================================= *)

let rec repl_seq imported () =
  InterpLib.Error.wrap_repl_cont (repl_seq_main imported) ()

and repl_seq_main imported () =
  flush stderr;
  Buffer.clear InterpLib.Error.repl_input;
  let fn buf n =
    let res = input stdin buf 0 n in
    Buffer.add_subbytes InterpLib.Error.repl_input buf 0 res;
    res
  in
  Printf.printf "> %!";
  let lexbuf = Lexing.from_function fn in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = "<stdin>"
    };
  match YaccParser.repl Lexer.token lexbuf with
  | Raw.REPL_Instr instr ->
    let instr' = parse_instr_content ~lexbuf ~imported instr in
    Seq.Cons([instr'], repl_seq imported)

  | Raw.REPL_Expr e ->
    let def = make_nowhere (Lang.Surface.DReplExpr(Desugar.tr_expr e)) in
    Seq.Cons([def], repl_seq imported)

  | Raw.REPL_Defs defs ->
    let defs = Desugar.tr_defs defs in
    Seq.Cons(defs, repl_seq imported)

  | Raw.REPL_Import import ->
    let imported, defs = Import.import_one imported import in
    Seq.Cons(defs, repl_seq imported)

  | exception Parsing.Parse_error ->
    Error.fatal (Error.unexpected_token
      (Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      (Lexing.lexeme lexbuf))


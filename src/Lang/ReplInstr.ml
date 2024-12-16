(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Language extensions Parts used exclusively by REPL. *)

type stage =
  | Raw
  | Surface
  | TUnif
  | EUnif
  | Core

type empty = |

(** Set of instructions for manipulating REPL *)
type ('expr, 'def, 'type_expr) repl_instr =
  | REPLI_Exit
  (** exit the REPL *)

  | REPLI_Help
  (** print all available commands *)

  | REPLI_Type    of stage option * 'expr
  (** print type of an expression, default stage is [EUnif] *)

  | REPLI_Kind    of 'type_expr
  (** print kind of an expression *)

  | REPLI_Methods of 'type_expr
  (** print registered methods for given type *)

  | REPLI_Module  of string * 'def list
  (** print interface of a module *)

  | REPLI_Cd      of string
  (** change current working directory *)

  | REPLI_Show    of string
  (** show part of an environment
      Available options are:
      - [implicits] - show registered implicits and their types
      - [datas]     - show defined data types
      - [ctors]     - show defined constructors and their types
      - [vars]      - show defined variables and types of their values
      - [var_vals]  - show defined variables and their values
      - [modules]   - show imported modules
      - [open_mods] - show opened modules *)

  | REPLI_Set     of string * string
  (** Set some flag or option of REPL *)

  | REPLI_Dump    of stage * 'expr
  (** Dump intermediate representation in speciefied stage,
      and stop evaluation after that *)

  | REPLI_Shell   of string list
  (** Run some command in shell (REPL will stop working
      until command finishes) *)

  | REPLI_Handled
  (** Internal representation saying that there was some REPL instruction,
      but it's been handled at an earlier stage *)


(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for REPL instructions *)

open Common
open TypeCheckFix

(* ========================================================================= *)

let max_var_length lst =
  let f acc (v, _) = max acc (String.length v) in
  List.fold_left f 0 lst

let pp_vars lst =
  let padding = max_var_length lst in
  let mapper (v, s) =
    Printf.sprintf "%-*s : %s\n" (padding + 2) v s
  in List.map mapper lst

(* ========================================================================= *)

let make_nowhere data =
  let pos = { Position.nowhere with pos_fname="<internal>"} in
  { Lang.Unif.pos  = pos
  ; Lang.Unif.data = data
  }

let make_str : type dir. Env.t -> (T.typ, dir) request -> string ->
    T.expr * (T.typ, dir) response * ret_effect =
  fun env req str ->
  let e = make_nowhere (T.EStr str) in
  let tp = T.Type.t_var T.BuiltinType.tv_string in
  match req with
  | Infer ->
    e, Infered tp, Pure
  | Check tp' ->
    Error.check_unify_result ~pos:Position.nowhere
      (Unification.subtype env tp' tp)
      ~on_error:(Error.expr_type_mismatch ~env tp' tp);
      e, Checked, Pure

let make_unit : type dir. Env.t -> (T.typ, dir) request ->
    T.expr * (T.typ, dir) response * ret_effect =
  fun env req ->
  let e = make_nowhere (T.ECtor(make_nowhere T.EUnitPrf, 0, [], [])) in
  match req with
  | Infer ->
    e, Infered T.Type.t_unit, Pure
  | Check tp ->
    Error.check_unify_result ~pos:Position.nowhere
      (Unification.subtype env tp T.Type.t_unit)
      ~on_error:(Error.expr_type_mismatch ~env tp T.Type.t_unit);
      e, Checked, Pure

let pp_definition env indent var scheme =
  let pp_ctx = Pretty.empty_context () in
  let scheme_str = Pretty.scheme_to_string pp_ctx env scheme in
  Printf.sprintf "%s%s : %s" indent var scheme_str

(* TODO : Combine two below functions to followinf signature:
  unwrap_ident : S.ident -> (Env.t -> string * T.schema) option
*)
let unwrap_ident (id : S.ident) =
  match id with
  | IdLabel -> assert false
  | IdVar (p, name)      when p -> Some (name)
  | IdImplicit (p, name) when p -> Some ("~"^name)
  | IdMethod (p, name)   when p -> Some ("method "^name)
  | _ -> None

let unwrap_ident1 (id : S.ident) =
  match id with
  | IdLabel -> assert false
  | IdVar (p, name)      when p ->
    Some (fun env ->
      match Env.lookup_var env (NPName name) with 
      | Some (VI_Var (_, sch)) -> name, sch
      | _ -> failwith "bad")
  | IdImplicit (p, name) when p ->
    Some (fun env ->
      match Env.lookup_implicit env (NPName name) with
      | Some(_, sch, _) -> "~"^name, sch
      | _ -> failwith "bad")
  | IdMethod (p, name)   when p -> Some (inner name ("method "^name))
  | _ -> None

let extract_scheme (vi : Module.var_info option) =
  match vi with
  | Some (VI_Var (_, sch)) -> sch
  | Some (VI_Ctor _) -> failwith "shouldnt happen"
  | Some (VI_MethodFn _) -> failwith "shouldnt happen"
  | None -> failwith "shouldnt happen"

exception ModuleSigFound of string

let rec prepare_def ~tcfix env ienv (def : S.def) defs req eff indent acc =
  let open (val tcfix : TCFix) in
  begin match def.data with
  | DLetId (ident, _)
  | DLetFun (ident, _, _, _) ->
    let var = unwrap_ident ident in
    check_def env ienv def req eff { run =
      fun env ienv req eff ->
      match var with
      | Some name ->
        let scheme = Env.lookup_var env (NPName name) |> extract_scheme in
        let line = pp_definition env indent name scheme in
        prepare_defs ~tcfix env ienv defs req eff indent (line :: acc)
      | None -> prepare_defs ~tcfix env ienv defs req eff indent acc
    }
  | DMethodFn (pub, operator, method_name) ->
    failwith ""
  | DData (pub, data_name, _, _) ->
    failwith ""
  | DRec defs' -> prepare_defs ~tcfix env ienv (defs' @ defs) req eff indent acc
  | DModule (pub, mod_name, defs) ->
    failwith ""
  | DImplicit _ | DHandlePat _ | DLabel _ | DOpen _ | DLetPat _
  | DReplExpr _ | DReplInstr _ ->
    check_def env ienv def req eff { run =
      fun env ienv req eff ->
      prepare_defs ~tcfix env ienv defs req eff indent acc
    }
  end |> ignore;
  assert false

and prepare_defs ~tcfix env ienv (defs : S.def list) req eff indent acc =
  begin match defs with
  | [] ->
    raise (ModuleSigFound (String.concat "\n" acc))
  | def :: defs ->
    prepare_def ~tcfix env ienv def defs req eff indent acc
  end |> ignore;
  assert false


and prepare_module ~tcfix env ienv (is_public, module_name, defs) eff indent =
  try
    let _ = prepare_defs ~tcfix env ImplicitEnv.empty defs Infer eff (indent^"  ") [] in
    failwith "internal mod error"
  with ModuleSigFound translated ->
    Printf.sprintf "%smodule %s\n%s%s\n%send"
        indent module_name
        (indent^"  ") translated
        indent


let rec handle_module_instr : type dir. tcfix:tcfix ->
  Env.t -> ImplicitEnv.t -> S.def list ->
  (T.typ, dir) request -> T.effrow ->
  T.expr * (T.typ, dir) response * ret_effect =
  fun ~tcfix env ienv (defs : S.def list) req eff ->
  let open (val tcfix : TCFix) in
  match defs with
  | [{ data = DModule (is_public, module_name, defs);_ }] ->
    let str = prepare_module ~tcfix env ienv (is_public, module_name, defs) eff "" in
    raise (ModuleSigFound str)
  | def :: defs -> 
    check_def env ienv def req eff { run =
      fun env ienv req eff ->
        handle_module_instr ~tcfix env ienv defs req eff
    }
  | _ -> failwith "internal mod error"

let handle_module ~tcfix env (mod_name, defs) eff =
  try
    let _ = handle_module_instr ~tcfix env ImplicitEnv.empty defs Infer eff in
    failwith "internal mod error"
  with ModuleSigFound translated -> translated

(* ========================================================================= *)

let handle_repl_instr ~tcfix env ienv (instr : S.repl_instr) eff =
  let open (val tcfix : TCFix) in
  match instr with
  | REPLI_Type e ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let (_, tp, _) = infer_expr_type env e eff in
    ImplicitEnv.end_generalize_impure ~pos:e.pos ~env:env1 ims tp;
    let pp_ctx = Pretty.empty_context () in
    let str = Printf.sprintf "Type: %s"
      (Pretty.type_to_string pp_ctx env tp) in
    T.REPLI_ToPrint str

  | REPLI_Kind tp ->
    let uv = T.Kind.fresh_uvar () in
    let _ = Type.check_kind env tp uv in
    let pp_ctx = Pretty.empty_context () in
    let str = Printf.sprintf "Kind: %s" (Pretty.kind_to_string pp_ctx uv) in
    T.REPLI_ToPrint str

  | REPLI_Scheme pe ->
    let (_, _, sch, _) = PolyExpr.infer_scheme ~tcfix env pe eff in
    let pp_ctx = Pretty.empty_context () in
    let str = Printf.sprintf "Scheme: %s"
      (Pretty.scheme_to_string pp_ctx env sch) in
    T.REPLI_ToPrint str

  | REPLI_Methods tp ->
    let tp' = Type.tr_ttype env tp in
    let a =
      match T.Type.whnf tp' with
      | Whnf_Neutral(NH_Var a, _) -> a
      | Whnf_Neutral(NH_UVar _, _) ->
        Error.fatal (Error.method_call_on_unknown_type ~pos:tp.pos)

      | Whnf_PureArrow _ | Whnf_Arrow _ | Whnf_Handler _
      | Whnf_Label _ ->
        Error.fatal (Error.method_call_on_invalid_type ~pos:tp.pos ~env tp')

      | Whnf_Effect _ | Whnf_Effrow _ ->
        failwith "Internal kind error"
    in
    let method_strs = Env.get_methods env a
      |> List.map (fun (m, sch : T.var * _) ->
        m.name, Pretty.scheme_to_string (Pretty.empty_context ()) env sch)
      |> pp_vars in
    let str = Printf.sprintf "Methods: %s\n%s"
        (Pretty.tvar_to_string (Pretty.empty_context ()) env a)
        (String.concat "\n" method_strs) in
    T.REPLI_ToPrint str

  | REPLI_Module (mod_name, defs) ->
    let res = handle_module ~tcfix env (mod_name, defs) eff in
    let str = Printf.sprintf "module %s\n%s\nend\n%!" mod_name res in
    T.REPLI_ToPrint str
  | REPLI_Show _ -> failwith "unimplemented"
  | REPLI_Dump _ -> failwith "unimplemented"
  | REPLI_Handled -> failwith "unimplemented"


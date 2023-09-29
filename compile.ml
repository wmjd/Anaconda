open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let stackloc si = RegOffset(-8 * si, RSP)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | EPrim1(op, e) -> compile_prim1 op e si env
  | EPrim2(op, e1, e2) -> compile_prim2 op e1 e2 si env
  | ELet(binding, body) -> 
    let vis, ext_env = compile_binding binding si env in
	let bis = compile_expr body (si + List.length binding) ext_env in
	vis @ bis
  | ENumber(i) -> [IMov(Reg RAX, Const i)]
  | EId(x) -> (
    match find env x with
    | None -> failwith "Unbound id"
    | Some(i) -> [IMov(Reg RAX, stackloc i)] )

(* Tail Recursive implementation needs to *reverse* the instruction list as usual trick. In this case, it is a little tricky because the list, ins, is built hierarchically in sections and subsections which must remain ordered *)
and compile_binding b si env = 
  let rec iter b si env ins =
    match b with
    | [] -> (List.rev ins, env)
    | (x,v)::more -> (
      let vis = compile_expr v si env in 
      let sis = [IMov(stackloc si, Reg RAX)] in
      iter more (si+1) ((x,si)::env) (sis @ (List.rev vis) @ ins) ) 
  in iter b si env []

and compile_prim1 op e si env =
  (* TODO *)
  failwith "Not yet implemented"

and compile_prim2 op e1 e2 si env =
  let e1is = compile_expr e1 si env in
  let e2is = compile_expr e2 (si + 1) env in
  let op_is = (
    match op with
    | Plus ->  [IAdd(Reg RAX, stackloc (si + 1))]
 	| Minus -> [ISub(Reg RAX, stackloc (si + 1))]
	| Times -> [ISub(Reg RAX, stackloc (si + 1))] )
  in
    e1is @
    [IMov(stackloc si, Reg RAX)] @
    e2is @
    [IMov(stackloc (si+1), Reg RAX)] @
    [IMov(Reg RAX, stackloc si)] @
    op_is

let compile_to_string prog =
  let prelude =
    "section .text\n" ^
    "global our_code_starts_here\n" ^
    "our_code_starts_here:" in
  let compiled = (compile_expr prog 1 []) in
  let as_assembly_string = (to_asm (compiled @ [IRet])) in
  sprintf "%s%s\n" prelude as_assembly_string


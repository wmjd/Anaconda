open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

(* Defines rules for what ids are valid -- ids must match the regex and not
 * be a reserved word *)
let reserved_words = ["let"; "add1"; "sub1"]

let check_reserved word = 
  if (List.fold_left (fun acc rw -> (rw = word) || acc) false reserved_words)
  then failwith ("Syntax error: " ^ word ^ " is a reserved word")
  else word    

(* Converts a string to Some int or None if not convertable *)
let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let rec parse (sexp : Sexp.t) : Expr.expr =
  match sexp with
    | Atom(s) ->
      (match int_of_string_opt s with
        | None -> EId(s)
        | Some(i) -> ENumber(i))
    | List(sexps) ->
      match sexps with
        | [Atom("add1"); arg] -> EPrim1(Add1, parse arg)
        | [Atom("sub1"); arg] -> EPrim1(Sub1, parse arg)
        | [Atom("+"); arg1; arg2] -> EPrim2(Plus, parse arg1, parse arg2)
        | [Atom("-"); arg1; arg2] -> EPrim2(Minus, parse arg1, parse arg2)
        | [Atom("*"); arg1; arg2] -> EPrim2(Times, parse arg1, parse arg2)
        | [Atom("let"); binding; body] ->
          ELet(parse_binding binding, parse body)
        | _ -> failwith "Parse error"

and parse_binding (binding : Sexp.t) : (string * Expr.expr) list =
  match binding with
    | List([List([Atom(name); value])]) -> [(name, parse value)]
    | List((List([Atom(name); value]))::more) -> ( (name, parse value)::(parse_binding (List more)) )

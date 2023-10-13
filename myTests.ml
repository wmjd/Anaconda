open Runner
open Expr
open Printf
open OUnit2

(* Fill in `myTestList` with your own tests. There are two ways to add a test:
 *
 * (1) By adding the code of the program as a string. In this case, add an entry
 *     of this form in `myTestList`:
 *
 *     t <test_name> <program_code> <result>
 *
 * (2) By adding a test inside the 'input/' folder and adding an entry for it
 *     in `myTestList`. The entry in this case should be:
 *
 *     t_file <test_name> <file_name> <result>
 *     
 *     Where name is the name of the file inside 'input/' with the extension
 *     ".ana". For example:
 *
 *     t_file "myTest" "mytest.ana" "6";
 *)

let f_to_s fname = Runner.string_of_file ("input/" ^ fname)

let t name program expected = name>::test_run program name expected;;
let t_file name program expected = (t name (f_to_s program) expected);;

let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (Runner.parse_string program));;

(* For folding *)
let t_f test_type (name,program,expected) =
  test_type name program expected

let t_err name program expected =
  name>::test_err program name expected

let t_parse_err name program expected =
  name>::test_parse_err program name expected


let myTestList =
  [ (* Fill in your tests here: *)
    t "one" "1" "1";
    t "101" "(let ((x (+ 99 (+ 1 1))) (y 22) (z 11)) x)" "101";
    t "102" "(let ((x (+ 99 (+ 1 1))) (y (+ x 22)) (z 11)) (add1 x))" "102";
    t "99" "(let ((x 99) (y 22) (z 11)) x)" "99";
    t "scope" "(let ((x 99) (y (+ 1 x))) y)" "100";    
    t "big_nest" "(let ((x (+ 1 -1)) (y (let ((a x)) (+ a x)))) (+ x y))" "0";
    t_err "dup_err" "(let ((x 99) (x 22) (z 11)) x)" "Duplicate binding";
    t_err "unbound_err" "(let ((x 99) (y 22) (z 11)) a)" "Unbound variable identifier a";
    (* the remaining tests report incorrectly even though they are patterned off working t_err 
       i.e., dub_err, unbound_err work just fine 
       might have something to do with where (file) the exception is raised *)
    t_err "empty_prog" "()" "Parse error"; 
    t_err "reserved_word" "(let ((let 99) (y 22) (z 11)) y)" "Syntax error: let is a reserved word";
  ]
;;

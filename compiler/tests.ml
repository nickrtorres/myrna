open Re
open Simplify

exception TestFailed of string

let fail expected actual =
  raise
    (TestFailed
       (Printf.sprintf "expected '%s' got '%s'" (debug_string expected)
          (debug_string actual)))

let run_simplify_tests () =
  let concat_null_left () =
    let expected = Atom Null in
    let null_l = simplify (Concatenation (expected, Atom (Char 'c'))) in
    match null_l with Atom Null -> () | actual -> fail expected actual
  in
  let concat_null_right () =
    let expected = Atom Null in
    let null_r = simplify (Concatenation (Atom (Char 'c'), expected)) in
    match null_r with Atom Null -> () | actual -> fail expected actual
  in
  let concat_epsilon_left () =
    let expected = Atom (Char 'c') in
    let epsilon_l = simplify (Concatenation (expected, Atom Epsilon)) in
    match epsilon_l with
    | Atom (Char 'c') -> ()
    | actual -> fail expected actual
  in
  let concat_epsilon_right () =
    let expected = Atom (Char 'c') in
    let epsilon_r = simplify (Concatenation (Atom Epsilon, expected)) in
    match epsilon_r with
    | Atom (Char 'c') -> ()
    | actual -> fail expected actual
  in
  let concat_unaffected () =
    let expected = Concatenation (Atom (Char 'a'), Atom (Char 'b')) in
    let unaffected = simplify expected in
    match unaffected with
    | Concatenation (Atom (Char 'a'), Atom (Char 'b')) -> ()
    | actual -> fail expected actual
  in
  let closure_null () =
    let expected = Atom Epsilon in
    let closure = simplify (Closure (Atom Null)) in
    match closure with Atom Epsilon -> () | actual -> fail expected actual
  in
  let closure_epsilon () =
    let expected = Atom Epsilon in
    let closure = simplify (Closure (Atom Epsilon)) in
    match closure with Atom Epsilon -> () | actual -> fail expected actual
  in
  let closure_unaffected () =
    let expected = Closure (Atom (Char 'a')) in
    let unaffected = simplify expected in
    match unaffected with
    | Closure (Atom (Char 'a')) -> ()
    | actual -> fail expected actual
  in
  concat_null_left ();
  concat_null_right ();
  concat_epsilon_left ();
  concat_epsilon_right ();
  concat_unaffected ();

  closure_null ();
  closure_epsilon ();
  closure_unaffected ()

let run () = run_simplify_tests ()

let () = run ()

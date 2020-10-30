open Re
open Simplify

exception TestFailed of string

let fail actual expected =
  raise
    (TestFailed
       (Printf.sprintf "expected '%s' got '%s'" (debug_string expected)
          (debug_string actual)))

let check actual expected =
  match actual with e when e = expected -> () | _ -> fail actual expected

let run_simplify_tests () =
  let concat_null_left () =
    let expected = Atom Null in
    let actual = simplify (Concatenation (expected, Atom (Char 'c'))) in
    check actual expected
  in
  let concat_null_right () =
    let expected = Atom Null in
    let actual = simplify (Concatenation (Atom (Char 'c'), expected)) in
    check actual expected
  in
  let concat_epsilon_left () =
    let expected = Atom (Char 'c') in
    let actual = simplify (Concatenation (expected, Atom Epsilon)) in
    check actual expected
  in
  let concat_epsilon_right () =
    let expected = Atom (Char 'c') in
    let actual = simplify (Concatenation (Atom Epsilon, expected)) in
    check actual expected
  in
  let concat_unaffected () =
    let expected = Concatenation (Atom (Char 'a'), Atom (Char 'b')) in
    let actual = simplify expected in
    check actual expected
  in
  let closure_null () =
    let expected = Atom Epsilon in
    let actual = simplify (Closure (Atom Null)) in
    check actual expected
  in
  let closure_epsilon () =
    let expected = Atom Epsilon in
    let actual = simplify (Closure (Atom Epsilon)) in
    check actual expected
  in
  let closure_unaffected () =
    let expected = Closure (Atom (Char 'a')) in
    let actual = simplify expected in
    check actual expected
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

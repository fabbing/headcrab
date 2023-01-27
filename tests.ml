open Headcrab.Lambda

let binding_abstraction () =
  let e1 = variable "x" in
  let e2 = abstraction "w" e1 in
  let e3 = abstraction "x" e1 in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2);
  Printf.printf "e3: %s\n" (print e3);

  let e4 = abstraction "w" e2 in
  let e5 = abstraction "x" e2 in
  Printf.printf "e4: %s\n" (print e4);
  Printf.printf "e5: %s\n" (print e5);

  let e6 = Application (variable "x", variable "y") in
  let e7 = abstraction "x" e6 in
  let e8 = abstraction "y" e6 in
  Printf.printf "e7: %s\n" (print e7);
  Printf.printf "e8: %s\n" (print e8)


let slide5 () =
  let e1 = Variable (Free "ID") in
  let e2 = Abstraction ("x", (Variable (Bounded ("x", 1)))) in
  let e3 = Application ((Variable (Free "x")), (Variable (Free "y"))) in
  let e4 = Abstraction ("w", Abstraction ("x", Variable (Free "y"))) in
  let e5 = Abstraction ("x", Application (Variable (Free "y"),
                                          Variable (Free "z")))
  in
  let e6 = Application (
    Variable (Free "foo"),
    Abstraction("bar",
      Application (Variable (Free "foo"),
        Application (Variable (Free "bar"), Variable (Free "baz"))
    )))
  in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2);
  Printf.printf "e3: %s\n" (print e3);
  Printf.printf "e4: %s\n" (print e4);
  Printf.printf "e5: %s\n" (print e5);
  Printf.printf "e6: %s\n" (print e6)


let slide8 () =
  let e1 = Application(
      Application(Variable (Free "x"), Variable (Free "y")),
      Variable (Free "z"))
  in
  let e2 = Application(
      Application(
        Application(Variable (Free "w"), Variable (Free "x")),
        Variable (Free "y")),
      Variable (Free "z"))
  in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2)
  (* TODO more on abstraction parsing *)


let slide16 () =
  let e1 = application
    (variable "x")
    (abstraction "x" (variable "x"))
  in
  let e2 = application
    (abstraction "x" (application
      (variable "x") (variable "y")))
    (variable "x")
  in
  let e3 = abstraction "x"
    (application (variable "x") (variable "y"))
  in
  Printf.printf "e1: %s, is_free x = %b\n" (print e1) (is_free e1 "x");
  Printf.printf "e2: %s, is_free x = %b\n" (print e2) (is_free e2 "x");
  Printf.printf "e3: %s, is_free x = %b\n" (print e3) (is_free e3 "x")


let slide20 () =
  let e1 =
    app
      (app
        (abs
          "x"
          (app
            (app
              (var "x")
              (abs
                "y"
                (app (app (app (var "x") (var "y")) (var "z")) (var "y"))
              )
            )
            (var "x")
          )
        )
        (var "x")
      )
      (var "y")
  in
  let e2 =
    app
      (abs
        "x"
        (abs "y" (app (var "x") (var "y")))
      )
      (abs "z" (app (var "x") (var "z")))
  in
  let e3 = abs "x"
    (
      app
        (var "x")
        (abs "x" (app (var "z") (var "x")))
    )
  in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2);
  Printf.printf "e3: %s\n" (print e3)


let print_grouping () =
  let e1 = abs "x" (var "body") in
  let e2 =
    app
      (abs "x" (var "body"))
      (var "y")
  in
  let e3 =
    app
      (app (var "x") (var "y"))
      (var "z")
  in
  let e4 =
    app
      (var "x")
      (app (var "y") (var "z"))
  in
  let e5 =
    app
      (app
        (var "x")
        (abs "y" (var "body"))
      )
      (var "x")
  in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2);
  Printf.printf "e3: %s\n" (print e3);
  Printf.printf "e4: %s\n" (print e4);
  Printf.printf "e5: %s\n" (print e5)


let slide21 () =
  let e1 = abs "y" (var "y") in
  let e2 = abs "x" (var "x") in
  let e3 = abs "x" (app (var "x") (var "y")) in
  let e4 = abs "y" (app (var "y") (var "x")) in
  let e5 = abs "x" (var "x") in
  let e6 = abs "x" (var "x") in
  Printf.printf "e1: %s, e2: %s are α-equivalent: %b\n" (print e1) (print e2) (alpha_equivalent e1 e2);
  Printf.printf "e3: %s, e4: %s are α-equivalent: %b\n" (print e3) (print e4) (alpha_equivalent e3 e4);
  Printf.printf "e5: %s, e6: %s are α-equivalent: %b\n" (print e5) (print e6) (alpha_equivalent e5 e6)


let slide24 () =
  let e1 = abs "x" (var "x") in
  let r1 = rename e1 "x" "foo" in
  let e2 =
    app
      (app
        (abs
          "x"
          (app
            (app
              (var "x")
              (abs
                "y"
                (app (app (app (var "x") (var "y")) (var "z")) (var "y"))
              )
            )
            (var "x")
          )
        )
        (var "x")
      )
      (var "y")
  in
  let r2 = rename e2 "x" "bar" in
  Printf.printf "e1: %s {foo/x} → %s\n" (print e1) (print r1);
  Printf.printf "e2: %s {bar/x} → %s\n" (print e2) (print r2)


let slide29 () =
  let e1 = abs "x" (var "x") in
  let s1 = substitute e1 "x" (var "foo") in
  Printf.printf "e1: %s [x→foo] → %s\n" (print e1) (print s1);
  let e2 = app (app (var "+") (var "1")) (var "x") in
  let s2 = substitute e2 "x" (var "2") in
  Printf.printf "e2: %s [x→2] → %s\n" (print e2) (print s2);
  let e3 = abs "x" (app (var "y") (var "x")) in
  let r3 = abs "z" (app (var "x") (var "z")) in
  let s3 = substitute e3 "y" r3 in
  Printf.printf "e3: %s [y→...] → %s\n" (print e3) (print s3);
  let e4 = app (var "x") (abs "y" (app (var "x") (var "y"))) in
  let r4 = app (var "y") (var "z") in
  let s4 = substitute e4 "x" r4 in
  Printf.printf "e4: %s [x→...] → %s\n" (print e4) (print s4)


let slide32_33 () =
  let e1 = app (abs "x" (var "x")) (var "y") in
  let n1 = execute e1 in
  Printf.printf "e1: %s	→	%s\n\n" (print e1) (print n1);
  Printf.printf "----\n";

  let e2 = app
    (abs "x" (app (var "x") (abs "x" (var "x"))))
    (app (var "u") (var "r"))
  in
  let n2 = execute e2 in
  Printf.printf "e2: %s	→	%s\n\n" (print e2) (print n2);
  Printf.printf "----\n";

  let e3 = app
    (abs "x" (var "y"))
    (app
      (abs "z" (app(var "z") (var "z")))
      (abs "w" (var "w")))
  in
  let n3 = execute e3 in
  Printf.printf "e3: %s	→	%s\n\n" (print e3) (print n3);
  Printf.printf "----\n"

let slide34 () =
  let e = app
    (abs "x" (app (var "x") (var "x")))
    (abs "x" (app (var "x") (var "x")))
  in
  let loop = execute e in
  Printf.printf "e: %s	→	%s\n" (print e) (print loop);
  Printf.printf "----\n"


let execute_rec () =
  let e1 = app (app (var "x") (var "y")) (var "z") in
  let e2 = app (var "x") (app (var "y") (var "z")) in
  let e3 = app
    (app
      (abs "x" (var "x"))
      (abs "y" (var "y")))
    (var "z")
  in
  let f1 = execute e1 in
  Printf.printf "e1: %s	→	%s\n" (print e1) (print f1);
  Printf.printf "----\n";
  let f2 = execute e2 in
  Printf.printf "e2: %s	→	%s\n" (print e2) (print f2);
  Printf.printf "----\n";
  let f3 = execute e3 in
  Printf.printf "e3: %s	→	%s\n" (print e3) (print f3)


let slide35_39 () =
  let l_true = abs "x" (abs "y" (var "x")) in
  let l_false = abs "x" (abs "y" (var "y")) in
  let l_and = abs "a" (abs "b"
    (app
      (app (var "a") (var "b"))
      l_false)
    )
  in
  Printf.printf "true: %s\n" (print l_true);
  Printf.printf "false: %s\n" (print l_false);
  Printf.printf "and: %s\n" (print l_and);
  Printf.printf "----\n";

  Printf.printf "and TT\n";
  let e = app (app l_and l_true) l_true in
  Printf.printf "e: %s\n" (print e);
  let r = execute e in
  Printf.printf "r: %s\n" (print r);
  Printf.printf "----\n";

  Printf.printf "and TF\n";
  let e = app (app l_and l_true) l_false in
  Printf.printf "e: %s\n" (print e);
  let r = execute e in
  Printf.printf "r: %s\n" (print r);
  Printf.printf "----\n";

  Printf.printf "and FT\n";
  let e = app (app l_and l_false) l_true in
  Printf.printf "e: %s\n" (print e);
  let r = execute e in
  Printf.printf "r: %s\n" (print r);
  Printf.printf "----\n";

  Printf.printf "and FF\n";
  let e = app (app l_and l_false) l_false in
  Printf.printf "e: %s\n" (print e);
  let r = execute e in
  Printf.printf "r: %s\n" (print r);
  Printf.printf "----\n"


let slide40 () =
  let l_true = abs "x" (abs "y" (var "x")) in
  let l_false = abs "x" (abs "y" (var "y")) in
  let l_not = abs "a" (app (app (var "a") l_false) l_true) in
  Printf.printf "not: %s\n" (print l_not);
  Printf.printf "----\n";

  Printf.printf "not F\n";
  let e = app l_not l_false in
  let r = execute e in
  Printf.printf "%s	→	%s\n" (print e) (print r);
  Printf.printf "----\n";

  Printf.printf "not T\n";
  let e = app l_not l_true in
  let r = execute e in
  Printf.printf "%s	→	%s\n" (print e) (print r);
  Printf.printf "----\n"

let () =
  (*
  let ignore_exn f =
    try f () with
    | e -> (Printf.printf "exn: %s\n" (Printexc.to_string e);
      Printf.printf "----\n")
  in
  *)
  let l_true = abs "x" (abs "y" (var "x")) in
  Format.fprintf Format.std_formatter "true = %a\n%!" pp l_true

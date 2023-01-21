open Headcrab.Lambda

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


let bounding_abstraction () =
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


let () = slide16 ()

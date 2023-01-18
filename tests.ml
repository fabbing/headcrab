open Headcrab.Lambda

let slide5 () =
  let e1 = Variable (Free "ID") in
  let e2 = Abstraction ("x", (Variable (Bounded "x"))) in
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


let bounding_abstraction () =
  let e1 = make_variable "x" in
  let e2 = make_abstraction "w" e1 in
  let e3 = make_abstraction "x" e1 in
  Printf.printf "e1: %s\n" (print e1);
  Printf.printf "e2: %s\n" (print e2);
  Printf.printf "e3: %s\n" (print e3);

  let e4 = make_abstraction "w" e2 in
  let e5 = make_abstraction "x" e2 in
  Printf.printf "e4: %s\n" (print e4);
  Printf.printf "e5: %s\n" (print e5);

  let e6 = Application (make_variable "x", make_variable "y") in
  let e7 = make_abstraction "x" e6 in
  let e8 = make_abstraction "y" e6 in
  Printf.printf "e7: %s\n" (print e7);
  Printf.printf "e8: %s\n" (print e8)


let () =
  bounding_abstraction ()

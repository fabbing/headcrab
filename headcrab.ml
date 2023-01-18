module Lambda = struct
  type variable =
    (*
      A variable is free if it doesn't appear within the body of an
      absantraction with a metavariable of the same name.

      x is free in E if
        - E = x
        - E = λ y . E₁, where y ≠ x and x is free in E₁
        - E = E₁ E₂, where x is free in E₁
        - E = E₁ E₂, where x is free in E₂
     *)
    | Free of string
    (*
     If an occurrence of x is free in E,
      then it's bound by λ x . in λ x . E

     If an occurrence of x is bound by a particular λ x . in E,
      then x is bound by the same λ x . in λ z. E
      even if z == x

     If an occurrence of x is bound by a particular λ x . in E₁,
      then that occurrence in E₁ is tied by the same abstraction λ x .
      in E₁ E₂ and E₂ E₁
     *)
    | Bounded of string

  type metavariable = string

  type expression =
    (* E -> ID *)
    | Variable of variable
    (*
      E -> λ ID . E

      The body expands as far to the right as possible.
       - ID is the variable or metavariable.
       - E is the body
     *)
    | Abstraction of metavariable * expression
    (* E -> E E

       Left associative
     *)
    | Application of expression * expression


  let make_variable name = Variable (Free name)

  let make_abstraction meta body =
    let rec bind = function
      | Variable bound as var -> begin
        match bound with
        | Free name -> if name = meta then Variable (Bounded name) else var
        | Bounded n -> var
        end
      | Abstraction (m, body) -> Abstraction (m, bind body)
      | Application (e1, e2) -> Application (bind e1, bind e2)
    in
    Abstraction (meta, bind body)


  let rec print = function
    | Variable var -> begin
      match var with
      | Free name -> Printf.sprintf "%s" name
      | Bounded name -> Printf.sprintf "_%s" name
      end
    | Abstraction (meta, expr) -> Printf.sprintf "λ _%s . %s" meta (print expr)
    | Application (e1, e2) -> begin
      match e2 with
      | Application _ -> Printf.sprintf "%s (%s)" (print e1) (print e2)
      | _ -> Printf.sprintf "%s %s" (print e1) (print e2)
      end


  (* An expression is a combinator if it does not have any free varaibles *)
  let rec is_combinator = function
    | Variable var -> begin
      match var with | Free _ -> false | Bounded _ -> true
      end
    | Abstraction (_, expr) -> is_combinator expr
    | Application (expr1, expr2) -> is_combinator expr1 && is_combinator expr2
end

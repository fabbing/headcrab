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

     The int encode the De Bruijn index
     *)
    | Bounded of string * int

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


  let variable name = Variable (Free name)

  let abstraction meta body =
    let rec bind depth expr = match expr with
      | Variable binding as var -> begin
        match binding with
        | Free name when name = meta -> Variable (Bounded (name, depth))
        | _ -> var
        end
      | Abstraction (m, b) -> Abstraction (m, bind (depth + 1) b)
      | Application (e1, e2) -> Application (bind depth e1, bind depth e2)
    in
    Abstraction (meta, bind 1 body)

  let application e1 e2 = Application (e1, e2)


  let rec print = function
    | Variable var -> begin
      match var with
      | Free name -> Printf.sprintf "%s" name
      | Bounded (name, index) -> Printf.sprintf "_%d_%s" index name
      end
    | Abstraction (meta, body) -> Printf.sprintf "λ _%s . %s" meta (print body)
    | Application (e1, e2) -> begin
      match e2 with
      | Application _ -> Printf.sprintf "%s (%s)" (print e1) (print e2)
      | _ -> Printf.sprintf "%s %s" (print e1) (print e2)
      end


  let rec is_free expr var =
    match expr with
    | Variable v -> (match v with | Free name when name = var -> true | _ -> false)
    | Abstraction (meta, body) -> if meta <> var then is_free body var else false
    | Application (e1, e2) -> is_free e1 var || is_free e2 var


  (* An expression is a combinator if it does not have any free varaibles *)
  let rec is_combinator = function
    | Variable var -> (match var with | Free _ -> false | Bounded _ -> true)
    | Abstraction (_, body) -> is_combinator body
    | Application (e1, e2) -> is_combinator e1 && is_combinator e2
end

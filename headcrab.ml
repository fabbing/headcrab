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
    (* TODO E -> (E) *)

  (* An expression is a combinator if it does not have any free varaibles *)
  let is_combinator = failwith("not implemented")
end

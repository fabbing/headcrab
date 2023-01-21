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

  let var = variable
  let abs = abstraction
  let app = application


  let rec print = function
    | Variable var -> begin
      match var with
      | Free name -> Printf.sprintf "%s" name
      | Bounded (name, index) -> Printf.sprintf "_%s.%d" name index
      end
    | Abstraction (meta, body) -> Printf.sprintf "(λ _%s . %s)" meta (print body)
    | Application (e1, e2) -> begin
      match e1, e2 with
      | _, Application _ -> Printf.sprintf "%s (%s)" (print e1) (print e2)
      | _ -> Printf.sprintf "%s %s" (print e1) (print e2)
      end


  let rec is_free expr var =
    match expr with
    | Variable v ->
        (match v with | Free name when name = var -> true | _ -> false)
    | Abstraction (meta, body) ->
        if meta <> var then is_free body var else false
    | Application (e1, e2) -> is_free e1 var || is_free e2 var


  (* An expression is a combinator if it does not have any free varaibles *)
  let rec is_combinator = function
    | Variable var -> (match var with | Free _ -> false | Bounded _ -> true)
    | Abstraction (_, body) -> is_combinator body
    | Application (e1, e2) -> is_combinator e1 && is_combinator e2


  (*
    α-equivalence is when two functions vary only by the names of the bound
    variables.

    E₁ =α= E₂
  *)
  let rec alpha_equivalent expr1 expr2 =
    match expr1, expr2 with
    | Variable (Free v1), Variable (Free v2) when v1 = v2 -> true
    | Variable (Bounded (_, i1)), Variable (Bounded (_, i2)) when i1 = i2 ->
        true
    | Abstraction (_, b1), Abstraction (_, b2) -> alpha_equivalent b1 b2
    | Application (a1e1, e1e2), Application (a2e1, a2e2) ->
        (alpha_equivalent a2e1 a2e1) && (alpha_equivalent e1e2 a2e2)
    | _ -> false

  (*
    Renaming operation

    E {y/x}
      - x {y/x} = y
      - z {y/x} = z, if x ≠ z
      - (E₁ E₂) {y/x} = (E₁ {y/x}) (E₂ {y/x})
      - (λ x. E) {y/x} = (λ y . E {y/x})
      - (λ z. E) {y/x} = (λ z . E {y/x}), if x ≠ z
  *)
  let rename expr src dst =
    let rec aux expr =
      match expr with
      | Variable binding as var-> begin
        match binding with
        | Free name when name = src -> Variable (Free dst)
        | Bounded (name, index) when name = src ->
            Variable (Bounded (dst, index))
        | _ -> var
        end
      | Abstraction (meta, body) ->
        Abstraction ((if meta = src then dst else meta), (aux body))
      | Application (e1, e2) -> Application ((aux e1), (aux e2))
    in
    aux expr
end

type expression = 
  | Value of string
  | True
  | False
  | Negation of expression
  | Or  of expression * expression
  | And of expression * expression
  | Implication of expression * expression
  | Equivalence of expression * expression


let rec simplify exp =
  match exp with
  | Value a -> Value a
  | True -> True
  | False -> False
  | Or (True,_) -> True
  | Or  (a, b) when (Negation a = b) -> True
  | And (a, b) when (Negation a = b) -> False
  | And (False,_) -> False
  | Or (a,b) | And (a,b) | Implication (a,b) | Equivalence (a,b) when (a = b) -> simplify a
  | Or (a,b) -> Or(simplify a, simplify b)
  | And (a,b) -> And(simplify a, simplify b)
  | Implication (a,b) -> Or(simplify (Negation a), simplify b)
  | Equivalence (a,b) -> Or(simplify (Implication(a,b)), simplify (Implication(b,a)))
  | Negation a ->
    match a with
     | True -> False
     | False -> True
     | Negation b -> simplify b 
     | Or (a,b) -> And (simplify (Negation(a)),simplify (Negation(b)))
     | And (a,b) -> Or (simplify (Negation(a)),simplify (Negation(b)))
     | _ -> Negation (simplify a)



let exp_to_text exp =
 let rec evaluate exp value =
   match exp with
   | Value a           ->  a
   | True -> "True"
   | False -> "False"
   | Negation a        -> "!" ^ evaluate a value
   | Or (a,b)          -> "("^ evaluate a value ^ " v " ^ evaluate b value ^ ")"
   | And (a,b)         -> "("^ evaluate a value ^ " ^ " ^ evaluate b value ^ ")"
   | Implication (a,b) -> "("^ evaluate a value ^ " => " ^ evaluate b value ^ ")"
   | Equivalence (a,b) -> "("^ evaluate a value ^ " <=> " ^ evaluate b value ^ ")"
 in evaluate exp ""  
    

let exp1 = 
  Negation(
    Negation(
      Implication (
        Implication(
          Negation(
            Or(Value("b"),Value("b"))
          ),
          And(Value("a"),Value("a"))
        ),
        Equivalence(
          Or(
            Negation(
              Value("c")
            ),
            Value("c")
          ),
          And(
            Value("a"),
            Negation(
              Value("b")
            )
          )
        )
      )
    )
  )

let exp2 = Negation(Value("a"))
let exp3 =  Or(Or(Value("a"),Value("a")),Or(Value("a"),Value("a")))
let exp4 = Or(
  Negation(
    Value("c")
    ),
  Value("c")
  )

let _ = exp1 |>  simplify |> exp_to_text |> print_endline
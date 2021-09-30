type Variable = string
type Name = string

type Term = 
    | Variable of Name
    | UnificationTerm of Term
    | ForAll of Variable * Term
    | ThereExists of Variable * Term 
    | Implies of Term * Term 
    | Or of Term * Term 
    | And of Term * Term 
    | Not of Term 
    | Function of Name * Term list 
    | Predicate of Name * Term list

let rec freeVariables term = 
    match term with
    | Variable(_) as v -> Set.of [v]
    | UnificationTerm(_) -> Set.empty
    | Function(_, terms) -> freeVarsFromList terms
    | Predicate(_, terms) -> freeVarsFromList terms
    | Not(terms) -> freeVariables terms
    | And(left, right) -> freeVariables [left; right]
    | Or(left, right) -> freeVariables [left; right]
    | Implies(left, right) -> freeVariables [left; right]
    | ForAll(v, term) -> Set.remove v freeVariables
    | ThereExists(v, term) -> Set.remove v freeVariables
and freeVarsFromList terms = 
        Set.of [for t in terms do
                  for v in freeVariables t do
                    yield v ]

let (==>) = Implies


// Useful terms to keep handy 
let P(x) = Predicate("P", x)
let Q(x) = Predicate("Q", x)
let p = Variable("P")
let x = Variable("x")
let y = Variable("x")

let examples = [
    //  P or not P
    Or(p, Not(p))
    //  forall x. P(x) implies (Q(x) implies P(x))
    ForAll("x", P(x) ==> (Q(x) => P(x)))
    // exists x. (P(x) implies forall y. P(y))
    Exists("x", P(x) ==> ForAll("y", P(y)))
]
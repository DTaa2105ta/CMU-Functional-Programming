datatype exp = Var of string
             | Int of int
             | Add of exp * exp
             | Mul of exp * exp
             | Not of exp
             | IfThenElse of exp * exp * exp;
type environ = (string * int) list;

(* lookup (env, x) ==> v
 * REQUIRES: (x, v) is in env, for some v
 * ENSURES: returns v
 *)
fun lookup ([] : environ, y : string) = raise Fail "REQUIRES violated"
  | lookup ((x,v)::xs, y) = if x = y then v else lookup (xs,y);

(* vars e ==> l
 * REQUIRES: true
 * ENSURES: l contains all vars in e
 *)
fun vars (Var x)              = [x]
  | vars (Int n)              = []
  | vars (Add (a,b))          = vars a @ vars b
  | vars (Mul (a,b))          = vars a @ vars b
  | vars (Not a)              = vars a
  | vars (IfThenElse (i,t,e)) = vars i @ vars t @ vars e;

(* eval (env, e) ==> n
 * REQUIRES: (x, v) is in env for all x in (vars e)
 * ENSURES: n is the integer representing the 
 *          value of e given environment env
 *)

(* fun eval (env : environ, e : exp) : int = raise Fail "Unimplemented" *)

fun eval (env : environ, e : exp) : int = 
  case e
    of Var str => lookup (env, str)
     | Int num => num
     | Add (a, b) => eval(env, a) + eval(env, b)
     | Mul (a, b) => eval(env, a) * eval(env, b)
     | Not a => if eval(env, a) = 0 then 1 else 0 
     | IfThenElse (i, t, e) => if eval(env, i) <> 0 then eval(env, t) else eval(env, e);

(* Basic BTest *)
val 150 = eval ([("x", 45) ,("y", 3)] , Add (Int 15 , Mul(Var "x" , Var "y")));
val 1 = eval ([("x", 0)], Not (Var "x"));
val 0 = eval ([("x", 0)], Not (Not (Var "x")));
val 0 = eval ([("x", 3)], Not (Var "x"));
val 999 = eval ([("x", 999)], IfThenElse (Int 3, Var "x", Int 0));
(* Test cases for Not and IfThenElse *)
(* 1. Double negation with variable *)
val 1 = eval ([("x", 5)], Not (Not (Var "x")));  (* 5 -> not(0) -> 1 *)

(* 2. Not with arithmetic expression *)
val 1 = eval ([("x", 2)], Not (Add (Var "x", Int (~2))));  (* 2 + (-2) = 0 -> not(0) = 1 *)

(* 3. Nested IfThenElse with Not *)
val 10 = eval ([("x", 0), ("y", 10)], 
    IfThenElse (Not (Var "x"), Var "y", Int 20));  (* not(0) = 1 -> choose y *)

(* 4. IfThenElse where condition is another IfThenElse *)
val 100 = eval ([("x", 0), ("y", 1)],
    IfThenElse (
        IfThenElse (Var "x", Int 0, Not (Var "y")),  (* x=0 -> not(y)=0 *)
        Int 50,
        Int 100
    ));

(* 5. Complex combination of Not and arithmetic *)
val 1 = eval ([("x", 1), ("y", 0)],
    Not (Mul (Add (Var "x", Int (~1)), Var "y")));  (* (1 + (-1)) * 0 = 0 -> not(0) = 1 *)

(* 6. Not with IfThenElse inside *)
val 1 = eval ([("x", 0)],
    Not (IfThenElse (Var "x", Int 1, Int 0)));  (* x=0 -> choose 0 -> not(0) = 1 *)

(* 7. Multiple nested IfThenElse *)
val 42 = eval ([("x", 1), ("y", 0)],
    IfThenElse (
        Not (Var "y"),
        IfThenElse (Var "x", Int 42, Int 24),
        Int 0
    ));

(* 8. Not applied to a comparison-like expression *)
val 1 = eval ([("x", 5)],
    Not (Add (Var "x", Mul (Int (~1), Var "x"))));  (* 5 + (-5) = 0 -> not(0) = 1 *)

(* 9. IfThenElse where both branches contain Not *)
val 0 = eval ([("x", 0)],
    IfThenElse (
        Var "x",
        Not (Int 0),  (* would be 1 *)
        Not (Int 1)   (* 0 *)
    ));

(* 10. Deeply nested Not and arithmetic *)
val 1 = eval ([("x", 1), ("y", 1)],
    Not (Add (
        Mul (Var "x", Int (~1)),
        IfThenElse (
            Not (Var "y"),
            Int 10,
            Var "x"  (* chosen *)
        )
    )));  (* -1 + 1 = 0 -> not(0) = 1 *)
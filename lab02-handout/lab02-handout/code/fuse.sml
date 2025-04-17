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

fun eval (env : environ, e : exp) : int = 
  case e
    of Var str => lookup (env, str)
     | Int num => num
     | Add (a, b) => eval(env, a) + eval(env, b)
     | Mul (a, b) => eval(env, a) * eval(env, b)
     | Not a => if eval(env, a) = 0 then 1 else 0 
     | IfThenElse (i, t, e) => if eval(env, i) <> 0 then eval(env, t) else eval(env, e);
     
(* Theorem: eval (env, fuse e) = eval (env, e) *)
fun fuse (Var x : exp) : exp = Var x
  | fuse (Int n) = Int n
  | fuse (Add (a,b)) = Add (fuse a, fuse b)
  | fuse (Mul (a,b)) = (
      case (vars a,vars b) of
        ([],[]) => Int (eval ([],a) * eval ([],b))
      | _       => Mul (fuse a,fuse b)
    )
  | fuse (Not a) = Not (fuse a)
  | fuse (IfThenElse (i,t,e)) = IfThenElse (fuse i,fuse t,fuse e);

val Add (Var "x",Int 6) = fuse (Add (Var "x", Mul (Int 2,Int 3)));

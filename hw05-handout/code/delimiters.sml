Control.Print.printLength := 100;
Control.Print.printDepth := 100;

(* Left and right parentheses *)
datatype par = LPAR | RPAR
type pList = par list

(* Parse tree for parentheses *)
datatype pTree = empty                (* no parentheses *)
               | nested of pTree      (* nested parentheses *)
               | sbs of pTree * pTree (* side by side *)

(* Stack of either open parentheses or parse trees *)
datatype stackItem = OPEN
                   | T of pTree
(*
OPEN: open parentheses that we have not closed yet
T: pTree's that we have completely recognized
*)
type stack = stackItem list
(********************* Begin utility functions *********************)

(* pList_fromString s ==> ps
 * ENSURES: convert the string of parentheses s into the corresponding
            entity ps of type pList; raises an exception if s
            contains characters other than "(" and ")".
 *)
fun pList_fromString (s: string): pList =
  let
    fun pts ([]: char list): pList = []
      | pts (#"("::l) = LPAR :: pts l
      | pts (#")"::l) = RPAR :: pts l
      | pts ps = raise Fail ("Bad parentheses string starting at " ^
                              String.implode ps)
  in
    pts (String.explode s)
  end

(* pList_toString pl ==> s
 * ENSURES: converts pl into a string
 *)
fun pList_toString ([]: pList): string = ""
  | pList_toString (LPAR::ps) = "(" ^ pList_toString ps
  | pList_toString (RPAR::ps) = ")" ^ pList_toString ps

(* pTree_toString pt ==> s
 * ENSURES: converts the given parse tree pt into a string
 *)
fun pTree_toString (empty: pTree): string = ""
  | pTree_toString (nested t) = "(" ^ pTree_toString t ^ ")"
  | pTree_toString (sbs (t1,t2)) = pTree_toString t1 ^
                                   pTree_toString t2

(* stack_toString st ==> s
 * ENSURES: converts the given stack st into a string
 *)
fun stack_toString ([]: stack): string = "#"
  | stack_toString (OPEN::S) = "( | " ^ stack_toString S
  | stack_toString (T t::S)  = pTree_toString t ^ " | " ^
                               stack_toString S

(********************** End utility functions **********************)
fun valid (parList: pList): bool = 
  let
    fun valid' ([]: pList, acc: int): int = acc
      | valid' (RPAR :: ps, acc) = valid' (ps, acc - 1)
      | valid' (LPAR :: ps, acc) = 
        case acc < 0
          of true => valid' (ps, acc)
           | false => valid' (ps, acc + 1)
  in
    valid' (parList, 0) = 0
  end
(*
flattenPTree: pTree -> pList
ENSURES: converts a pTree to the corresponding pList
*)
fun flattenPTree (pt: pTree): pList = pList_fromString (pTree_toString pt)
(**)
fun preProcessStack (S: stack): stack =
  case S of
    T (t2) :: T (t1) :: ss => preProcessStack(T (sbs (t1, t2)) :: ss)
  | _ => S
(* Process the stack when we see a right parenthesis *)
fun processRPAR ([]: stack): stack = raise Fail "Not as task's assumption"
	| processRPAR (OPEN :: ss) = T (nested empty) :: ss
	| processRPAR (T (t) :: OPEN :: ss) = T (nested (t)) :: ss
(* Process the final T pTree stack to produce a single pTree *)
fun finalizeStack ([]: stack): pTree = empty
  | finalizeStack (T t :: []) = t
  | finalizeStack (T t2 :: T t1 :: ss) = finalizeStack (T (sbs(t1, t2)) :: ss)
(* Main parsing function *)
fun pp (ps: pList, S: stack): pTree =
  case ps of
    [] => finalizeStack S
  | LPAR :: rest => pp(rest, OPEN :: S)
  | RPAR :: rest => pp(rest, processRPAR (preProcessStack S))
		
fun parsePar (parList: pList): pTree = pp (parList, [])
(******************************* BONUS PART ******************************)

(* Left and right delimiters *)
datatype par2 = L of string | R of string
type pList2 = par2 list

(* Parse tree for parentheses *)
datatype pTree2 = empty2                     (* no parentheses *)
                | nested2 of string * pTree2 (* nested parentheses *)
                | sbs2 of pTree2 * pTree2    (* side by side *)

(* Stack of either open parentheses or parse trees *)
datatype stackItem2 = OPEN2 of string
                    | T2 of pTree2
type stack2 = stackItem2 list


(********************* Begin utility functions *********************)

(* pList2_fromString s ==> pl2
 * ENSURES: convert the string s of delimiters of the form "(s" or
            ")s" where s is any parenthesis-free string into the
            corresponding entity pl2 of type pList2.
 *)
fun pList2_fromString (s: string): pList2 =
  let
    datatype del = Ld (* left delimiter marker *)
                 | Rd (* right delimiter marker *)
    fun parsed (Ld, acc) = L (String.implode (rev acc))
      | parsed (Rd, acc) = R (String.implode (rev acc))
    fun scan ([]: char list, w: del * char list): pList2 = [parsed w]
      | scan (#"("::cs, w) = (parsed w) :: scan (cs, (Ld,[]))
      | scan (#")"::cs, w) = (parsed w) :: scan (cs, (Rd,[]))
      | scan (c::cs, (d,acc)) = scan (cs, (d, c::acc))
    fun pfs ([]: char list): pList2 = []
      | pfs (#"("::cs) = scan (cs, (Ld, []))
      | pfs (#")"::cs) = scan (cs, (Rd, []))
      | pfs cs = raise Fail ("Malformed delimiter string " ^ String.implode cs)
  in
    pfs (String.explode s)
  end

(* pList2_toString pl2 ==> s
 * ENSURES: converts the given pl2 into a string
 *)
fun pList2_toString ([]: pList2): string = ""
  | pList2_toString ((L s)::ps) = "(" ^ s ^ pList2_toString ps
  | pList2_toString ((R s)::ps) = ")" ^ s ^ pList2_toString ps

(* pTree2_toString pt2 ==> s
 * ENSURES: converts the given parse tree pt2 into a string
 *)
fun pTree2_toString (empty2: pTree2): string = ""
  | pTree2_toString (nested2 (s,t)) = "(" ^ s ^ pTree2_toString t ^ ")" ^ s
  | pTree2_toString (sbs2 (t1,t2)) = pTree2_toString t1 ^
                                     pTree2_toString t2

(* stack2_toString st2 ==> s
 * ENSURES: converts the given st2 into a string
 *)
fun stack2_toString ([]: stack2): string = "#"
  | stack2_toString ((OPEN2 s)::S) = "(" ^ s^ " | " ^ stack2_toString S
  | stack2_toString (T2 t::S)  = pTree2_toString t ^ " | " ^
                                 stack2_toString S

(********************** End utility functions **********************)
fun valid2(ps: pList2): bool = 
	let
		fun valid2'([]: pList2, hStack: pList2): bool = true
			| valid2'(L (lstr) :: pss, hStack) = valid2'(pss, L (lstr) :: hStack)
			| valid2'(R (rstr) :: pss, L (lstr) :: rest) =
				case rstr = lstr 
          of false => false
					 | true => valid2'(pss, rest)
	in
		valid2'(ps, [])
	end

fun flattenPTree2 (pt: pTree2): pList2 = pList2_fromString (pTree2_toString pt)

fun preProcessStack2 (S: stack2): stack2 =
  case S of
  T2 (t2) :: T2 (t1) :: ss => preProcessStack2(T2 (sbs2 (t1, t2)) :: ss)
  | _ => S
(* Process the stack when we see a right parenthesis *)
fun processRPAR2 ([]: stack2): stack2 = raise Fail "Not as task's assumption"
	| processRPAR2 ((OPEN2 str) :: ss) = (T2 (nested2 (str, empty2)) :: ss)
	| processRPAR2 ((T2 t) :: (OPEN2 str) :: ss) = T2 (nested2 (str, t)) :: ss
(* Process the final T pTree stack to produce a single pTree *)
fun finalizeStack2 ([]: stack2): pTree2 = empty2
  | finalizeStack2 ((T2 t) :: []) = t
  | finalizeStack2 ((T2 t2) :: (T2 t1) :: ss) = finalizeStack2 (T2 (sbs2(t1, t2)) :: ss)
(* Main parsing function *)
fun pp2 (ps: pList2, S: stack2): pTree2 =
  case ps of
    [] => finalizeStack2 S
  | (L lstr) :: rest => pp2(rest, (OPEN2 lstr) :: S)
  | (R rstr) :: rest => pp2(rest, processRPAR2 (preProcessStack2 S))

fun parsePar2 (parList2: pList2): pTree2 = pp2 (parList2, [])



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

fun valid        _ = raise Fail "Unimplemented"
fun flattenPTree _ = raise Fail "Unimplemented"
fun pp           _ = raise Fail "Unimplemented"
fun parsePar     _ = raise Fail "Unimplemented"




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

fun valid2        _ = raise Fail "Unimplemented"
fun flattenPTree2 _ = raise Fail "Unimplemented"
fun pp2           _ = raise Fail "Unimplemented"
fun parsePar2     _ = raise Fail "Unimplemented"


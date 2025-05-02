Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

val test1 = pp(pList_fromString "()", []) = nested empty

val test2 = pp(pList_fromString ")", [OPEN]) = nested empty

val test3 = pp(pList_fromString ")", 
            [T(nested empty), OPEN]) = nested (nested empty)

val test5 = pp(pList_fromString "())", 
            [OPEN]) = nested (nested empty)

val test6 = pp(pList_fromString ")", 
            [T(nested empty), OPEN]) = nested (nested empty)

val test7 = pp(pList_fromString ")", 
            [T(nested(nested(nested empty))), OPEN]) = 
            nested(nested(nested(nested empty)))

val test8 = pp(pList_fromString "())", 
            [T(nested empty), OPEN]) = 
            nested(sbs(nested empty, nested empty))

val test9 = pp(pList_fromString ")()", 
            [OPEN, T(nested empty), T(nested empty)]) = 
            sbs(nested empty, sbs(nested empty, sbs(nested empty, nested empty)))

val test10 = pp(pList_fromString "))", 
             [T(nested(nested empty)), T(nested empty), OPEN, OPEN]) = 
             nested(nested(sbs(nested empty, nested(nested empty))))

val test11 = pp([], [T(nested empty), T(nested empty), T(nested empty)]) = 
             sbs(nested empty, sbs(nested empty, nested empty))

val test12 = pp([], [T(nested empty), T(nested empty), 
                     T(nested empty), T(nested empty)]) = 
             sbs(nested empty, sbs(nested empty, sbs(nested empty, nested empty)))

val test13 = pp([], [T(nested(nested empty)), T(nested empty), 
                     T(nested(nested empty))]) = 
             sbs(nested(nested empty), sbs(nested empty, nested(nested empty)))

val test14 = pp(pList_fromString ")", 
               [T(sbs(nested empty, nested empty)), 
                T(nested(nested empty)), 
                T(nested empty), 
                OPEN]) = 
             nested(sbs(nested empty, sbs(nested(nested empty), 
                                        sbs(nested empty, nested empty))))

val test15 = pp([LPAR, RPAR, RPAR], 
                [T(nested empty), OPEN, T(nested empty), T(nested empty)]) = 
                sbs(nested empty, sbs(nested empty, nested(sbs(nested empty, nested empty))))
(* Ordinary test case *)
val test16 = pp(pList_fromString ")(())", [OPEN]) = 
                sbs (nested empty,nested (nested empty)) (* ()(()) *)

(* Simple pass/fail output *)
val _ = app (fn (name, test) => 
    print ((if test then "PASS: " else "FAIL: ") ^ name ^ "\n"))
    [("test1", test1),
     ("test2", test2),
     ("test3", test3),
     ("test5", test5),
     ("test6", test6),
     ("test7", test7),
     ("test8", test8),
     ("test9", test9),
     ("test10", test10),
     ("test11", test11),
     ("test12", test12),
     ("test13", test13),
     ("test14", test14),
     ("test15", test15),
     ("test16", test16)]
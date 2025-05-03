Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function for comparing pList2s *)
fun equalPList2s ([], []) = true
  | equalPList2s ((L s1)::ps1, (L s2)::ps2) = s1 = s2 andalso equalPList2s (ps1, ps2)
  | equalPList2s ((R s1)::ps1, (R s2)::ps2) = s1 = s2 andalso equalPList2s (ps1, ps2)
  | equalPList2s _ = false

(* Test runner with detailed output *)
fun runTest (testName: string, input: pTree2, expected: pList2) =
    let
        val result = flattenPTree2 input
        val passed = equalPList2s (result, expected)
        val message = testName ^ ":\n" ^
                     "  Input tree: " ^ pTree2_toString input ^ "\n" ^
                     "  Expected list: " ^ pList2_toString expected ^ "\n" ^
                     "  Got list: " ^ pList2_toString result ^ "\n" ^
                     "  Result: " ^ (if passed then "PASSED" else "FAILED") ^ "\n\n"
    in
        print message;
        passed
    end

val test_flattenPTree2 = 
  let
    (* Basic Cases *)
    val test1 = runTest("Empty tree",
                       empty2,
                       [])

    val test2 = runTest("Single nested tag",
                       nested2("tag", empty2),
                       pList2_fromString "(tag)tag")

    (* LaTeX Environment Cases *)
    val test3 = runTest("LaTeX center environment",
                       nested2("center", empty2),
                       pList2_fromString "(center)center")

    val test4 = runTest("Nested LaTeX environments",
                       nested2("document",
                             nested2("center", empty2)),
                       pList2_fromString "(document(center)center)document")

    (* Complex Nesting *)
    val test5 = runTest("Multiple side-by-side environments",
                       sbs2(nested2("equation", empty2),
                            nested2("align", empty2)),
                       pList2_fromString "(equation)equation(align)align")

    val test6 = runTest("Deep nested environments",
                       nested2("document",
                             nested2("chapter",
                                   nested2("section",
                                         nested2("theorem", empty2)))),
                       pList2_fromString "(document(chapter(section(theorem)theorem)section)chapter)document")

    (* Mixed Complex Cases *)
    val test7 = runTest("Mixed nesting and side-by-side",
                       sbs2(nested2("abstract", 
                                  nested2("theorem", empty2)),
                            nested2("proof", empty2)),
                       pList2_fromString "(abstract(theorem)theorem)abstract(proof)proof")

    (* LaTeX Special Cases *)
    val test8 = runTest("Complex LaTeX structure",
                       nested2("begin{document}",
                             sbs2(nested2("begin{center}", empty2),
                                  nested2("begin{equation}", 
                                        nested2("begin{align}", empty2)))),
                       pList2_fromString "(begin{document}(begin{center})begin{center}(begin{equation}(begin{align})begin{align})begin{equation})begin{document}")

    (* Edge Cases *)
    val test9 = runTest("Deep side-by-side",
                       sbs2(sbs2(nested2("a", empty2),
                                nested2("b", empty2)),
                            sbs2(nested2("c", empty2),
                                 nested2("d", empty2))),
                       pList2_fromString "(a)a(b)b(c)c(d)d")

    val test10 = runTest("Complex mixed structure",
                        nested2("outer",
                              sbs2(nested2("inner1", 
                                         nested2("deep", empty2)),
                                   nested2("inner2",
                                         sbs2(nested2("p1", empty2),
                                              nested2("p2", empty2))))),
                        pList2_fromString "(outer(inner1(deep)deep)inner1(inner2(p1)p1(p2)p2)inner2)outer")

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 10\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
  end

val _ = test_flattenPTree2;
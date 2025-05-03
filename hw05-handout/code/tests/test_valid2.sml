Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function for running tests *)
fun runTest (name: string, input: string, expected: bool) =
    let
        val result = valid2(pList2_fromString input)
        val passed = result = expected
        val message = name ^ ":\n" ^
                     "  Input: " ^ input ^ "\n" ^
                     "  Expected: " ^ Bool.toString expected ^ "\n" ^
                     "  Got: " ^ Bool.toString result ^ "\n" ^
                     "  Result: " ^ (if passed then "PASSED" else "FAILED") ^ "\n\n"
    in
        print message;
        passed
    end

val test_valid2 = 
  let
    (* Basic Cases *)
    val test1 = runTest("Empty string", "", true)
    
    val test2 = runTest("Single matching pair", "(tag)tag", true)
    
    (* Multiple Different Tags *)
    val test3 = runTest("Nested different tags", "(outer(inner)inner)outer", true)
    
    val test4 = runTest("Adjacent different tags", "(a)a(b)b", true)

    (* Complex Nesting *)
    val test5 = runTest("Deep nesting same tag", 
                       "(div(div(div)div)div)div", true)
    
    val test6 = runTest("Mixed nested tags", 
                       "(html(head)head(body(p)p)body)html", true)

    (* Invalid Cases *)
    val test7 = runTest("Mismatched tags", "(a)b", false)
    
    val test8 = runTest("Cross nesting", "(a(b)a)b", false)
    
    val test9 = runTest("Wrong order", "(a(b)a)b", false)

    (* HTML-like Cases *)
    val test10 = runTest("HTML structure", 
                        "(html(head(title)title)head(body(h1)h1(p)p)body)html", true)
    
    val test11 = runTest("Invalid HTML nesting", 
                        "(html(p(b)p)b)html", false)

    (* LaTeX-like Cases *)
    val test12 = runTest("LaTeX environments", 
                        "(document(center(equation)equation)center)document", true)
    
    val test13 = runTest("Invalid LaTeX nesting", 
                        "(center(document)center)document", false)

    (* Complex Mixed Cases *)
    val test14 = runTest("Complex valid nesting",
                        "(outer(a(b)b(c)c)a(d(e)e)d)outer", true)
    
    val test15 = runTest("Complex invalid nesting",
                        "(outer(a(b)a(c)b)c)outer", false)

    (* LaTeX Complex Cases *)
    val test16 = runTest("Nested LaTeX environments",
                        "(document(abstract(itemize(item)item)itemize)abstract)document", true)
    
    val test17 = runTest("LaTeX table in figure",
                        "(document(figure(table(row)row)table)figure)document", true)
    
    val test18 = runTest("Invalid LaTeX nesting order",
                        "(document(table(figure)table)figure)document", false)

    (* Documentation Format Cases *)
    val test19 = runTest("XML-like nesting",
                        "(xml(tag(attr)attr(value)value)tag)xml", true)
    
    val test20 = runTest("Markdown-like structure",
                        "(doc(section(subsection)subsection(code)code)section)doc", true)

    (* Mixed Format Cases *)
    val test21 = runTest("LaTeX in HTML",
                        "(html(body(latex(equation)equation)latex)body)html", true)
    
    val test22 = runTest("Invalid cross-format nesting",
                        "(html(latex)html(document)latex)document", false)

    (* Complex Document Structures *)
    val test23 = runTest("Complex document with multiple environments",
                        "(document(chapter(section(theorem)theorem(proof)proof)section)chapter)document", true)
    
    val test24 = runTest("Multiple parallel environments",
                        "(doc(text)text(code)code(math)math)doc", true)
    
    val test25 = runTest("Invalid parallel environment closure",
                        "(doc(text)code(math)text)doc", false)

    (* Edge Cases *)
    val test26 = runTest("Deep nested same-name tags",
                        "(env(env(env(env(env)env)env)env)env)env", true)
    
    val test27 = runTest("Complex invalid cross-nesting",
                        "(a(b(c)a)c)b", false)
    
    val test28 = runTest("Mixed depth parallel structures",
                        "(doc(a(b)b)a(x(y(z)z)y)x)doc", true)

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 28\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9,
           test10, test11, test12, test13, test14, test15, test16, test17, 
           test18, test19, test20, test21, test22, test23, test24, test25,
           test26, test27, test28])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9,
           test10, test11, test12, test13, test14, test15, test16, test17, 
           test18, test19, test20, test21, test22, test23, test24, test25,
           test26, test27, test28]
  end

val _ = test_valid2;
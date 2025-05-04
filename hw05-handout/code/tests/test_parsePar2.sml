Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function for comparing pTree2s *)
fun equalPTree2s (empty2, empty2) = true
  | equalPTree2s (nested2(s1, t1), nested2(s2, t2)) = 
    s1 = s2 andalso equalPTree2s (t1, t2)
  | equalPTree2s (sbs2(t1a, t1b), sbs2(t2a, t2b)) = 
    equalPTree2s (t1a, t2a) andalso equalPTree2s (t1b, t2b)
  | equalPTree2s _ = false

(* Test runner with detailed output *)
fun runTest (testName: string, input: string, expected: pTree2) =
    let
        val result = parsePar2(pList2_fromString input)
        val passed = equalPTree2s (result, expected)
        val message = testName ^ ":\n" ^
                     "  Input: " ^ input ^ "\n" ^
                     "  Expected: " ^ pTree2_toString expected ^ "\n" ^
                     "  Got: " ^ pTree2_toString result ^ "\n" ^
                     "  Result: " ^ (if passed then "PASSED" else "FAILED") ^ "\n\n"
    in
        print message;
        passed
    end

val test_parsePar2 = 
  let
    (* Complex LaTeX Document Structure *)
    val test1 = runTest(
      "Complex LaTeX article",
      "(document(title)title(abstract(theorem)theorem(proof)proof)abstract" ^
      "(section(subsection(equation)equation(align)align)subsection" ^
      "(figure(caption)caption(table)table)figure)section)document",
      nested2("document",
             sbs2(nested2("title", empty2),
                  sbs2(nested2("abstract",
                              sbs2(nested2("theorem", empty2),
                                   nested2("proof", empty2))),
                       nested2("section",
                              sbs2(nested2("subsection",
                                          sbs2(nested2("equation", empty2),
                                               nested2("align", empty2))),
                                   nested2("figure",
                                          sbs2(nested2("caption", empty2),
                                               nested2("table", empty2)))))))))

    (* Complex HTML-like Structure *)
    val test2 = runTest(
      "Complex HTML document",
      "(html(head(meta)meta(title)title)head" ^
      "(body(div(header)header(nav)nav)div" ^
      "(main(article(section)section)article" ^
      "(aside(widget)widget)aside)main)body)html",
      nested2("html",
             sbs2(nested2("head",
                         sbs2(nested2("meta", empty2),
                              nested2("title", empty2))),
                  nested2("body",
                         sbs2(nested2("div",
                                     sbs2(nested2("header", empty2),
                                          nested2("nav", empty2))),
                              nested2("main",
                                     sbs2(nested2("article",
                                                 nested2("section", empty2)),
                                          nested2("aside",
                                                 nested2("widget", empty2)))))))))

    (* Mixed Format Document *)
    val test3 = runTest(
      "Mixed document formats",
      "(document(xml(data)data)xml" ^
      "(html(body(latex(math)math)latex)body)html" ^
      "(markdown(code)code)markdown)document",
      nested2("document",
             sbs2(nested2("xml",
                         nested2("data", empty2)),
                  sbs2(nested2("html",
                              nested2("body",
                                     nested2("latex",
                                            nested2("math", empty2)))),
                       nested2("markdown",
                              nested2("code", empty2))))))

    (* Deep Nested Academic Document *)
    val test4 = runTest(
      "Deep nested academic paper",
      "(paper(abstract(summary)summary)abstract" ^
      "(theorem(lemma(proof(step)step)proof)lemma" ^
      "(corollary(proof(case)case)proof)corollary)theorem)paper",
      nested2("paper",
             sbs2(nested2("abstract",
                         nested2("summary", empty2)),
                  nested2("theorem",
                         sbs2(nested2("lemma",
                                     nested2("proof",
                                            nested2("step", empty2))),
                              nested2("corollary",
                                     nested2("proof",
                                            nested2("case", empty2))))))))

    (* Complex Side-by-Side with Nesting *)
    val test5 = runTest(
      "Complex parallel structures",
      "(main(col1(box1)box1(box2)box2)col1" ^
      "(col2(box3)box3(box4)box4)col2" ^
      "(col3(box5)box5(box6)box6)col3)main",
      nested2("main",
             sbs2(nested2("col1",
                         sbs2(nested2("box1", empty2),
                              nested2("box2", empty2))),
                  sbs2(nested2("col2",
                              sbs2(nested2("box3", empty2),
                                   nested2("box4", empty2))),
                       nested2("col3",
                              sbs2(nested2("box5", empty2),
                                   nested2("box6", empty2)))))))

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 5\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5])) ^ "\n");
    List.all (fn x => x) [test1, test2, test3, test4, test5]
  end

val _ = test_parsePar2;
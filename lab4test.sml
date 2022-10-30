use ("lab4func.sml");

fun test(fun_name : string, result_true, result_actual) =
if result_true = result_actual
then (fun_name, "Passed")
else (fun_name, "Failed");

(*Test_Task_01*)
print "\nTest_Task_01:\n";
(*Test_Task_01*)
print "\nTest_Task_01:\n";
test("only_capitals", ["To", "Be"], only_capitals(["To", "be", "oR", "nOt", "to", "Be"]));
test("only_capitals", [], only_capitals(["to", "be", "or", "not", "to", "be"]));

(*Test_Task_02*)
print "\nTest_Task_02:\n";
test("longest_string1", "", longest_string1([]));
test("longest_string1", "not", longest_string1(["to", "be", "or", "not", "to", "be"]));
test("longest_string1", "abc", longest_string1(["abc", "to", "be", "or", "not", "to", "be"]));

(*Test_Task_03*)
print "\nTest_Task_03:\n";
test("longest_string2", "", longest_string2([]));
test("longest_string2", "not", longest_string2(["to", "be", "or", "not", "to", "be"]));
test("longest_string2", "not", longest_string2(["abc", "to", "be", "or", "not", "to", "be"]));

(*Test_Task_04*)
print "\nTest_Task_04:\n";
test("longest_string3", "", longest_string3([]));
test("longest_string3", "not", longest_string3(["to", "be", "or", "not", "to", "be"]));
test("longest_string3", "abc", longest_string3(["abc", "to", "be", "or", "not", "to", "be"]));

test("longest_string4", "", longest_string4([]));
test("longest_string4", "not", longest_string4(["to", "be", "or", "not", "to", "be"]));
test("longest_string4", "not", longest_string4(["abc", "to", "be", "or", "not", "to", "be"]));

(*Test_Task_05*)
print "\nTest_Task_05:\n";
test("longest_capitalized", "", longest_capitalized([]));
test("longest_capitalized", "Or", longest_capitalized(["to", "be", "Or", "not", "to", "be"]));
test("longest_capitalized", "To", longest_capitalized(["abc", "To", "be", "or", "not", "to", "Be"]));

(*Test_Task_06*)
print "\nTest_Task_06:\n";
test("rev_string", "nihkimyskaM", rev_string("Maksymikhin"));

(*Test_Task_07*)
print "\nTest_Task_07:\n";
test("first_answer", 4, first_answer (fn(x) => if x > 3 then SOME x else NONE)  [1,2,3,4,5]);
test("first_answer", "c", first_answer (fn(x) => if x = "c" then SOME x else NONE)  ["a","b","c","d"]);

(*Test_Task_08*)
print "\nTest_Task_08:\n";
test("all_answers", SOME [4, 5], all_answers (fn(x) => if x > 3 then SOME [x] else NONE)  [4,5]);
test("all_answers", NONE , all_answers (fn(x) => if x > 3 then SOME [x] else NONE)  [1,2,3,4,5]);
test("all_answers", SOME ["c","c","c"], all_answers (fn(x) => if x = "c" then SOME [x] else NONE)  ["c","c","c"]);
test("all_answers", NONE , all_answers (fn(x) => if x = "c" then SOME [x] else NONE)  ["a","c","b","c","d","c"]);

(*Test_Task_09_1*)
print "\nTest_Task_09_1:\n";
test("count_wildcards", 1 , count_wildcards Wildcard);
test("count_wildcards", 2, count_wildcards (TupleP ([Wildcard, Wildcard])));
test("count_wildcards", 3, count_wildcards (TupleP ([Variable("abc"), Wildcard, Wildcard, ConstP(9), Wildcard])));

(*Test_Task_09_2*)
print "\nTest_Task_09_2:\n";
test("count_wild_and_variable_lengths", 2, count_wild_and_variable_lengths (TupleP ([Wildcard, Wildcard])));
test("count_wild_and_variable_lengths", 3, count_wild_and_variable_lengths (TupleP ([Variable("abc"), ConstP(9)])));
test("count_wild_and_variable_lengths", 5, count_wild_and_variable_lengths (TupleP ([Variable("abc"), Wildcard, Wildcard, ConstP(9)])));

(*Test_Task_09_3*)
print "\nTest_Task_09_3:\n";
test("count_some_var", 2, count_some_var("abc", (TupleP ([Variable("abc"), Variable("abcd")]))));
test("count_some_var", 1, count_some_var("abc", Variable("abc")));
test("count_some_var", 2, count_some_var("abc", (TupleP ([Variable("abc"), Variable("abc"), Variable("ab"), Wildcard]))));
test("count_some_var", 0, count_some_var("abc", ConstructorP ("abc",(Wildcard))));

(*Test_Task_10*)
print "\nTest_Task_10:\n";
test("check_pat", true, check_pat(Variable("abc")));
test("check_pat", true, check_pat((TupleP ([Variable("abc"), Variable("abd"), Variable("abe")]))));
test("check_pat", true, check_pat((TupleP ([Variable("abc"), Variable("abd"), Variable("abe"), ConstructorP ("abf", (Wildcard))]))));

(*Test_Task_11*)
print "\nTest_Task_11:\n";
test("first_match", SOME [("abc", Const 3)], first_match (Constructor("abc", Const 3)) ([ConstructorP("abc", Variable("abc")), Wildcard]));
test("first_match", SOME [("abc", Const 5)], first_match (Const 5) ([Variable("abc"), Wildcard, UnitP]));
test("first_match", SOME [("abcd", Unit)], first_match (Unit) ([Variable("abcd"), Wildcard, Variable("abc")]));
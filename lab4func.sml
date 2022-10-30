(*Task_01*)
fun only_capitals(stringList: string list) =
List.filter (fn str => Char.isUpper(String.sub(str,0))) stringList;

(*Task_02*)
fun longest_string1(stringList: string list) = 
List.foldl (fn(str1, str2) =>    if (String.size(str1) > String.size(str2)) 
                                then str1 
                                else str2) 
            "" 
            stringList;

(*Task_03*)
fun longest_string2(stringList: string list) = 
List.foldl (fn(str1, str2) =>    if (String.size(str1) >= String.size(str2)) 
                                then str1 
                                else str2) 
            "" 
            stringList;

(*Task_04*)
fun longest_string_helper f = 
List.foldl (fn(str1,str2) =>    if f(String.size(str1),String.size(str2)) 
                                then str1 
                                else str2) 
            "";

val longest_string3 = longest_string_helper(fn (str1, str2) => str1 > str2);
val longest_string4 = longest_string_helper(fn (str1, str2) => str1 >= str2);

(*Task_05*)
val longest_capitalized = longest_string1 o only_capitals;

(*Task_06*)
val rev_string = String.implode o rev o String.explode;

(*Task_07*)
exception NoAnswer;
fun first_answer f list = 
case list of
    [] => raise NoAnswer
    |x::xs => case f(x) of
        NONE => first_answer f xs 
        |SOME result => result;

(*Task_08*)
fun all_answers f list=
    let fun find_all_answers(resultList, lst) = 
        case lst of 
            [] => SOME(resultList)
            |x::xs => (case f(x) of   
                        NONE => NONE
                        |SOME result => find_all_answers (resultList @ result, xs))
    in 
        find_all_answers([], list) 
    end; 

(*Task_09*)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(*Task_09_1*)
fun count_wildcards (p: pattern) = g (fn _ => 1) (fn _ => 0) p;

(*Task_09_2*)
fun count_wild_and_variable_lengths(p:pattern)=
    g (fn _ => 1) (String.size) p;

(*Task_09_3*)
fun count_some_var(str : string, p : pattern)=
    g (fn _ => 0) ( fn x => if String.isSubstring str x then 1 else 0) p;

(*Test_Task_10*)
fun check_pat(p: pattern)=
    let 
        fun getAllStrings(p: pattern) = 
            case p of
                Variable x => [x]
                |ConstructorP(_, str) => getAllStrings(str)
                |TupleP ps => List.foldl(fn(ps_elem, acc)=> getAllStrings(ps_elem) @ acc) [] ps 
                | _ => []

        fun checkDuplicates(allStr: string list) = 
            case allStr of
                [] => true
                |x::xs => if (List.exists(fn str => str = x) xs)
                        then false
                        else checkDuplicates(xs)
        in 
            checkDuplicates(getAllStrings(p))
        end;

(*Test_Task_11*)
fun first_match value patternList =
    let 
        fun match_function (value, pattern) =
            case (value, pattern) of  
                (_, Wildcard) => SOME []
                |(_, Variable s ) => SOME [(s, value)]
                |(Unit, UnitP) => SOME []
                |(Const v1, ConstP p1) =>if (v1 = p1) then SOME [] else NONE

                |(Tuple vs, TupleP ps) =>   if List.length vs = List.length ps 
                                            then case all_answers match_function(ListPair.zip(vs,ps))  of
                                                    SOME result => SOME result
                                                    |_ => NONE
                                            else NONE
                |(Constructor (s2 ,v), ConstructorP (s1, p) ) =>    if s2 = s1 
                                                                    then match_function(v,p) 
                                                                    else NONE
                |(_,_) => NONE
    in
        SOME (first_answer (fn pattern => match_function (value, pattern)) patternList)
        handle NoAnswer => NONE
    end;
(*
			 CS 51 Problem Set 1
		Core Functional Programming -- Testing
			     Spring 2017
 *)			     

open Ps1 ;;


(* sample tests *)
let test_reversed () =
  assert ((reversed [5; 4; 3; 2; 1; 0; -1]) = true) ;
  assert ((reversed []) = true) ;
  assert ((reversed [1]) = true) ;
  assert ((reversed [3; 3; 3; 2; 1]) = true) ;
  assert ((reversed [1; 2; 3]) = false) ;
  assert ((reversed [3; 2; 1; 1; 2; 3]) = false) ;
  assert ((reversed [3; 0; -3]) = true) ;
  assert ((reversed [-2; -3; 0; -1]) = false)

let test_merge () = 
  assert ((merge [1; 2; 3] [4; 5; 6; 7]) = [1; 2; 3; 4; 5; 6; 7]) ;
  assert ((merge [4; 5; 6; 7] [1; 2; 3]) = [1; 2; 3; 4; 5; 6; 7]) ;
  assert ((merge [4; 5; 6; 7] [1; 2; 3]) = [1; 2; 3; 4; 5; 6; 7]) ;
  assert ((merge [2; 2; 2; 2] [1; 2; 3]) = [1; 2; 2; 2; 2; 2; 3]) ;
  assert ((merge [1; 2] [1; 2]) = [1; 1; 2; 2]) ;
  assert ((merge [-1; 2; 3; 100] [-1; 5; 1001]) = [-1; -1; 2; 3; 5; 100; 1001]) ;
  assert ((merge [] []) = []) ;
  assert ((merge [1] []) = [1]) ;
  assert ((merge [] [-1]) = [-1]) ;
  assert ((merge [1] [-1]) = [-1; 1]) 

let test_unzip () =
  assert (unzip [(6,2);(2,4);(5,6)] = ([6;2;5],[2;4;6])) ;
  assert (unzip [(6,-2);(-2,4);(5,-6)] = ([6;-2;5],[-2;4;-6])) ;
  assert (unzip [] = ([], [])) ;
  assert (unzip [(1,2)] = ([1],[2]))

let test_variance () =
  assert (variance [1.0; 2.0; 3.0; 4.0; 5.0] = Some 2.5) ;
  assert (variance [1.0] = None) ;
  assert (variance [0.] = None) ;
  assert (variance [1.0; 1.0; 1.0; 1.0] = Some 0.)

let test_few_divisors () =
  assert( few_divisors 17 3 = true) ;
  assert( few_divisors 4 3 = false) ;
  assert( few_divisors 4 4 = true)

let test_concat_list () = 
  assert( concat_list ", " ["Greg"; "Anna"; "David"] = "Greg, Anna, David") ;
  assert( concat_list "..." ["Moo"; "Baaa"; "Quack"] = "Moo...Baaa...Quack") ;
  assert( concat_list ", " [] = "") ;
  assert( concat_list ", " ["Moo"] = "Moo")

let test_to_run_length () =
  assert( to_run_length [] = []) ;
  assert( to_run_length ['a'] = [(1,'a')]) ;
  assert( to_run_length ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] =
	  [(5,'a');(3,'b');(1,'c');(4,'d')])

let test_from_run_length () =
  assert(from_run_length [(3,'a');(3,'b');(1,'c');(4,'d')] = 
      ['a';'a';'a'; 'b';'b';'b';'c';'d';'d';'d';'d']) ;
  assert(from_run_length [(1,'a')] = ['a']) ;
  assert(from_run_length [(0,'a')] = []) ;
  assert(from_run_length [] = [])

;;

test_reversed () ;;
test_merge () ;;
test_unzip () ;;
test_few_divisors () ;;
test_concat_list () ;;
test_to_run_length () ;;
test_from_run_length () ;;

print_endline "All tests passed.";;


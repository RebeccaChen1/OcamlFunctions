(* 
			 CS 51 Problem Set 1
		     Core Functional Programming
			     Spring 2017
*)

(*======================================================================
Problem 1: Dealing with types

........................................................................
For each of the expressions below, enter a string describing the type
of the expression in the expressions below, replacing the ??? with the
appropriate type. The first one is done for you. Be sure to remove the
comments from each subproblem and to type check it before submission.
......................................................................*)


let prob0 : int = 42 ;;

let prob1a : string = let greet y = "Hello " ^ y in greet "World!";;

let prob1b : int option list = [Some 4; Some 2; None; Some 3];;

let prob1c : ('a option * float option) * bool = ((None, Some 42.0), true);;


let prob0 = "int";;

let prob1a = "string";;

let prob1b = "int option list";;

let prob1c = "('a option * float option) * bool";;

(*......................................................................
There are several values defined below that do not type check. 

Explain in a comment above each corresponding value why the following
definitions will not type check, and then provide a fixed version of 
each function as an OCaml value (outside of a comment). Your fix should
change the code minimally. 
......................................................................*)
(* 
The type is actually a (string * int) list because it is a list
of string * int rather than a tuple with one side being a string 
and the other an int list
*)

let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;

(* 
add itself is a function that accepts type int. 3.9 is a 
float so the function does not typecheck
*)  

let prob1e : int =
  let add (x, y) = x + y in
  if add (4, 4) = 10 then 4 else 2;;


(*
The list is a (string * int option) list. Also, some of the items in the
list are missing a "some" before the int
*)

let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
  ("May", None); ("June", Some 1); ("July", None); ("August", None);
  ("September", Some 3); ("October", Some 1); ("November", Some 2); 
  ("December", Some 3)] ;;

(*======================================================================
Problem 2 - Writing functions

........................................................................
For each subproblem, you must implement a given function, providing
appropriate unit tests in the accompanying file pset1_tests.ml. You
are provided a high level description as well as a type signature of
the function you must implement. Keep in mind the CS51 style guide and
what you've learned so far about efficiency and elegance. You are
*not* allowed to use library functions (i.e., the List module) for
*this* problem unless you implement the functionality yourself.
......................................................................*)

(*......................................................................
Problem 2a: The function "reversed" takes a list of integers and
returns true if the list is in decreasing order. The empty list is
considered to be reversed n this sense. Consecutive elements of the
same value can be considered equal in a reversed list.

Here is its signature: 
reversed : int list -> bool

Replace the line below with your own definition of "reversed".
......................................................................*)

(*takes list of integers and returns true is list is in decreasing order*)
let rec reversed (l : int list) : bool = 
  match l with
  | [] -> true
  | [_] -> true
  | a::b::t ->
      (* if all elements are decreasing, return true, else false *)
      if a >= b then reversed (b::t)
      else false ;;

(*......................................................................
Problem 2b: The function "merge" takes two integer lists, each
*sorted* in increasing order, and returns a single merged list in
sorted order.  For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;2;5] [2;4;6];;
- : int list = [1; 2; 2; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its signature:
merge : int list -> int list -> int list

Replace the line below with your own definition of "merge".
......................................................................*)

(*merges two already sorted integer lists*)
let rec merge (x : int list) (y: int list) : int list = 
  match x, y with
  | [], _ -> y
  | _ , [] -> x
  | hx::tx, hy::ty ->
      (* cons the greater head onto the list *)
      if hx < hy then hx :: merge tx (hy::ty)
      else hy :: merge (hx::tx) ty ;;



(*......................................................................
Problem 2c: The function "unzip", given a list of integer pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element.  The returned list should have elements in the order in which
they were provided. For example:

unzip [(6,2);(2,4);(5,6)];;
- : int list * int list = ([6;2;5],[2;4;6])

Here is its signature:
unzip : (int * int) list -> int list * int list)

Replace the line below with your own definition of "unzip".
......................................................................*)

(* given list of int pairs, returns pair of lists*)
let rec unzip (l : (int * int) list) : int list * int list =
  match l with
  | [] -> [], []
  (* recursively calls unzip and stores unzipped values in a,b*)
  | (x,y)::t -> let a, b = unzip t in x::a, y::b ;;


(*......................................................................
Problem 2d: The function "variance" takes a float list and returns
None if the list has fewer than two elements. Otherwise, it should
return Some of the variance of the floats. Recall that the variance of
a sequence of numbers is given by the following equation:
						
	1/(n-1) * sum (x_i - m)^2

where n indicates the number of elements in the list, m is the
arithmetic mean of the list, and x_i is element in the ith index of
the list. If you want to compare your output with an online
calculator, make sure you find one that calculates the (unbiased)
sample variance.  For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : float option = Some 2.5
variance [1.0];;
- : float option = None

Remember to use the floating point version of the arithmetic operators
when operating on floats (+., *., etc). The function "float" can
convert ("cast") an int to a float.

Here is the signature of "variance":
float list -> float option 							

Replace the line below with your own definition of "variance".
......................................................................*)

let variance (l : float list) : float option = 
  (* finds length of list *)
  let rec length (l : float list) : float = 
    match l with
    | [] -> 0.
    | _::t -> 1.0 +. length t in
    (* finds mean of list *)
  let rec mean (l : float list) : float =
    match l with
    | [] -> 0.
    | h::t -> h +. mean t in
    (* calculates sum of squared difference of x_i and the mean *)
  let rec calc_variance (x : float) (l : float list) : float =
    match l with
    | [] -> 0.
    | h::t -> (h -. x) ** 2. +. calc_variance x t in
    (* calculates variance *)
  match l with
  | [] -> None
  | [_] -> None
  | _::_ -> Some ((1. /. (length l -. 1.)) *. (calc_variance (mean l /. length l) l )) ;;

(*......................................................................
Problem 2e: The function "few divisors" takes two integers, x and y, and
returns true if x has fewer than y divisors (including 1 and x). Note:
this is NOT the same as x having fewer divisors than y. For example:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

Do not worry about negative integers at all. We will not test your code
using negative values for x and y, and do not consider negative integers
for divisors (i.e. -2 being a divisor for 4). 

Here is its signature:
few_divisors : int -> int -> bool 

Replace the line below with your own definition of "few_divisors".
......................................................................*)
(* finds number of divisors *)
let rec num_divisors (x : int) 
                     (i : int) 
                     (n : int) 
                    : int =
   match i with
    | 0 -> n
    | _ -> if (x mod i) = 0
           then num_divisors x (i - 1) (n + 1)
           else num_divisors x (i - 1) n 
(* returns  true if x has fewer than y divisors *)
let few_divisors (x : int) (y : int) : bool = 
  let divisors = num_divisors x (x/2) 1 in 
    if divisors < y then true else false ;;

(*......................................................................
Problem 2f: The function "concat_list" takes two arguments: sep, a
string, and lst, a string list. It returns one string with all the
elements of lst concatenated together but separated by the string
sep. For example:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

Here is its signature:
concat_list : string -> string list -> string

Replace the line below with your own definition of "concat_list"
......................................................................*)

(* separates elements in a list with sep *)
let rec concat_list (sep : string) (l : string list) : string =
  match l with
  | [] -> ""
  | [x] -> x
  | h::t -> h ^ sep ^ concat_list sep t ;;

(*......................................................................
Problem 2g: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'd'; 'd'; 'd'; 'd'] 

we can (sometimes) represent the same information more compactly as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'c'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times, followed by 3 occurrences of the character 'b',
followed by one 'c', and finally 4 copies of 'd'.

Write a function "to_run_length" that converts a list of characters
into the run-length encoding, and then write a function
"from_run_length" that converts back. Writing both functions will make
it easier to test that you've gottem them right.

Here are their prototypes/signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definition of "to_run_length"
and "from_run_length".
......................................................................*)
(* expands each char c, n times*)
let rec counter (n : int)
                (c : char) 
                (lst : char list)
              : (int * char) list =
   match lst with
   | [] -> [(n,c)]
   | h::t -> if h = c then counter (n + 1) c t
             else (n,c) :: counter 0 h lst ;;


let to_run_length (l : char list) : (int * char) list = 
    match l with
    | [] -> []
    | h::_t -> counter 0 h l ;;


(* cons char b onto list n times *)
let rec from_run_length (l : (int * char) list) : char list = 
    match l with 
    | [] -> []
    | (0,_b)::t -> from_run_length t
    | (n,b)::t -> b::(from_run_length ((n - 1,b)::t)) ;;


(*======================================================================
Problem 3: Challenge problem: Permutations

........................................................................
The function "permutations" takes a list of integers and should
return a list containing every permutation of the list. For example:

  permutations [1; 2; 3] =
  - : int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; 
  [3; 1; 2]; [3; 2; 1]]

It doesn't matter what order the permutations appear in the returned
list.  Note that if the input list is of length n, then the answer
should be of length n! (that is, the factorial of n).

Hint: One way to do this is to write an auxiliary function, interleave
: int -> int list -> int list list, that yields all interleavings of
its first argument into its second. For example:

  interleave 1 [2; 3] = 
  - : int list list = [ [1; 2; 3]; [2; 1; 3]; [2; 3; 1] ]

You may also use list module functions for this question and may find 
List.map and List.concat helpful. 

Here is the signature of permutations:

  permutations : int list -> int list list

Replace the line below with your own definition of "permutations".
(fun _ -> failwith "permutations not implemented")
......................................................................*)


let permutations = fun _ -> failwith "permutations not implemented" ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 660 ;;

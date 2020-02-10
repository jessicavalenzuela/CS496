
(* ******************************************** *)
(**I pledge my honor that I have abided by the Stevens Honors System -jvalenzu**)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
let rec apply_transition_function : tf -> state -> symbol -> state option = fun tf state sym -> 
(**applies the transition function f to
the symbol sym assuming that the current state is st**)
	match tf with
	| [] -> None
	| (firststate, input, secondstate) :: transitions ->
	if sym = input 
	then Some secondstate
	if firststate = state
	then Some state
	else apply_transition_function transitions state sym

let accept : fa -> input -> bool = fun fa input -> 
(**Accepts if in accept state**)
	let rec accepthelper currentstate = function
		| [] -> List.mem currentstate fa.final
		| first :: rest ->
		(match apply_transition_function fa.tf currentstate first with
			| Some state -> accepthelper state rest
			| None -> false)
	in accepthelper fa.start input




let deterministic : fa -> bool = fun fa -> 
(**This function checks whether the given automaton is non-deterministic or not**)
	let rec deterhelper tf track = 
	match tf with
	| [] -> true
	| (firststate, input, _) :: _ when List.mem (firststate,input) track -> false
    | (firststate, input, _) :: rest -> deterhelper rest ((firststate,input)::track)
  in deterhelper fa.tf []

  let valid : fa -> bool = fun fa ->
  (**checks for validity. A FA is said to be valid if
(a) The start state belongs to set of states;
(b) The final states belong to set of states; and
(c) It is deterministic.**)
  	List.mem fa.start fa.states
  	&& List.for_all (fun state -> List.mem state fa.states) fa.final
  	&& deterministic fa


  let reachable : fa -> state list = fun fa ->
  (**Reports list of states that are reachable from the start state.**)
  	let rec reachhelper = function
  	| state when fa.start = state -> true
  	| state ->
  		let p =
  			fa.tf
  			|> List.filter (fun (_,_,d) -> d = state) 
        	|> List.map (fun (s, _, _) -> s)
        in List.mem fa.start p || List.exists reachhelper p
     in List.filter reachhelper fa.states

  let remove_dead_states : fa -> fa = fun fa ->
  (**Removes all dead (i.e. unreachable) states from a valid FA**)
  let reach = reachable fa 
  in {
    states = reach;
    start = fa.start;
    tf = List.filter (fun (s,_,d) -> List.mem s reach && List.mem d reach) fa.tf;
    final = List.filter (fun state -> List.mem state reach) fa.final
  } 

	










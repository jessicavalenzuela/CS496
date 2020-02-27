(*1.  defining dTree *)
type dTree = 
	|Leaf of int 
	|Node of (char*dTree*dTree)

(* 2. define two expressions tLeft and tRight, of type dTree *)
let tLeft = Node('w',Node('x',Leaf(2),Leaf(5)),Leaf(8))
let tRight = Node('w',Node('x',Leaf(2),Leaf(5)),Node('y',Leaf(7),Leaf(5)))
let tMid = Node('w', Node('x',Node('y', Leaf(0), Leaf(0)), Node('y', Leaf(0), Leaf(0))), Node('y', Node('y', Leaf(0), Leaf(0)), Leaf(0)))
(* 3.a dTree_height: that given a dTree returns its height*)
let rec dTree_height t =
	match t with
	|Leaf(x) -> 0
	|Node(i,l,r) -> let lt_height = dTree_height l in
					let rt_height = dTree_height r in
					1 + max lt_height rt_height

(* 3.b dTree_size: that given a dTree returns its size*)
let rec dTree_size t =
	match t with
	|Leaf(x) -> 1
	|Node(i,l,r) -> 1 + dTree_size l + dTree_size r

(* 3.c dTree_paths: that given a dTree returns a list with all the paths to its leaves*)
let rec dTree_paths t =
	let rec mapAppend k list =
		match list with
		|[] -> []
		|x::xs -> [k::x]@mapAppend k xs in
	match t with
	|Leaf(x) -> [[]]
	|Node(i,l,r) -> let lt = mapAppend 0 (dTree_paths l) in
					let rt = mapAppend 1 (dTree_paths r) in
					lt@rt

(* 3.d dTree_is_perfect: that determines whter a dTree is perfect *)
let rec dTree_is_perfect t =
	match t with
	|Leaf(x) -> true
	|Node(i,l,r) -> if dTree_height l = dTree_height r
						then dTree_is_perfect l && dTree_is_perfect r
						else false

(* 3.e dTree_map: that given the follow arguments
									f: char-> char
									g: int-> int
									t: dTree*)
let rec dTree_map f g t =
	match t with
	|Leaf(x) -> Leaf(g x)
	|Node(i,l,r) -> Node(f i, dTree_map f g l, dTree_map f g r)

(* 4 Define list_to_tree, a function that given a list of charrcacters l,
 creates a tree in which the symbols of an inner node at level n corresponds to the n-th element in 1 *)
let rec list_to_tree lst =
	match lst with
	|[] -> Leaf(0)
	|h::t -> Node(h,list_to_tree t, list_to_tree t)

(* 5 replace_leaf_at, a function that given a tree t and a graph for a function f replaces
 all the leave in t by value indicated in the graph of the function *)
let rec replace_leaf_at t g =
	let rec helper t r newVal = 
	match r with
	|[] -> Leaf(newVal)
	|x::xs ->
		match t with
		|Node(i,l,r) ->
			if x = 0
			then Node(i, helper l xs newVal, r)
			else Node(i, l, helper r xs newVal)
	in match g with
	|[] -> t
	| (x,y)::ys -> replace_leaf_at(helper t x y) ys
	

(*  6Define a function bf_to_dTree that takes a pair-encoding of a boolean function and
returns its tree-encoding *)

 
let bf_to_dTree (x,y) =
	replace_leaf_at (list_to_tree x)
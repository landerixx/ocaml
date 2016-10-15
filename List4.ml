(* /2 - Napisac funkcje insert dla drzewa planarnego i find  (drzewo ma 3 dzieci) *)

(* /3 - Zdefiniowac drzewo planarne, przechowujace wartosci int tylko w lisciach. Napisac funkcje obliczajaca sume wartosci z lisci *)
(* /4 - Napisac funkcje obliczajaca sume dlugosci wszystkich sciezek od korzenia do liscia w drzewie planarnym *)

(*zad2 *)
type 'a planTree = Empty | Node of 'a planTree * 'a planTree * 'a planTree * int;;

let rec insert(tree,v) =
	match tree with
		Empty 			 -> Node(Empty,Empty,Empty,v)
		| Node(l,m,r,vl) -> if v<vl/2 then Node(insert(l,v),m,r,vl)
							else if v>2*vl then Node(l,m,insert(r,v),vl)
							else Node(l,insert(m,v),r,vl);;
							
let tree2 = Empty;;
let tree2=insert(tree2,3);;
let tree2=insert(tree2,4);;



let rec find(tree,v) = 
	match tree with
		Node(l,m,r,vl)-> if v=vl then true
						 else if v<vl/2 then find(l,v)
						 else if v>2*vl then find(r,v)
						 else find(m,v)
		|Empty        -> false;;

find(tree2,3);;
find(tree2,4);;


(*zad3*)

type 'a planTree = Empty | Node of 'a planTree list | Leaf of int;;

let t = Node([
Leaf(4);
Node([
	Node([Leaf(9)]);			Node([	Node([ (Leaf(5)) ])	])    
	]);	
Node([Leaf(6)])
]);;


(*zad 4*)


let rec suma = function
	Empty		 -> 0
	|Leaf(n)	 -> n
	|Node([])	 -> 0
	|Node(x::xs) -> suma x + suma(Node(xs));;
	
suma t;;
				
let rec sumSciez = function
	Empty->1
	|Leaf(n)->1
	|Node([])->1
	|Node(x::xs)-> 1 + sumSciez(Node(xs));;		
	
sumSciez t;;




(*zad 3 *)

type 'a planarne1 = Leaf of int | Node2 of 'a planarne1 list;;
	
let tPlan1 = Node2(
							Node2(Leaf 1::Leaf 2::Leaf 3::[])::Node2(Leaf 4::Leaf 5::Leaf 6::[])::Node2(Leaf 7::Leaf 8::Leaf 9::[])::[]
							);;

							
let rec sumTree tree =
	match tree with
	| Leaf v -> v
	| Node2(h::t) -> sumTree h + sumTree(Node2(t))
	| Node2([]) -> 0;;

sumTree tPlan1;;




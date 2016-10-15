(*
1. funkcja insert dla drzewa bst 
2. funkcja find dla drzewa bst 

3. zdefiniowac typ drzewa planarnego  
4. napisac funkcje przeksztalcajaca drzewo binarne w planarne 
5. napisac funkcje  obliczajaca max stopien wierzcholka w drzewie planarnym 
*)





type 'a tree = Empty | Node of 'a tree * 'a tree *int * string ;;


(*zad 1 *)

let rec insert (bst, key, value ) =
	match bst with  
			Empty 			-> Node (Empty,Empty, key, value)
	|  Node(left,right,k,v) -> 
	
				if key > k       then Node(left ,insert(right,key,value),  k,  v )
				else  if key < k then Node(insert(left,key, value ), right , k , v )
				else failwith "zdublowany klucz";;	
	
	
(*zad2 *)	
let rec find (key, bst) = 
	match bst with 
		
		Node(left,right,k,v) ->  
		
			if key = k 		then Some v 
			else if key < k then find(key, left)
			else   	find(key, right)
						
		|	Empty			 -> None
						
	;;					
						
						
	

	
(*zad3*)
type 'a treePlan = EmptyPlan | NodePlan of 'a treePlan list * int * string ;;
	
	

(*zad4*)	
	let rec przeksztalc (bst) = 
			match bst with	
			Empty 				   -> EmptyPlan
			| Node(left,right,k,v) ->  NodePlan([przeksztalc(left);przeksztalc(right)], k, v ) 
	
	;;
		

	
(*zad5*)
	


	
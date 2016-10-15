
(*1. utworzyc nieskonczona liste leniwa zaiweraaca elementy ciagu: an = 2an-1 + 3an-2 - 5an-3 
2. Napisac funkcje przekrzaltajaca skocnzona liste liczb (kluczy) w leniwe uporzadkowane drzewo binarne 


*)



type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

(*Zad1*)
let rec ltake = function
  (0, _) 			 -> []
  | (_, LNil) 		 -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;


let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
ltake (5,lfrom 30);;
 
	let rec ciag = 
		let rec lciagpom a1 a2 a3 =
			let an = 2*a1 + 3*a2 - 5* a3
				in  LCons(an , function() -> lciagpom an a1 a2 )
			in LCons(0, function() -> LCons(0, function() -> LCons(1, function() -> lciagpom 1 0 0  )));;
	

	ltake(7,ciag) ;;
	
	
	
	
	
	
(*Zad2*)	
type 'a lbtree = LEmpty | LNode of 'a * (unit -> 'a lbtree) * (unit -> 'a lbtree) ;;
	

	
let rec list2bst l =
	let rec insert2bst = function
			| (k , LNode (r,lt,rt)  ) ->
				if k<r then LNode(r,( fun () -> insert2bst( k, lt() ) ), rt ) else
				if k>r then LNode(r, lt,( fun() -> insert2bst( k, rt() )))
				else failwith "duplicated key"
			| (k, LEmpty) 			  -> LNode(k,(fun() ->LEmpty), (fun() ->LEmpty)) 
	in
	match l with
	   h::t -> insert2bst(h, list2bst t) 
	   | [] -> LEmpty;;

		
		
	
			
let rec lTree n =
	LNode(n, (fun () -> lTree(2 * n)), (fun () -> lTree(2 * n + 1)))
;;		
	
	(* b) *)
let breadth ltree =
	let rec breadthHelp = function
		| [] -> LNil
		| LEmpty :: t -> breadthHelp t
		| LNode(v, l, r) :: t ->
				LCons(v, fun () -> breadthHelp (t @ (l() :: r() :: [])))
	in breadthHelp [ltree]
	
	

let tree = lTree 3 ;;
let llista = breadth tree;;	
ltake(10,llista);;
	
	
let lista = [2;6;4;10;12;11;9];;		
let drzewo = list2bst lista;;
let lazylist = breadth drzewo;;
let listawyjsciowa=ltake(10,lazylist);;

print_newline;;
print_newline;;
print_newline;;

let lista2 = [2;4;6;10;11;12;9];;
let drzewo2 = list2bst lista2;;
let lazylist2 = breadth drzewo2;;
let listawyjsciowa2=ltake(10,lazylist2);;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 
  
	
	
	
	
	
	
	
	
	
	

let rec func  (n, k) =
	if n=0 then 1
	else n*func(n-k,k);;
	
	
let rec find (lista,argument) =
	if lista = [] then -1
	else if  argument= fst (List.hd lista) then  snd (List.hd lista)
	else find(List.tl lista, argument);;

	
	
		
let rec nwd(n,k) = 
    if  n=k then k 
	else if(n>k) then
		nwd(n-k,k)
	else
	    nwd(n,k-n);;
					
		
										
let rec nwdMod(n,k) =
   if n=0 then k
   else 
	nwdMod(k mod n, n);;
			
							
			
	
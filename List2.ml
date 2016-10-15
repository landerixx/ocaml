(*
zad1. funkkcja odwracajaca liste i czy lista jest palindromem

zad 2.funkcja obliczajaca wartosc potegi logarytmiczny. x^n 

zad 3.pierwiatkowanie: x0= ...




*)


 (*zad 1 *)
let rec reverse lista =
	if lista =[] then []
	else reverse (List.tl lista) @ (List.hd lista::[]);;


let palindrome lista =
	lista = reverse lista;;


(*zad 2 *)
let rec power (x ,n)= 
	if n=0 then 1.
	else ( if n mod 2 = 0 then let d = power( x ,n/2 )	in d*.d	
	else x *. power(x ,n-1));;


	
	
(*zad 3 *)
let rec helpPierw(a,przyb,n) =
	
	if  power(przyb,n)  -. a <0.000001 then  przyb
	
	else 
		helpPierw (a ,    1./.(float_of_int n)	*. ( przyb *. float_of_int(n-1) +.(a/.power(przyb,n-1)))  ,    n);;
	
	
let rec pierwiastek(a,n) =
	let przyb=a  in 	
		if  n=1 then  przyb
		else helpPierw(a,przyb,n);;
	













	


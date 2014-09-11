(*let x = 3 + 2 in x - 2s

utop # let v ((k:int list) (j:int list)):int list = k@j in v [2;3] [5];;
Error: Parse error: [fun_binding] expected after [ipatt] (in [let_binding])  *)
(*
let minappmax((a:int list),(b:int list)) =
	let x = List.length(a) in
	let y = List.length(b) in
	match (x<y) with
		true -> a@b
		| _ -> b@a
							in minappmax ([3;5], [4;6])

*)
(*Exercise 1*)

let rec is_mon_inc(il : int list) : bool =
	match (il) with
	 [] -> true
	| hd::[] -> true
	| hd::nx::tl -> if(hd<nx) then is_mon_inc(nx::tl) else false
	
		(* in is_mon_inc ([3;5;9]) *)


(*Exercise 2*)



(*

let rec is_unimodal(il2 : int list) : bool = 
	let fun incrcheck(il3 : int list) : bool = 
	
	(let w = is_mon_inc(il3) in 
		match(w) with
		| true -> false
		| _ -> is_unimodal(il3)) in
	match (il2) with
	| [] -> true
	| hd::[] -> true
	| hd::nx::tl -> if(hd>nx) then incrcheck(nx::tl) else is_unimodal(nx::tl)



*)

(*Exercise 4*)
(*
let rev_int (number: int) : int =

	let rec reverse ((num : int),(place : float),(lst: int list)) : int list =
		if (num < 1) then lst
		else let returned = (num mod (int_of_float(10.0** place)))/ int_of_float(10.0** (place-.1.0)) in reverse((num-returned),(place +. 1.0),(returned::lst))
		in

	let rec addlist ((flister : int list),(curfloat : int)) : int =
		match flister with
		 [] -> curfloat
		| hd::fl -> addlist(fl,(curfloat + hd))
		in
	
	let ffinal = addlist (reverse (abs number, 1.0, [] ), 0) in

	match (number < 0) with
	 true -> ((-1) * ffinal)
	| _ -> ffinal
	*)


(*Exercise 5*)

let unflatten k lst =

	(*bg is an int for the first value in the cut off list. ed is an int for
	the last value in the new substring for example if bg = 1 and ed = 3 for
	 string [0;1;2;3;4;5;] then [1;2;3] will be returned. cplace is an updated
	 from zero. *)
	let rec shortenls (bg:int) (ed:int) (cplace:int) origlst newlst  =
		match origlst with
		 [] -> newlst
		 | hd::tl -> if (cplace < bg) then shortenls bg ed (cplace + 1) tl newlst
							else if (cplace < (ed + 1)) then shortenls bg ed (cplace + 1) tl (newlst @ [hd])
							else newlst
		in

	let rec makelst (currentlst: 'a list) (finallst: 'a list list) :'a list list =

		if (List.length(currentlst) < k) then (finallst @ [currentlst])
		else makelst (shortenls k (List.length(currentlst) - 1) 0 currentlst [])  (finallst @ [(shortenls 0 (k-1) 0 currentlst [])])
	

	in
	match (k <= 0) with
	 true -> None
	| _ -> if (lst != []) then Some(List.tl(makelst lst [[]])) else Some([[]])





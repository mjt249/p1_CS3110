
(*Exercise 1*)
(*is_mon_inc determines whether an int list increases monotonically
*il is the int list to be tested. 
*Precondition: il must be an int list.
*Postcondition: will return a bool corresponding to whether the 
*input list is monotonic. If il is [], then true
*)
let rec is_mon_inc(il : int list) : bool =
    match (il) with
     [] -> true
    | hd::[] -> true
    | hd::nx::tl -> if(hd<nx) then is_mon_inc(nx::tl) else false
	
		

(*Exercise 2*)
(*is_unimodal determines whether an int list does not increase
*after it starts decreasing. il2 is the int list to be tested
*Precondition: il must be an int list
*Postcondition: will return a bool corresponding to whether
*the input list is unimodal as described. So il []  and [int] will
*return true
*)
let rec is_unimodal(il : int list) : bool = 
    match (il) with
     [] -> true
    | (hd::[])-> true
    | (hd::nx::[]) -> true
    | (hd::nx::th::tl) -> if(hd>=nx && nx<th) then false 
       else is_unimodal(nx::th::tl)


(*Exercise 3*)
(*powerset takes a set and returns a set of all the possible 
*permutations of the input set. set is the input set
*Precondition: set is a list of any type
*Postcondition: will return the powerset of set. Set []
*will return [[]].
*)
let powerset (set: 'a list) :'a list list =
    let rec adder (staticset: 'a list list) (currentlt: 'a list list) 
    (nextval: int) (newlt: 'a list list) =
        match currentlt with
         [] -> (staticset@newlt)
        | hd::tl -> if(newlt = [[]]) then adder staticset
         (List.tl(currentlt)) nextval [nextval::hd] 
        else 
        adder staticset (List.tl(currentlt)) nextval ((nextval::hd) :: newlt)
    in
    let rec setmaker (givenset: 'a list) (returnedset:'a list list) =
        match givenset with
         [] -> returnedset
        | hd::tl -> setmaker tl (adder returnedset returnedset hd [[]])
    in
    setmaker set [[]]


(*Exercise 4*)
(*rev_int takes an integer and returns an integer with the input int's
*digits reversed. number is the integer to be reversed
*Precondition: number is an int that will not go to or beyond
* the Max or Min int once reversed. Otherwise undefined
*Postcondition: will return an int with the reversed digits of the 
*input. 001 --> 1 and -30 --> 3
*)
let rev_int (number: int) : int =
    let rec reverse ((num : int),(place : float),(lst: int list)) : int list =
        if (num < 1) then lst
        else let returned = (num mod (int_of_float(10.0** place))) in
         reverse((num - returned) ,(place +. 1.0),(returned / 
         int_of_float(10.0** (place -. 1.0)))::lst)
        in
    let rec addlist ((flister : int list),(curfloat : int),
    	(cplace : float)) : int =
        match flister with
         [] -> curfloat
        | hd::fl -> 
        addlist(
         fl,(curfloat + (hd * int_of_float(10.0** cplace))), (cplace +. 1.0))
        in
    let ffinal = addlist (reverse (abs number, 1.0, [] ), 0, 0.0) in
    match (number < 0) with
     true -> ((-1) * ffinal)
    | _ -> ffinal
	


(*Exercise 5*)
(*unflatten takes a list and returns a list of the input list divided
*into smaller lists with length k. lst is the input list to be divided
*Precondition: k must be an int > 0 or function will return None.
*lst must be an 'a list
*Postcondition: will return an 'a list list option of the input list
*broken into smaller lists of length k. If the list length is not divisible 
*by k, then the last smaller list may have a length less than k. 
*)
let unflatten (k:int) (lst: 'a list) : 'a list list option =
	(*bg is an int for the first value in the cut off list. ed is an int for
	 *the last value in the new substring for example if bg = 1 and ed = 3 for
	 *string [0;1;2;3;4;5;] then [1;2;3] will be returned. cplace is an updated
	 *from zero. *)
    let rec shortenls (bg:int) (ed:int) (cplace:int) origlst newlst  =
        match origlst with
         [] -> newlst
        | hd::tl -> if (cplace < bg) 
        then shortenls bg ed (cplace + 1) tl newlst
            else if (cplace < (ed + 1)) 
            then shortenls bg ed (cplace + 1) tl (newlst @ [hd])
            else newlst
    in
    let rec makelst (currentlst: 'a list) (finallst: 'a list list) 
    :'a list list =
        if (List.length(currentlst) <= k) then (finallst @ [currentlst])
        else makelst (
        	shortenls k (List.length(currentlst) - 1) 0 currentlst []) 
        (finallst @ [(shortenls 0 (k-1) 0 currentlst [])])
    in
    match (k <= 0) with
     true -> None
    | _ -> if (lst != []) then Some(List.tl(makelst lst [[]])) else Some([[]])


(*Exercise 6*)
(*int_of_roman takes a *)
type numeral = I | V | X | L | C | D | M
type roman = numeral list
let rec int_of_roman ( r : roman ) : int =
    let int_of_numeral (n : numeral) = 
        match n with
        | I -> 1
        | V -> 5
        | X -> 10
        | L -> 50
        | C -> 100
        | D -> 500
        | M -> 1000 in 
    let hd = 
        match r with
         [] -> 0
        | h::ta -> int_of_numeral h in 
    let nx = 
        match r with 
         [] -> 0
        | h::[] -> 0
        | h::n::ta -> int_of_numeral n in
    let th =
        match r with
         [] -> 0
        | h::[] -> 0
        | h::n::[] -> 0
        | h::n::t::ta -> int_of_numeral t in
    let tl =
        match r with
        [] -> []
        | h::[] -> []
        | h::n::[] -> []
        | h::n::t::[] when nx < th -> []
        | h::n::t::[] when (hd>nx && nx=th)-> n::t::[]
        | h::n::t::[] when nx=th-> t::[]
        | h::n::t::[] when nx>th-> t::[]
        | h::n::t::ta when nx<th -> ta
        | h::n::t::ta when (hd>nx && nx=th && nx=0)-> []
        | h::n::t::ta when (hd>nx && nx=th && th=0)-> n::[]
        | h::n::t::ta when (hd>nx && nx=th)-> n::t::ta
        | h::n::t::ta when (nx=th && th=0)-> []
        | h::n::t::ta when nx=th-> t::ta
        | h::n::t::ta when (nx>th && th=0)-> [] 
        | h::n::t::ta when nx>th-> t::ta 
        | _ -> [] in
    match r with
     [] -> 0
    | h::[] -> hd
    | h::n::[] when hd<nx -> nx - hd
    | h::n::[] -> hd + nx
    | h::n::t::ta when ((hd>nx && nx>th) || (hd=nx && nx>th) ||
     (hd=nx && nx=th)) -> (hd+nx) + int_of_roman tl
    | h::n::t::ta when (hd>nx && nx=th) -> hd + int_of_roman tl
    | h::n::t::ta when (hd>nx && nx<th) -> (hd + (th - nx)) + int_of_roman tl
    | h::n::t::ta when (hd=nx && nx<th) -> (th - (hd + nx)) + int_of_roman tl
    | h::n::t::ta when (hd<nx) -> (nx - hd) + int_of_roman tl
    | _ -> int_of_roman []


(*Exercise 7*)

(*List.tl([[]]) was used in exercises 3 and 5 since it is not possible to
*through an error calling List.tl on a 'a list list since [] will be returned.
*Also, after careful consideration, it seemed to be the best way to go about
*removing the empty set inherit in conning a list with [[]]. 
*Kayla could not get git working so she emailed any code done on her
*Computer to Madeleine, who pushed it to the repository.
*Division of work: The first three problems were done together.
*While all exercises were designed and pseudocoded by both
*partners, due to time constraints, Kayla wrote the code for #2 and #6 and 
*Madeleine wrote the code for #3, #4 and #5. Partners worked together to 
*design and run tests for all exercises.
*Kayla wrote up the first three problems formally for scanning and
*Madeleine fixed style issues, prepared the git log to turn in 
*and wrote comments.

*)
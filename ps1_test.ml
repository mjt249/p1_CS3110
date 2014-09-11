open Ps1
open Assertions
open Printf

(*

TEST_UNIT "is_mon_inc_test1" =

let t1 = is_mon_inc([1;2;3;4;5]) in
assert_true (t1) 
let t2 = is_mon_inc([1;2]) in
assert_true (t2) 
let t3 = is_mon_inc([1]) in
assert_true (t3) 
let t4 = is_mon_inc([3;2;1]) in
assert_false (t4) 
let t5 = is_mon_inc([]) in
assert_true (t5) 
let t6 = is_mon_inc([1;2;1]) in
assert_false (t6) 
let t7 = is_mon_inc([2;1;2]) in
assert_false (t7) 




TEST_UNIT "rev_int_test1" =
let v1 = rev_int(4321) in
let b1 = 1 in
let () = printf "%d " v1 in

assert_false (v1 == b1)
*)
(*
TEST_UNIT "shortenls_test1" =
let s1 = shortenls 0 [0;1;2;3;4;5;6;7] [] in
let s2 = true in
let () = List.iter (printf "%d ") s1 in
assert_true (s2 == true)
*)
TEST_UNIT "unflatten_test1" =
let u1 = unflatten 3 [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = Some [[0; 1; 2]; [3; 4; 5]; [6; 7; 8]; [9; 10]]  in
assert_true (u1 = u2)
let u1 = unflatten 0 [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = None  in
assert_true (u1 = u2)
let u1 = unflatten (-310) [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = None  in
assert_true (u1 = u2)
let u1 = unflatten 6 [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = Some [[0; 1; 2; 3; 4; 5]; [6; 7; 8; 9; 10]] in
assert_true (u1 = u2)


(*
TEST_UNIT "is_unimodal_test1"=

let r1 = is_unimodal([1;2;3;2])in
assert_true(r1)
let r2 = is_unimodal([1;3;2;3])in
assert_false(r2)
let r3 = is_unimodal([1;2;3])in
assert_true(r3)
let r4 = is_unimodal([3;2;1])in
assert_true(r4)
let r5 = is_unimodal([1;1;1])in
assert_true(r5)
let r6 = is_unimodal([])in
assert_true(r6)
let r7 = is_unimodal([1])in
assert_true(r7)
let r8 = is_unimodal([2;1;2])in
assert_false(r8)
*)



let () = Pa_ounit_lib.Runtime.summarize() 


(*let () = List.iter (printf "%d ") a*)
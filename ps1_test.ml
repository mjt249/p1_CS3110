open Ps1
open Assertions
open Printf


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



TEST_UNIT "is_unimodal_test2"=

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
let r9 = is_unimodal([1;2;3;3]) in
assert_true(r9)
let r10 = is_unimodal([1;2;3;2;2]) in 
assert_true(r10)
let r11 = is_unimodal([3;2;2]) in
assert_true(r11)
let r12 = is_unimodal([1;2]) in
assert_true(r12)
let r13 = is_unimodal([1;1]) in
assert_true(r13)
let r14 = is_unimodal([2;1]) in
assert_true(r14)


TEST_UNIT "powerset_test3" =
let p1 = powerset [1;2] in
let p2 = [[]; [1]; [2; 1]; [2]] in
assert_true (p1 = p2)
let p1 = powerset [] in
let p2 = [[]] in
assert_true (p1 = p2)
let p1 = powerset [1] in
let p2 = [[]; [1]] in
assert_true (p1 = p2)


TEST_UNIT "rev_int_test4" =
let v1 = rev_int(4321) in
let v2 = 1234 in 
assert_true (v1 = v2)
let v1 = rev_int(-4321) in
let v2 = (-1234) in 
assert_true (v1 = v2)
let v1 = rev_int(10) in
let v2 = 1 in 
assert_true (v1 = v2)
let v1 = rev_int(4) in
let v2 = 4 in 
assert_true (v1 = v2)
let v1 = rev_int(1111111) in
let v2 = 1111111 in 
assert_true (v1 = v2)
let v1 = rev_int(-100) in
let v2 = (-1) in 
assert_true (v1 = v2)


TEST_UNIT "unflatten_test5" =
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
let u1 = unflatten 300 [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = Some [[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]] in
assert_true (u1 = u2)
let u1 = unflatten 11 [0;1;2;3;4;5;6;7;8;9;10] in 
let u2 = Some [[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]] in
assert_true (u1 = u2)
let u1 = unflatten 1 [0;1] in 
let u2 = Some [[0];[1]] in
assert_true (u1 = u2)
let u1 = unflatten 2 [0;1] in 
let u2 = Some [[0; 1]] in
assert_true (u1 = u2)

(*
TEST_UNIT "shortenls_test5" =
let s1 = shortenls 0 [0;1;2;3;4;5;6;7] [] in
let s2 = true in
let () = List.iter (printf "%d ") s1 in
assert_true (s2 == true)
*)

TEST_UNIT "int_of_roman_test6" =
let n1 = int_of_roman [] in
assert_true(n1=0)
let n2 = int_of_roman [I; I; I] in
assert_true(n2=3)
let n3 = int_of_roman [X; L; I; I] in
assert_true(n3=42)
let n4 = int_of_roman [M; C; M; X; C; I; X] in
assert_true(n4=1999)
let n5 = int_of_roman [I] in
assert_true(n5=1)
let n6 = int_of_roman [I; I] in
assert_true(n6=2)
let n7 = int_of_roman [I; V] in
assert_true(n7=4)
let n8 = int_of_roman [V; I] in
assert_true(n8=6)
let n9 = int_of_roman [X; C; V] in
assert_true(n9=95)
let n10 = int_of_roman [V; I; I; I] in
assert_true(n10=8)
let n11 = int_of_roman [L; M; X; I; V] in
assert_true(n11=964)
let n12 = int_of_roman [X; X; V] in
assert_true(n12=25) 
let n13 = int_of_roman [X; I; V] in
assert_true(n13=14)
let n14 = int_of_roman [X; V; I] in
assert_true(n14=16)
let n15 = int_of_roman [L; X; V; I] in
assert_true(n15=66)
let n16 = int_of_roman [C; I; C] in 
assert_true(n16=199)


let () = Pa_ounit_lib.Runtime.summarize() 


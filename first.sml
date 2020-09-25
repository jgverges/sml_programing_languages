(* This is a comment *)
val x = 34; 
(* static enviropnment x : int *)
(* dynamic environment x --> 34 *)

val y = 17;
(* static enviropnment x : int , y : int *)
(* dynamic environment x --> 34, y --> 17 *)
val z = (x+y) + (y+2)
(* static enviropnment x : int , y : int *, z : int *)
(* dynamic environment x --> 34, y --> 17 , z --> 70 *)
val q = z +1
(* static enviropnment x : int , y : int *, z : int *, q : int *)
(* dynamic environment x --> 34, y --> 17 , z --> 70 , q --> 71 *)
val abs_of_z = if z < 0 then 0 - z else z; (* bool *) (* int *)
(* abs_of_z : int *)
(* dynaic environment : ..., abs_of_z --> 70 *)
val abs_of_z_simpler = abs z;
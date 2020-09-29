(* Problem 1 - Alternate subtraction, addition *)
fun alternate (xs : int list) =
  (* alternate [1,2,3,4] = 1-2+3-4 = -2 *)
  if null xs then 0
  else if null (tl xs) then hd xs
  else hd xs - hd (tl xs) + alternate(tl (tl xs))

(* Problem 2 - Minimum and maximum of a list *)
fun maximum (xs : int list) =
  if null xs then NONE
  else if null (tl xs) then SOME (hd xs)
  else 
    let val max = maximum (tl xs)
       in if hd xs > valOf max
	       then SOME (hd xs)
	       else max
       end

fun minimum (xs : int list) =
  if null xs then NONE
  else if null (tl xs) then SOME (hd xs)
  else let val min = minimum (tl xs)
       in if hd xs < valOf min
	        then SOME (hd xs)
	        else min
       end

(* Challenge : Implement this without maximum and minimum *)	   
fun min_max (xs : int list) =
  (* Takes a non-empty list of numbers and returns (min, max) *)
  (valOf (minimum xs), valOf (maximum xs))

(* Problem 3 - Cumulative sums of a list *)
fun cumsum (xs : int list) =
  if null xs then []
  else if null (tl xs) then [hd xs]
  else (hd xs)::cumsum(hd xs + hd (tl xs)::(tl (tl xs)))

(* Problem 4 - Greetings! *)		      
fun greeting (s : string option) =
  if isSome s
  then "Hey there, " ^ valOf s ^ "!"
  else "Hey there, you!"

(* Problem 5 - Repeat elements in a list a given number of times *)	   
fun repeat (xs : int list, rs : int list) =
  (* repeat ([1,2,3],[4,0,3]) = [1,1,1,1,3,3,3] *)
  if null xs orelse null rs then []
  else if hd rs = 0
  then repeat (tl xs, tl rs)
  else hd xs :: repeat (xs, (hd rs - 1)::(tl rs))

(* Problem 6 - Add two int options *)		       
fun addOpt (x : int option, y : int option) =
  if isSome x andalso isSome y
  then SOME ((valOf x) + (valOf y))
  else NONE

(* Problem 7 - Add a list of int options *)	
(* ERROR: don't return NONE *)   
fun addAllOpt (xs : (int option) list) =
  if null xs 
  then SOME 0
  else if (hd xs) = NONE
    then addAllOpt (tl xs)
    else SOME (valOf (hd xs) + valOf (addAllOpt (tl xs)))

(* Problem 8 - OR on a list *)	    
fun any (bs : bool list) =
  if null bs
  then false
  else hd bs = true orelse any (tl bs) = true

(* Problem 9 - AND on a list *)					     
fun all (bs : bool list) =
  if null bs
  then true
  else hd bs = true andalso all (tl bs) = true

(* Problem 10 - Zip two lists *)					      
fun zip (xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys)::zip(tl xs, tl ys)

(* Problem 11 - Zip lists with different lengths *)
(* This zips one very large 'repeated' list with a smaller list *)		    
fun zipRecycle (xs : int list, ys : int list) =
  let
      fun cycle (ls : int list, n : int) =
	      if n = 0
	      then []
	      else ls @ cycle (ls, n-1)
      val lx = length xs
      val ly = length ys
  in
      if lx < ly
      then zip(cycle(xs, ly), ys)
      else zip(xs, cycle(ys, lx))
  end
      
(* Problem 12 - Zip lists only if equal lengths *)
fun zipOpt (xs : int list, ys : int list) =
  if length xs = length ys
  then SOME (zip (xs, ys))
  else NONE

(* Problem 13 - Find string in list *)	   
fun lookup (ss : (string*int) list, s2 : string) =
  if null ss then NONE
  else if #1 (hd ss) = s2
  then SOME (#2 (hd ss))
  else lookup (tl ss, s2)
	      
(* Problem 14 - Split a list into two different lists (>= 0) *)
fun splitup (xs : int list) =
  if null xs
  then ([], [])
  else
      let
	      val (ps, ns) = splitup (tl xs)
      in
	      if hd xs >= 0
	      then (hd xs::ps, ns)
	      else (ps, hd xs::ns)
      end
	  
(* Problem 15 - Split a list into two different lists with a threshold *)
fun splitAt (xs : int list, thres : int) =
  if null xs
  then ([], [])
  else
      let val (ps, ns) = splitAt (tl xs, thres)
      in
	      if hd xs < thres
	      then (hd xs::ps, ns)
	      else (ps, hd xs::ns)
      end

(* Problem 16 - Check if list is sorted in ascending order *)
fun isSorted (xs : int list) =
  if null (tl xs) then true
  else (hd xs <= hd (tl xs)) andalso isSorted (tl xs)

(* Problem 17 - Check if list is sorted in ascending or descending order *)
fun isAnySorted (xs : int list) =
  if null (tl xs) then true
  else isSorted(xs) orelse (hd xs > hd (tl xs)) andalso isAnySorted (tl xs)

(* Problem 18 - Merge two sorted lists *)
fun sortedMerge (xs : int list, ys : int list) =
  if null xs
  then ys
  else if null ys
  then xs
  else if hd xs < hd ys
  then hd xs :: sortedMerge (tl xs, ys)
  else hd ys :: sortedMerge (xs, tl ys)
			    

(* Problem 19 - Quicksort *)			    
fun qsort (xs : int list) =
  if null xs then []
  else if null (tl xs) then [hd xs]
  else
      let
	      val smaller = qsort(#1(splitAt(tl xs, hd xs)))
	      val bigger = qsort(#2(splitAt(tl xs, hd xs)))
      in
	      smaller @ [hd xs] @ bigger
      end
  
(* Problem 20 - Divide a list into two, alternatively *)
fun divide (xs : int list) =
  if null xs then ([], [])
  else if null (tl xs) then (hd xs::[], [])
  else
      let val (ys, zs) = divide (tl (tl xs))
      in
	      (hd xs::ys, hd(tl xs)::zs)
      end	  


(* Problem 21 - MergeSort-ish? *)
fun not_so_quick_sort (xs : int list) =
  if null xs then []
  else if null (tl xs) then [hd xs]
  else
      let
	      val ys = #1 (divide xs)
	      val zs = #2 (divide xs)
      in
	      sortedMerge(not_so_quick_sort(ys), not_so_quick_sort(zs))
      end


(* Problem 22 - Full Divide *)	
fun fullDivide (k : int, n : int) =
  (k div n, k mod n)

(* test_______________________________________ *)

(* Quirky Addition -- Continued *)
val test_add_all_opt_1 = addAllOpt [SOME 1, NONE, SOME 3] = SOME 4
val test_add_all_opt_2 = addAllOpt [] = NONE
val test_add_all_opt_3 = addAllOpt [NONE, NONE, NONE] = NONE
val test_add_all_opt_4 = addAllOpt [SOME 123] = SOME 123
val test_add_all_opt_5 = addAllOpt [NONE, SOME ~1, NONE, NONE] = SOME ~1


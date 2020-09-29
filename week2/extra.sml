(* 
https://www.coursera.org/learn/programming-languages/supplement/U9go7/extra-practice-problems 
*)


(* 1. alternate : int list -> int *)
fun alternate(numbers_list:int list)=
    if null numbers_list
    then 0
    else hd numbers_list + (alternate(tl numbers_list) * ~1)


(* 2. function min_max : int list -> int * int that takes a non-empty list of numbers, 
and returns a pair (min, max)|(min, max)of the minimum and maximum of the numbers in the list.x *)
fun min_max(lst: int list)=
    let
    fun search(min:int, max:int, lst)=
        if null lst
        then (min,max)
        else 
            search (if min < hd lst then min else hd lst, 
                if max > hd lst then max else hd lst,
                tl lst)
    in
        search(hd lst,hd lst,lst) (* better -> search(hd lst,hd lst,tl lst) *)
    end


(* 3. fun cumsum : int list -> int list that takes a list of numbers and 
returns a list of the partial sums of those numbers. For example cumsum [1,4,20] = [1,5,25] *)
fun cumsum(lst: int list)=
    let
    fun helper(pos:int, lst: int list)=
        if null lst
        then []
        else pos+hd lst::helper(pos + hd lst , tl lst)
    in
        helper(0, lst)
    end


(* 4:  fun greeting : string option -> string  that given a string option SOME name 
returns the string "Hello there, ...!" where the dots would be replaced by name. 
Note that the name is given as an option, so if it is NONE then replace the dots with "you". *)
fun greeting(name: string option)=
            if isSome name
            then ( "Hello there, "^ valOf name)
            else ( "Hello there, you") 


(* 5    repeat : int list * int list -> int list that given a list of integers and another list of
nonnegative integers, repeats the integers in the first list according to the numbers indicated by 
the second list. For example: repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]|repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]. *)  
 fun repeat (f_lst: int list, s_lst: int list) =
    if null f_lst
    then []
    else if hd s_lst >0
        then hd f_lst :: repeat(f_lst, hd s_lst-1 :: tl s_lst)
        else repeat (tl f_lst, tl s_lst)


(*6. function addOpt : int option * int option -> int option that given two "optional" integers, 
adds them if they are both present (returning SOME of their sum), or returns NONE if at least one of the two arguments is NONE.
 addOpt(SOME 1, SOME 40); ->  val it = SOME 41 : int option; addOpt(SOME 1, NONE);  -> val it = NONE : int option    *)
fun addOpt(f_opt:int option, s_opt: int option)=
    if isSome f_opt andalso isSome s_opt
    then SOME (valOf f_opt + valOf s_opt)
    else NONE


(* 7. Write a function addAllOpt : int option list -> int option that given 
a list of "optional" integers, adds those integers that are there (i.e. adds all the SOME i).
For example: addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4.If the list does not contain any SOME is in it, 
i.e. they are all NONE or the list is empty, the function should return NONE. *)
fun addAllOpt(opt_lst: int option list)=
    let
    fun add(acc:int option , opt_lst: int option list)=
        if null opt_lst
        then  acc
        else  if isSome acc
              then if isSome (hd opt_lst)
                   then add (SOME (valOf (hd opt_lst)+ valOf acc), tl opt_lst)
                   else add (acc, tl opt_lst)
              else add (hd opt_lst, tl opt_lst)
    in
        add( NONE ,opt_lst)
    end


(* 8. Write a function any : bool list -> bool that given a list of booleans returns 
true if there is at least one of them that is true, otherwise returns false. (If the list is empty it should return \verb|false|false because there is no \verb|true|true.) *)
fun any (bool_list: bool list)=
    if null bool_list
    then false
    else if hd bool_list
        then true
        else any(tl bool_list)


(* 9. Write a function all : bool list -> bool that given a list of booleans returns 
true if all of them true, otherwise returns false. (If the list is empty it should return true because there is no \verb|false|false.) *)
fun all (bool_list: bool list)=
    if null bool_list
    then true
    else if hd bool_list
        then all(tl bool_list)
        else false


(* 10. Write a function zip : int list * int list -> int *int list that given two lists of 
integers creates consecutive pairs, and stops when one of the lists is empty. For example: 
zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]. *)
fun zip (list1: int list, list2: int list)=
    if null list1
    then []
    else if null list2
        then []
        else [(hd list1, hd list2)] @ zip (tl list1, tl list2)

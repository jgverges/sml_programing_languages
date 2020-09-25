val x =5

fun pow (x:int, y: int) =
    if y=1
    then x
    else x * pow(x, y-1)

fun countdown (x)=
    if x=0
    then []
    else x::countdown(x-1)

fun swap(pr: int*bool*int, num :int)=
    (#2pr, #3 pr, #1 pr, num)

fun sort_pair(pr: int*int)=
    if #1 pr > #2 pr
    then (#2 pr, #1 pr) 
    else pr

fun sum_two_pairs(pr1:int*int, pr2:int*int)=
    (#1 pr1+ #1 pr2, #2 pr1 + #2 pr2)

fun div_mod(x:int, y: int)=
    (x div y, x mod y)

fun sum_list(xs:int list)= 
    if null xs
    then 0
    else hd xs +sum_list(tl xs)

fun append( xs: int list, ys: int list)=
    if null xs
    then ys
    else hd xs :: append(tl xs, ys)

(* fun append_one_element_to_list(xs: (int*int) list, ys: (int*int) list)=
    hd xs::hd ys
 *)
fun append_date(count:(int*int*int)list, dateList:(int*int*int)list)=
    hd dateList::(1,2,3)::hd count::[]
(*                 if null dateList
                then count
                else search_month(if #2(hd dateList)= month then count:: hd dateList::[] else count, 
                                    tl dateList)
 *)
fun sum_pairs_list(pr: (int*int) list)=
    if null pr
    then 0
    else #1 (hd pr) + #2 (hd pr) +sum_pairs_list(tl pr)

fun firsts(xs: (int*int)list)=
    if null xs
    then []
    else #1 (hd xs) :: firsts(tl xs)

fun sum_firsts(xs: (int*int)list)=
    if null xs
    then 0
    else #1 (hd xs) + sum_firsts(tl xs)

fun seconds(xs: (int*int)list)=
    if null xs
    then []
    else #2 (hd xs)::seconds(tl xs)

fun sum_pairs_list2( xs : (int*int)list)=
    ( (sum_list(firsts(xs))), (sum_list(seconds(xs))) )    

fun min_of_list(numbers:int list)=
    let
      fun search(min:int, numbers:int list)=
        if null numbers
        then min
        else
            search(if hd numbers < min then hd numbers else min,tl numbers)

    in
      search(hd numbers, tl numbers)
    end

fun search_value(numbers:int list, value:int)=
    let
      fun search(count:int, numbers:int list)=
        if null numbers
        then count
        else search(if hd numbers = value then count+1 else count, tl numbers)
    in
      search(0, numbers)
    end

(* just try printing out a string *)
fun sum1(xs : int list) =

        if  null xs
        then 0
        else (print ("Hi \n") ;hd xs + sum1(tl xs));

(* sum1([1, 2, 3, 4,5, 6]); *)

fun insert_list(xs:int list, y:int)=
    if null xs
    then [y]
    else hd xs::insert_list(tl xs, y)

    
fun is_older_v2 (firstDate :int*int*int, secondDate : int*int*int) =

    if #1 firstDate < #1 secondDate
    then true
    else if  #1 firstDate > #1 secondDate   
        then false
        else if #2 firstDate < #2 secondDate  
            then true
            else false

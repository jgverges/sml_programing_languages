(* 9 *)

fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)


 fun what_month(day_of_year: int)=
    if day_of_year > 0 andalso day_of_year < 366
    then 1+number_before_reaching_sum(day_of_year,[31,28,31,30,31,30,31,31,30,31,30,31])
    else 0   
    
fun what_month2 (day_of_year : int) =
    let 
	     val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	     1 + number_before_reaching_sum(day_of_year, month_lengths)
    end


fun month_range (day1:int, day2: int)=
    if day1 > day2
    then []
    else let
        fun moths_betwen_days(result: int, day:int)=
            if day = day2
            then what_month(day2)::[]
            else what_month(day):: moths_betwen_days( what_month(day1+1), day+1)
        in
        moths_betwen_days(what_month(day1),day1)
        end

fun month_range (day1:int, day2: int)=
    if day1 > day2
    then []
    else what_month(day1):: month_range( day1+1, day2)
        
(* alternate : int list -> int *)
fun alternate(numbers_list:int list)=
    if null numbers_list
    then 0
    else hd numbers_list + (alternate(tl numbers_list) * ~1)

fun count(lst: int list)=
    if null lst
    then 0
    else 1 + count(tl lst )
       



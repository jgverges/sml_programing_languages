
fun is_older (firstDate :int*int*int, secondDate : int*int*int) =
     #1 firstDate < #1 secondDate orelse 
    (#1 firstDate = #1 secondDate andalso #2 firstDate < #2 secondDate) orelse 
    (#1 firstDate = #1 secondDate andalso #2 firstDate = #2 secondDate) andalso #3 firstDate < #3 secondDate

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month) 


fun number_in_months(dates : (int * int * int) list, monthsList : int list) =
    if null monthsList
    then 0
    else number_in_month(dates, hd monthsList) + number_in_months(dates, tl monthsList)


fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) =month
        then hd dates::dates_in_month( tl dates, month)
        else dates_in_month( tl dates, month)

fun dates_in_months(dateList : (int*int*int)list , monthsList : int list)=
    if null monthsList
    then []
    else dates_in_month(dateList, hd monthsList) :: dates_in_months(dateList, tl monthsList)
    
fun get_nth (strings:string list, element:int)= 
    if  element = 0 orelse null strings
    then ""
    else if element = 1
        then hd strings
        else get_nth(tl strings, element-1)

fun date_to_string (date:int*int*int)=
    let
      val monthsList = ["January", "February", "Marc", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in
      get_nth(monthsList, #2 date)^" "^Int.toString(#3 date) ^", " ^Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)

fun what_month (day_of_year : int) =
    let 
	     val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	     1 + number_before_reaching_sum(day_of_year, month_lengths)
    end

fun month_range (day1:int, day2: int)=
    if day1 > day2
    then []
    else what_month(day1):: month_range( day1+1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let 
             val ans = oldest(tl dates)
	 in 
       if isSome ans andalso is_older(valOf ans, hd dates)
	     then ans
	     else SOME (hd dates)
	 end 

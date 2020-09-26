(* 1 *)
fun is_older (firstDate :int*int*int, secondDate : int*int*int) =
     #1 firstDate < #1 secondDate orelse 
    (#1 firstDate = #1 secondDate andalso #2 firstDate < #2 secondDate) orelse 
    (#1 firstDate = #1 secondDate andalso #2 firstDate = #2 secondDate) andalso #3 firstDate < #3 secondDate


(* 2 *)
fun number_in_month( dateList : (int*int*int)list , month : int)=
    let
    fun search_month(count:int, dateList:(int*int*int)list)=
        if null dateList
        then count
        else search_month(if #2(hd dateList)= month then count+1 else count, tl dateList)
    in
        search_month(0, dateList)
    end
    (* count is not necessary, we can do counters just by adding 1+ recusive function *)
fun number_in_month_model (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month_model(tl dates, month)
    else number_in_month_model(tl dates, month) 


(* 3*)
fun number_in_months( dateList : (int*int*int)list, months : int list)= 
    let
    fun search_by_month(count:int, months)=
        if null months
        then count
        else search_by_month((count + number_in_month(dateList, hd months)), tl months)
    in
      search_by_month(0, months)
    end
    (* model *)
fun number_in_months(dates : (int * int * int) list, monthsList : int list) =
    if null monthsList
    then 0
    else number_in_month(dates, hd monthsList) + number_in_months(dates, tl monthsList)




(* 4 *)
fun dates_in_month (dateList : (int*int*int)list , month : int)=
    let
    fun append_dates(dates:(int*int*int)list, date : (int*int*int)list)=   
        if null dates     
        then date
        else hd dates::append_dates(tl dates, date)

    fun search_month(count:(int*int*int)list, dateList:(int*int*int)list)=
        if null dateList
        then tl count
        else search_month(if #2(hd dateList)= month then append_dates(count,[(hd dateList)])else count, tl dateList)
    in
        search_month( [(1,1,1)] , dateList)  
    end

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) =month
        then hd dates::dates_in_month( tl dates, month)
        else dates_in_month( tl dates, month)




(* 5  *)
fun dates_in_months(dateList : (int*int*int)list , months : int list)=
    let
    fun append_dates(dates:(int*int*int)list, date : (int*int*int)list)=   
        if null dates     
        then date
        else hd dates::append_dates(tl dates, date)

    fun search_by_month(count:(int*int*int)list, months)=
        if null months
        then tl count
        else search_by_month(append_dates(count, dates_in_month(dateList, hd months)),tl months)
    in
      search_by_month([(1,1,1)], months)
    end
 
fun dates_in_months(dateList : (int*int*int)list , monthsList : int list)=
    if null monthsList
    then []
    else dates_in_month(dateList, hd monthsList) :: dates_in_months(dateList, tl monthsList)



 (* 6 *)  
fun get_nth (strings:string list, element:int)= 
    let
      fun search_string(strings:string list, count:int)=
        if null strings
        then ""
        else 
            if count = element
            then hd strings
            else search_string(tl strings, count+1)
    in
        search_string(strings,1)
    end

fun get_nth (strings:string list, element:int)= 
    if  element = 0 orelse null strings
    then ""
    else if element = 1
        then hd strings
        else get_nth(tl strings, element-1)


(* 7 *)
fun date_to_string (date:int*int*int)=
    get_nth(["January ", "February ", "March ", "April ","May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "],
            #2 date) ^Int.toString(#3 date) ^", " ^Int.toString(#1 date)

fun date_to_string (date:int*int*int)=
    let
      val monthsList = ["January", "February", "Marc", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in
      get_nth(monthsList, #2 date)^" "^Int.toString(#3 date) ^", " ^Int.toString(#1 date)
    end


(* 8 *)
fun number_before_reaching_sum (total:int, numbers:int list)=
    let
      fun sum(partial:int, pos:int, numbers:int list)=
        if null numbers
        then pos
        else if partial + hd numbers <total
            then sum(partial +hd numbers, pos+1, tl numbers)
            else pos
    in
      sum (0,0, numbers)
    end


fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)


(* 9 *)
 fun what_month(day_of_year: int)=
    if day_of_year > 0 andalso day_of_year < 366
    then number_before_reaching_sum(day_of_year,[0,31,28,31,30,31,30,31,31,30,31,30,31])
    else 0   

fun what_month (day_of_year : int) =
    let 
	     val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	     1 + number_before_reaching_sum(day_of_year, month_lengths)
    end


(* 10 *)
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

(* 11 *)
fun oldest(dateList: (int*int*int)list )=
    if null dateList
    then NONE
    else
        let 
            fun old_betwen_dates(dateList: (int*int*int)list)=
                if null (tl dateList)
                then hd dateList
                else let
                        val tl_ans = old_betwen_dates(tl dateList)
                    in
                        if is_older(hd dateList, tl_ans)
                        then hd dateList
                        else tl_ans
                    end
        in 
           SOME (old_betwen_dates (dateList))
        end

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

(* arguably better alternate solution avoiding isSome / valOf *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let 
             fun f dates =
		             if null (tl dates)
		             then hd dates
		             else let 
                          val ans = f (tl dates)
	              	     in 
                          if is_older(ans, hd dates)
		                      then ans
		                      else hd dates
		                   end
	       in 
             SOME(f dates) 
         end

fun sum(xs : int list)=
    if null xs
    then 0
    else hd(xs) + sum(tl(xs))

fun countdown(x : int ) =
    if x = 0
    then []
    else x::countdown(x-1)

fun append (xs: int list, ys :int list)=
    if null xs
    then ys
    else hd xs ::append((tl xs),ys)

fun first (xs: (int *int) list)=
    if null xs
    then []
    else #1(hd(xs))::first(tl (xs))

fun seconds (xs: (int *int) list)=
    if null xs
    then []
    else #2(hd(xs))::seconds(tl (xs))

fun sum_pair_List(xs:(int*int) list) =
    (sum(first xs ))+ (sum(seconds xs))



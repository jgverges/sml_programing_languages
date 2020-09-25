val x = 7
fun pow(x : int, y : int) = 
    if y = 0
    then 1
    else x * pow(x,y-1)

fun cube(x : int)=
    pow(x,3)
    
fun sixtyfour = cube(4)

fun fortytwo = pow(2,2+2) +pow(4,2) + cube(2) + 2
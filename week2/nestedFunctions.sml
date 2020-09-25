
fun countup_from1(x:int)=
let
  fun count(from :int)=
    if from=x
    then x::[]
    else from::count(from+1)
in
  count(1)
end
    

fun count_list(from:int, to: int) = (* old way *) 
  if from = to
  then to::[]
  else from :: count(from + 1, to)


fun countup_from2(x)=
  let
    fun count(from) =
      if from = x
      then x::[]
      else from::count(from+1)
  in
    count(1)
  end

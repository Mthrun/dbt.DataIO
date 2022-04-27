TheSameKey <- function(keyA, keyB) {
# KeyOK= TheSameKey(KeyA, KeyB)
# KeyOK= TheSameKey(Key, ClsKey ); KeyOK
# Checks, if the Keys are the same
# INPUT
# KeyA[1:n]      vector of integer
# KeyB[1:n]      vector of integer
#
# OUTPUT
# KeyOK                   bool, tru if the same, else false
#
# $Author: MT 04/2016
  
tryCatch({
  if(length(keyA)!=length(keyB)) theSameKey=FALSE
  else theSameKey <- suppressWarnings(length(keyA) == sum(keyA == keyB))
},er=function(ex){
  theSameKey=FALSE
})
return(theSameKey)
}
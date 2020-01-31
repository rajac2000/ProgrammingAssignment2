

### This function vect_same is to check if the two matrices are identical
### It takes each matrix and convert them into vectors and a for loop is 
### used to check if each of the elements match, 
### Logical values returned as the result.

vect_same <- function(a,b) {
  vectsame = F
  vecta <- as.vector(a)
  vectb <- as.vector(b)
  #print(vecta)
  #print(vectb)
  for(i in seq_along(vecta)){
    #print(paste("iteration :", i))
    if(vecta[i] == vectb[i]){
      vectsame = T
    } 
    else {
      vectsame = F
      break()
    }
  }
  vectsame
}


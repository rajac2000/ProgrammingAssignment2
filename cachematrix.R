## R source code for Programming Assignment 2 - coursera - JH - Data science.

## By Rajasekar Chandrasekaran - rajac2000@yahoo.com

## 1. makeCacheMatrix - This function creates a special "matrix" object that 
##                      can cache its inverse. 


## 2. cacheSolve      - This function computes the inverse of the special 
##                      "matrix" returned by makeCacheMatrix above. 
##                       If the inverse has already been calculated 
##                      (and the matrix has not changed), then the cachesolve 
##                      should retrieve the inverse from the cache.

## The makeCacheMatrix fumction will be called by cacheSolve function.
## This stores the matrix passed into environment variable "cache_solved"
## This varibale will be returned by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  #print(class(x))
  cache_solved <<- x
}

## The cacheSolve fumction can be called on terminal using a matrix as arg.
## This calls the makeCacheMatrix to store a given matrix in cache.
## This also calls vect_same to determine if the matrix passed is actually the
## the same matrix used to find the inverse which is in cache.
##
## Please see bottom for sample execution.

cacheSolve <- function(x, ...) 
{
  if(exists("cache_origin") & exists("cache_solved"))
  { 
    #print("inside if 1")
    if(!vect_same(cache_origin,x) | !exists("cache_solved"))
    {
      #print("inside if 2")
      cache_origin <<- x
      makeCacheMatrix(solve(x))
      print("New input Solve called")
    }
  }
  else
  {
    #print("inside else 1")
    cache_origin <<- x
    makeCacheMatrix(solve(x))
    print("New input Solve called")
  }
  cache_solved
}
## Return a matrix that is the inverse of 'x'


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


### Sample Execution of the cacheSolve function.
# >
### 
### Create mat1, random 4 x 4 matrix.
### 
# > mat1 <- matrix(round(rnorm(16,50,5)),4,4)
# > mat1
# [,1] [,2] [,3] [,4]
# [1,]   58   44   49   49
# [2,]   52   41   47   50
# [3,]   56   52   54   46
# [4,]   53   53   45   45
### 
### Create mat2, another random 4 x 4 matrix.
### 
# > mat2 <- matrix(round(rnorm(16,50,5)),4,4)
# > mat2
# [,1] [,2] [,3] [,4]
# [1,]   53   54   55   46
# [2,]   51   48   58   48
# [3,]   53   54   46   53
# [4,]   46   52   57   55
### 
### Call cacheSolve for mat1. note the printed message 
### "New input Solve called" for a new matrix after the call
### This means the inverse was generated using Solve during this call.
### 
# > cacheSolve(mat1)
# [1] "New input Solve called"
# [,1]         [,2]        [,3]         [,4]
# [1,]  0.20671660 -0.151522593 -0.05682097  0.001350688
# [2,] -0.06857809 -0.003192534 -0.00119720  0.079444990
# [3,] -0.06575393  0.033644401  0.13761665 -0.106458743
# [4,] -0.09694253  0.148575639 -0.06928414  0.033521611
### 
### Call cacheSolve for mat1 once more. note the printed message 
### "New input Solve called" is not printed now.
### This means the inverse was returned from cache.
### 
# > cacheSolve(mat1)
# [,1]         [,2]        [,3]         [,4]
# [1,]  0.20671660 -0.151522593 -0.05682097  0.001350688
# [2,] -0.06857809 -0.003192534 -0.00119720  0.079444990
# [3,] -0.06575393  0.033644401  0.13761665 -0.106458743
# [4,] -0.09694253  0.148575639 -0.06928414  0.033521611
### 
### Call cacheSolve for mat2. note the printed message AGAIN 
### "New input Solve called" for a new matrix after the call
### This means the inverse was generated during this call 
### because the input matrix is different.
### 
# > cacheSolve(mat2)
# [1] "New input Solve called"
# [,1]        [,2]        [,3]        [,4]
# [1,] -0.05009849  0.11043992  0.06099803 -0.11326330
# [2,]  0.15211753 -0.17445831 -0.03645765  0.06016087
# [3,]  0.01762968  0.03125410 -0.06864741  0.02413001
# [4,] -0.12019041  0.04018385  0.05459619  0.03102429
### 
### Call cacheSolve for mat2 once more. note the printed message 
### "New input Solve called" is not printed now.
### This means the inverse was returned from cache.
### 
# > cacheSolve(mat2)
# [,1]        [,2]        [,3]        [,4]
# [1,] -0.05009849  0.11043992  0.06099803 -0.11326330
# [2,]  0.15211753 -0.17445831 -0.03645765  0.06016087
# [3,]  0.01762968  0.03125410 -0.06864741  0.02413001
# [4,] -0.12019041  0.04018385  0.05459619  0.03102429
### 
### Call cacheSolve for mat1 once more. note the printed message 
### "New input Solve called" is printed now.
### meaning -  the inverse was genearated using solve.
### 
# > cacheSolve(mat1)
# [1] "New input Solve called"
# [,1]         [,2]        [,3]         [,4]
# [1,]  0.20671660 -0.151522593 -0.05682097  0.001350688
# [2,] -0.06857809 -0.003192534 -0.00119720  0.079444990
# [3,] -0.06575393  0.033644401  0.13761665 -0.106458743
# [4,] -0.09694253  0.148575639 -0.06928414  0.033521611
# >
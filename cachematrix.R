## R source code for Programming Assignment 2 - coursera - JH - Data science.

## By Rajasekar Chandrasekaran - rajac2000@yahoo.com

## 1. makeCacheMatrix - This function creates a special "matrix" object that 
##                      can cache its inverse. 


## 2. cacheSolve      - This function computes the inverse of the special 
##                      "matrix" returned by makeCacheMatrix above. 
##                       If the inverse has already been calculated 
##                      (and the matrix has not changed), then the cachesolve 
##                      should retrieve the inverse from the cache.

## The makeCacheMatrix fumction should be called by cacheSolve function.
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

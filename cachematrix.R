## R source code for Programming Assignment 2 - coursera - JH - Data science.

## By Rajasekar Chandrasekaran - rajac2000@yahoo.com

## 1. makeCacheMatrix - This function creates a special "matrix" object that 
##                      can cache its inverse. 


## 2. cacheSolve      - This function computes the inverse of the special 
##                      "matrix" returned by makeCacheMatrix above. 
##                       If the inverse has already been calculated 
##                      (and the matrix has not changed), then the cachesolve 
##                      should retrieve the inverse from the cache.

## Creates the object for fucntions set, get for the original matrix and
## fucntions setInv and getInv for the Inverse matrix.

makeCacheMatrix <- function(ObjParm = matrix()) {
  if(!exists("cached_Matrix")){   
    cached_Matrix <<- ObjParm
    cached_Inverse <<- NULL
  }
  else if(!identical(cached_Matrix,ObjParm)){
    cached_Matrix <<- ObjParm
    cached_Inverse <<- NULL
  }
  
  getobjectMatrix <- function() ObjParm
  
  getCachedMatrix <- function() cached_Matrix
  
  setCachedMatrix <- function(newMatrix){
    cached_Matrix <<- newMatrix
  }
   
  getInverse <- function() cached_Inverse
  
  setInverse <- function(inversed_Matrix){
    cached_Inverse <<- inversed_Matrix
    
  }
  list(getobjectMatrix=getobjectMatrix, 
       getCachedMatrix=getCachedMatrix,
       setCachedMatrix=setCachedMatrix, 
       getInverse=getInverse, 
       setInverse=setInverse)
}


## Finds the inverse of the "matrix" for the object created by 
## makeCacheMatrix above. If the inverse has already been calculated it 
## should retrieve the inverse from the cache.
##
## Please see bottom for sample execution.

cacheSolve <- function(x, ...){
  
  if(!identical(x$getobjectMatrix(),x$getCachedMatrix())){
    x$setCachedMatrix(x$getobjectMatrix())
    x$setInverse(solve(x$getobjectMatrix()))
    print("cacheSolve : Inverse not in cache - solve() called ")
    
  }
  if(is.null(x$getInverse())){ 
    x$setInverse(solve(x$getobjectMatrix()))
    print("cacheSolve : Inverse not in cache - solve() called ")
  }
  print(" Solved Matrix")
  x$getInverse()
}
## Return a matrix that is the inverse of 'x'


## Execution sample for resting the above code.


# 
# > matrix1 <- matrix(round(rnorm(4,50,5)),2,2)
# 
# > matrix1
# [,1] [,2]
# [1,]   54   51
# [2,]   42   61
# 
# > solve(matrix1)
# [,1]        [,2]
# [1,]  0.05295139 -0.04427083
# [2,] -0.03645833  0.04687500
# 
# > matrix2 <- matrix(round(rnorm(4,20,5)),2,2)
# 
# > matrix2
# [,1] [,2]
# [1,]   17   14
# [2,]   26   17
# 
# > solve(matrix2)
# [,1]       [,2]
# [1,] -0.2266667  0.1866667
# [2,]  0.3466667 -0.2266667
# 
# > cacheMatrix1 <- makeCacheMatrix(matrix1)
# 
# > cacheSolve(cacheMatrix1)
# [1] "cacheSolve : Inverse not in cache - solve() called "
# [1] " Solved Matrix"
# [,1]        [,2]
# [1,]  0.05295139 -0.04427083
# [2,] -0.03645833  0.04687500
# 
# > cacheSolve(cacheMatrix1)
# [1] " Solved Matrix"
# [,1]        [,2]
# [1,]  0.05295139 -0.04427083
# [2,] -0.03645833  0.04687500
# 
# > cacheMatrix2 <- makeCacheMatrix(matrix2)
# 
# > cacheSolve(cacheMatrix2)
# [1] "cacheSolve : Inverse not in cache - solve() called "
# [1] " Solved Matrix"
# [,1]       [,2]
# [1,] -0.2266667  0.1866667
# [2,]  0.3466667 -0.2266667
# 
# > cacheSolve(cacheMatrix2)
# [1] " Solved Matrix"
# [,1]       [,2]
# [1,] -0.2266667  0.1866667
# [2,]  0.3466667 -0.2266667
# 
# > cacheSolve(cacheMatrix1)
# [1] "cacheSolve : Inverse not in cache - solve() called "
# [1] " Solved Matrix"
# [,1]        [,2]
# [1,]  0.05295139 -0.04427083
# [2,] -0.03645833  0.04687500
# 
# > cacheSolve(cacheMatrix1)
# [1] " Solved Matrix"
# [,1]        [,2]
# [1,]  0.05295139 -0.04427083
# [2,] -0.03645833  0.04687500
# 
# > cacheSolve(cacheMatrix2)
# [1] "cacheSolve : Inverse not in cache - solve() called "
# [1] " Solved Matrix"
# [,1]       [,2]
# [1,] -0.2266667  0.1866667
# [2,]  0.3466667 -0.2266667
# > 
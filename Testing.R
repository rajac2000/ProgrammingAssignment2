matrix1 <- matrix(round(rnorm(4,50,5)),2,2)
matrix1
solve(matrix1)

matrix2 <- matrix(round(rnorm(4,20,5)),2,2)
matrix2
solve(matrix2)

cacheMatrix1 <- makeCacheMatrix(matrix1)
cacheSolve(cacheMatrix1)
cacheSolve(cacheMatrix1)


cacheMatrix2 <- makeCacheMatrix(matrix2)
cacheSolve(cacheMatrix2)
cacheSolve(cacheMatrix2)

cacheSolve(cacheMatrix1)
cacheSolve(cacheMatrix1)
cacheSolve(cacheMatrix2)


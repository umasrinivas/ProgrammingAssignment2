## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) inverseMatrix = inverse
    getInverse <- function() inverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)){
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    
    MatrixData <- x$getMatrix()
    inverseMatrix <- solve(MatrixData, ...)
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
        ## Return a matrix that is the inverse of 'x'
}

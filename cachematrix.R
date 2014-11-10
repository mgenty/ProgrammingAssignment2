##
## ==========================================================================
## This File Is Comprised Of Two Functions That Cache The Inverse Of A 
## Given Matrix. The First Function Creates A Special "Matrix" From The 
## Passed In Matrix That Is Capable Of Caching Its Own Inverse. The Second 
## Function Computes The Inverse Of The Special "Matrix" Created By The 
## First Function. In Other Words, The Second Function Exercises The First 
## Function.
##
## NOTE: The makeCacheMatrix Function Is A Derivative Of The makeVector
##       Function, And The cacheSolve Function Is A Derivative Of The
##       cachemean Function, Both Of Which Were Supplied As Example
##       Functions For This Programming Assignment.
##
## **************************************************************************
## Coursera:      Johns Hopkins Data Science Specialization
## R-Programming: Programming Assignment 2
## Last Update:   10Nov14 By Marc Genty
## **************************************************************************
##
## ==========================================================================
##

##
## --------------------------------------------------------------------------
## Description: Function To Create A Special "Matrix" Object
##              That Can Cache Its Inverse.
##
## Example Use: squareMatrix <- matrix(1:4, 2)
##              cacheMatrix  <- makeCacheMatrix(squareMatrix)
## --------------------------------------------------------------------------
##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
## --------------------------------------------------------------------------
## Description: Function To Compute The Inverse Of The Special
##              "Matrix" Returned By makeCacheMatrix (above).
##
## Example Use: squareMatrix   <- matrix(1:4, 2)
##              cacheMatrix    <- makeCacheMatrix(squareMatrix)
##              inverseMatrix  <- makeCacheMatrix(cacheMatrix)
## --------------------------------------------------------------------------
##

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
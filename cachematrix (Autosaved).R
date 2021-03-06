## These functions takes a matrix as an argument, and calculates the
## inverse of that matrix, then finally returns the value of the
##inverse matrix and stores this in the cache. The way these functions
## are constructed you can reset the input matrix by running the x$set
## function return with the makeCacheMatrix function.

## The first function (makeCache) takes a matrix, and returns a list of 4 functions 
## that 1. Sets the matrix, 2) gets the matrix, 3) Calculates the
## invers matrix and 4) gets the invers matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function uses the setinv function to calculate the invers
## matrix that we set with in the makeCacheMatrix function and stores
## it in the Cahce. However it first checks to see if there is an 
## invers matrix stored, and if there is it returns that matrix and 
## stops

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

## Matrix inversion is often computationally intensive.
## this script will cache the inverse of a matrix rather than computing it repeatedly.

## this function creates a special "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function compute the inverse of matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated, then this function will retrieve from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

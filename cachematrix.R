## The following two functions can be used to
## efficiently repetitively compute the inverse of a matrix. This is done by
## cashing the inverse
##
## makeCacheMatrix() creates a special matrix object which is able to cash
## its inverse
##
## cacheSolve() computes the inverse or just fetches it from the cash if
## it already was computed

## gets the matrix x
## when called it initializes the inverse with NULL and returns a list of 
## functions, with which you can set and get the matrix as well as set and get
## the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) s <<- inverse
    getinverse <- function() s
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## gets the matrix x and optional arguments for solving
## looks for the cached inverse of the matrix (inside x) and if not found
## computes it and caches it in x

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}

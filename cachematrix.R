## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_ <- NULL
    set <- function(y) {
        x <<- y
        inv_ <<- NULL
    }
    get <- function() x
    setinv <- function(inv_arg) inv_ <<- inv_arg
    getinv <- function() inv_
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_ <- x$getinv()
    if(!is.null(inv_)) {
        message("getting cached data")
        return(inv_)
    }
    data <- x$get()
    # inv_ <- matlib::inv(data, ...)
    inv_ <- solve(data, ...)
    x$setinv(inv_)
    inv_
}

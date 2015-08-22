## Put comments here that give an overall description of what your
## functions do

## Creating a function list and a object to cache the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) m <<- inv
    get_inv <- function() m
    list(set = set, 
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)    
}


## If the inverse has been cached, get it from the cache object. Otherwise, calculate the inverse and store it into the cache
cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
## Functions that are used to create and compute the inverse of a special "matrix" object 
## by caching the inverse of a matrix
## The matrix supplied is assumed to be invertible

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the special "matrix". If the inverse has already 
## been calculated it 'gets' it from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv                
}

## Solution of homework assignment 2
## These functions store a matrix and lazily caches its inverse
## @NotThreadSafe 

## Store the matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Control logic for accessing the matrix&inverse 
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}

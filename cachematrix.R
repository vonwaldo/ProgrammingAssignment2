## Make the "matrix" object, capable of cache-ing the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        matrix <- list(get = get,
             setinv = setinv,
             getinv = getinv)
}

## Check if the matrix has its inverse in the cache, if not then calculate the inverse and store it in the cache

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse... ...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## These functions are designed to avoid repeated time-consuming calculations
## of matrix inverse by caching already computed results and getting them from cache when needed.

## makeCacheMatrix creates a special object which is indeed a list of functions.
# These functions set and get a matrix, then set and get its inverse and cashe this.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL     
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve prints the inverse of the matrix created by makeCacheMatrix.
# If the inverse has already been calculated, it returns this calculation from
# cashe. If not, it calculates the inverse and put it into cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
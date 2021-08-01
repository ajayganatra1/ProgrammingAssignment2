# My cache inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # Get the matrix for inversion
    get <- function() x
    # matrix inversion
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
# Looking in the Cache
cacheSolve <- function(x,...){
    inv <- x$getinverse()
    if(!is.null(inv)){
        # pulling the inverse from cache
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}

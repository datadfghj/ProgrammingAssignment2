## This set of functions implement matrices capable of caching their inverses.

## Create a matrix. If the argument is omitted then 1x1 matrix is created with 
## a NULL in it. Use get and set methods of a returned (kind of) object to
## work with an underlying matrix. DO NOT use setinverse and getinverse
## methods, use the cacheSolve function instead.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Takes an object returned by the makeCacheMatrix function and returns an inverse
## of it's underlying matrix.

cacheSolve <- function(x) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                return inverse
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

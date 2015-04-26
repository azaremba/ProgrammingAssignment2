## The following functions are use to cache the inverse
## of a matrix. The assist to speed up the process
## of calculating the inverse of a matrix multiple times.

## This function will store a list of functions that
## are used to set a matrix as provided by user
## and to set (cache) the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the 
## matrix created above.  It first checks to see
## if the inverse has already been calculated, and if so
## it takes the saved inverse that has been cached using the
## function above in order to skip the computation.  If no 
## inverse has yet to be calculated it does so in this
## function then sets it to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        Matrix <- x$getmatrix()
        inv <- solve(Matrix, ...)
        x$setinverse(inv)
        inv
}

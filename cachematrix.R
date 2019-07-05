## The following functions are used to store a matrix and cache its inverse.

## makeCacheMatrix takes a matrix and creates a special "matrix" which is really
## a list that:

## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse
## 4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}


## cacheSolve computes the inverse of what is returned by the function above.
## If the inverse has already been solved, however, then cacheSolve returns the
## cached inverse instead of computing it again.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

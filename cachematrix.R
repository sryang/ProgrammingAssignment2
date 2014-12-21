## Matrix inversion can be costly so it would be a benefit to cache
## the inverse of the matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special matrix
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

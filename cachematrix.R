## These functions calculate and return a matrix which is the inverse
## of the input matrix. If the input matrix was seen previously, a
## cached value of the inverse matrix is returned, rather than
## recalculating the inverse all over again.

## makeCacheMatrix creates a list of four functions, which set and get
## the value of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
         )
}


## cacheSolve returns a matrix which is the inverse of the input
## matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(is.matrix(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Some examples to cut-n-paste:
## > A <- matrix(1:4, nrow = 2)
## > A
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a <- makeCacheMatrix(A)
## > b <- matrix(c(3,2,1,1,1,-1,0,1,2), nrow=3)
## > b
##      [,1] [,2] [,3]
## [1,]    3    1    0
## [2,]    2    1    1
## [3,]    1   -1    2
## > B <- matrix(c(3,2,1,1,1,-1,0,1,2), nrow=3)
## > b <- makeCacheMatrix(B)
## > cacheSolve(b)
##      [,1]       [,2]       [,3]
## [1,]  0.5 -0.3333333  0.1666667
## [2,] -0.5  1.0000000 -0.5000000
## [3,] -0.5  0.6666667  0.1666667
## > cacheSolve(b)
##      [,1]       [,2]       [,3]
## [1,]  0.5 -0.3333333  0.1666667
## [2,] -0.5  1.0000000 -0.5000000
## [3,] -0.5  0.6666667  0.1666667
## > 

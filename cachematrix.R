## Put comments here that give an overall description of what your
## functions do
## The purpose of these functions is to avoid calculating the
## inverse of a matrix repeatedly. Since the calculation of 
## the inverse of a matrix can be a time-consuming operation, 
## the inverse of a matrix is internally buffered.
## The inverse of a matrix will be calculated once at the 
## first request; on subsequent calls, the inverse is
## taken from the internal buffer.
## 
## Example:
## # load this file
## > source("cachematrix.R")
## # build a 3x3 matrix
## > M <- matrix(c(2,1,0,0,4,2,1,3,4), nrow=3, ncol=3)
## # construct an object oM
## > oM <- makeCacheMatrix(M)
## # first call: compute inverse of M
## > MInv <- cacheSolve(oM)
## # second call: the inverse is taken from the cache
## > MInv <- cacheSolve(oM)
## getting cached data
## # test if MInv is the inverse of M
## M %*% MInv

## Write a short comment describing this function
##
## This function works like a constructor for an instance of
## a 'Matrix class'
## The 'Matrix class' has the following attributes:
##      x           the matrix 
##      xinv        the inverse of the matrix
## The following getter/setter methods are available 
## in the 'Matrix class'
##      get()         returns the matrix
##      set(y)        replaces matrix x by matrix y
##      getinv()      returns the matrix xinv
##      setinv(yinv)  replaces xinv with yinv
## Note: the inverse has to be calculated separately.
##       This is done using cacheSolve given below.
## 
## An instance of this 'Matrix class' is internally represented 
## by a list.
##
makeCacheMatrix <- function(x = matrix()) {
    ## initialize with NULL. Null value indicates that
    ## the inverse has to be calculated
    xinv <- NULL
    ## replace matrix x with new matrix y
    set <- function(y) {
        x <<- y
        ## current inverse is no longer valid
        xinv <<- NULL
    }
    ## fetch the matrix 
    get <- function() x
    ## set the inverse of x. 
    ## We do not check whether inv is indeed the inverse of x
    ## The inverse is provided by cacheSolve
    setinv <- function(inv) xinv <<- inv
    ## getinv returns the inverse of x stored in xinv (can be NULL)
    getinv <- function() xinv
    ## the list of functions is returned 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## 
## this function takes an 'instance' provided by makeCacheMatrix
## as argument. It returns the inverse of the given matrix.
## If the inverse has already been calculated, it is returned from
## the cache, otherwise it will be calculated using solve(x).
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        ## return inverse from cache
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    ## calculate the inverse of x
    ## for simplicity, we may assume that the inverse exists ...
    ## otherwise we could use det(x) to test if x is invertible.
    xinv <- solve(data)
    x$setinv(xinv)
    ## return the calculated inverse
    xinv
}

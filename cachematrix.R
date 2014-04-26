## the file contains functions that cache the inverse of a matrix.
## 1. makeCacheMatrix() creates a "matrix" which can cache its inverse.
## 2. cacheSolve() computes the inverse of the matrix created by makeCacheMatrix()
##    if the inverse has already been calculated (and it matrix has not been changed), 
##    then it will return the cached inverse.


## makeCacheMatrix() creates a special "matrix" which stores its inverse.
## It returns a list containing functions of different operations on the "matrix"
## 1. set() set the value of the matrix 
## 2. get() get the value of the matrix
## 3. setinv() set the inverse of the matrix
## 4. getinv() get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve() calculates the inverse of the "matrix" created by makeCacheMatrix()
## it first call getinv()
## 1. If getinv() returns something, it will futher check its correctness.
##    If correct, rerurn the cached inverse.
## 2. Otherwise, it will call get() to get the matrix, and calcute the inverse
##    through solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    data <- x$get()
    correct <- FALSE
    ## to check whether the value returned is correct
    if (!is.null(inv)) correct <- identical(inv %*% data,diag(rep(1,ncol(data))))
    if (correct) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


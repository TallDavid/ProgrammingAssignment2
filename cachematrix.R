## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. This program will create a pair of functions that 
## cache the inverse of a matrix.

## NOTE: Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment it is assumed that the matrix supplied is always invertible.

## solve(a, b, ...)
## Arguments:
## a: a square numeric or complex matrix containing the coefficients of the linear system. 
##  Logical matrices are coerced to numeric.
## b: a numeric or complex vector or matrix giving the right-hand side(s) of the linear system. 
##  If missing, b is taken to be an identity matrix and solve will return the inverse of a.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverted matrix
## 4) get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setCacheMatrix <- function(solve) m <<- solve
    getCacheMatrix <- function() m
    list(set = set, get = get,
         setCacheMatrix = setCacheMatrix,
         getCacheMatrix = getCacheMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getCacheMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setCacheMatrix(m)
    m
}

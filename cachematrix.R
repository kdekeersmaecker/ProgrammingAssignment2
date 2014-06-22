## cacheMatrix.R - by K. De Keersmaecker
## 
## Modified from example code at URL:
## https://class.coursera.org/rprog-004/human_grading/view/courses/972139/assessments/3/submissions
##
## -----------------------------------------------------------------------------
## Purpose:
##
## As matrix inversion can be a costly computation, computing the inverse and
## caching it could be more beneficial than computing the inverse repeatedly.
## To this end, a pair of functions, i.e. 'makeCacheMatrix' and 'cacheSolve', 
## is defined:
##  - 'makeCacheMatrix' creates an object that can cache a matrix and its inverse
##  - 'cacheSolve'      is a function that computes the inverse of the special
##                      "matrix" object created by 'makeCacheMatrix' and caches it
##                      within the special "matrix" object; once the inverse is
##                      cached, subsequent function calls will retrieve the
##                      cached inverse instead of re-computing the inverse
##
## Usage:
##
## In order to compute and cache a matrix and its inverse, use this pair of
## functions as follows:
##  - First, create a special "matrix" object and store a matrix in it. This can
##    be done in two ways, either:
##      a. call the makeCacheMatrix() function with a matrix argument, and assign
##         it to a variable to keep a handle on it and make it persistent, e.g.
##         m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2))           
##      b. call the makeCacheMatrix() function without an argument, and assign it
##         to a variable; next, use the 'set' function of this newly created
##         "matrix" object to set and cache in it a matrix value, e.g.
##         m <- makeCacheMatrix() ; m$set(matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2))
##  - Next, call the cacheSolve() function on the special "matrix" object just
##    created by the makeCacheMatrix() function. This will compute the
##    inverse of the matrix cached in the special "matrix" object, and cache
##    the inverse in the same special "matrix" object
##  - As long as the matrix value in the special "matrix" object is not re-set, 
##    calling the cacheSolve() function will retrieve the cached inverse.
## 
## Comment:
##
## This pair of functions exploits lexical scoping in R. 
##
## By assigning the special "matrix" object created by the makeCacheMatrix() 
## function to a variable, a matrix value and an inverse matrix value can be 
## cached in the makeCacheMatrix closure linked to this assigned variable. 
## 
## The persistent cache variables are the formal argument 'm' and the 
## local variable 'cachedinverse'; they can be accessed as long as the assigned 
## variable persists. The cache variables are defined in the parent environment
## of the subfunctions. The subfunctions defined within makeCacheMatrix() gain 
## access to these cache variables via the '<<-' operator; using the regular '<-' 
## operator would have defined variables local to the subfunctions, 
## without any link to the cache variables.
##
## The cacheSolve function accesses these cache variables via the subfunctions
## linked to the special "matrix" object that was passed to it as an argument.
## -----------------------------------------------------------------------------
## _________________________
##
## FUNCTION: makeCacheMatrix
## _________________________
##
## makeCacheMatrix() acts as a wrapper that caches values and provides functions
## to set and retrieve these cached values.
##
## makeCreates a special "matrix" object, which is a list containing a function to
##  - 'set': sets and caches the value of the matrix; resets the cached inverse to NULL
##  - 'get': gets the value of the cached matrix
##  - 'setinverse': sets and caches the inverse of the matrix 
##  - 'getinverse': gets the cached inverse matrix (can be NULL if not yet computed)
## This special "matrix" object caches a matrix and an inverse matrix within
## the closure of the makeCacheMatrix() function.
## Setting the cached matrix value by either calling makeCacheMatrix() with an 
## argument or by explicitely setting the value via the set() function, both reset
## the cached inverse matrix to NULL.
##
## Note that the current implementation allows direct access to the setmean() 
## function, which voids guarantee that the cached inverse is the actual inverse
## of the cached matrix; the setinverse() and getinverse() functions are intended
## to be called only from within the cacheSolve() function.
## 

makeCacheMatrix <- function(x = matrix()) {
    cachedinvmatrix <- NULL
    set <- function(y) {
        x <<- y
        cachedinvmatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(m) cachedinvmatrix <<- m
    getinverse <- function() cachedinvmatrix
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## ____________________
##
## FUNCTION: cacheSolve
## ____________________
##
## cacheSolve computes the inverse of a special "matrix" object x
## created with the makeCacheMatrix function described above.
##
## It first checks whether the inverse of the matrix has been computed before,
## by retrieving the cached value from within the special "matrix" object that 
## was passed as an argument. If this cached value is not NULL, it returns
## the cached value and skips computing the inverse. If the cached value is NULL,
## and thus the inverse has not been calculated before, it computes the inverse,
## caches this inverse in the special "matrix" object argument, and returns the
## value of this inverse.
##
## Note: the matrix cached in the special "matrix" object should be inversible.
## If not, error messages will be raised by the solve() function used in the 
## cacheSolve() function.
##
## Note that the solve() function is forced to compute the inverse of the matrix
## by passing it an identity matrix; if omitted, the solve function could be 
## overruled to compute the solution to a general x %*% ? = b equation via the
## optional arguments.
##
## Note also that cacheSolve() accesses the cache variables of x via the helper 
## functions defined within makeCacheMatrix() linked to x. None of the variables 
## within cacheSolve() (i.e. x, invmat, data) are in anyway linked directly
## to the cache variables within x; their names can thus differ from the ones
## in the makeCacheMatrix() function.
## 

cacheSolve <- function(x, ...) {
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, diag(nrow(data)), ...)
    x$setinverse(invmat)
    invmat
}

## --- END OF FILE -------------------------------------------------------------

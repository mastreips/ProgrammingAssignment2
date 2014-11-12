## Put comments here that give an overall description of what your
## functions do

# AUTHOR: Marcus A. Streips
# DATE: November 11, 2014
# Version: 1.00
# 
# makeCacheMatrix takes in a matrix and stores it as a global variable (global_x), it 
# also provides maintenance functions for storing, accessing and handling matrix objects and 
# their inverses. The function is a list of functions. 
# 
# cacheSolve calls the makeCacheMatrix function to get the matrix and inverse matrix of
# any makeCacheMatrix object that is passed to it.  It then determines whether an inverse
# matrix has already been solved using the if statement and the identical() function
# referencing the global variable (global_x) to determine if the matrix has been
# solved before. If the logical condition evaluates to the true, the cacheSolve function
# calls the makeCacheMatrix function to return the cached matrix, otheriwise, the inverse
# is calculated, stored in the makeCacheMatrix object and returned. 
# 
# PLEASE NOTE:  Functions give the correct output according to Unit tests (with expected 
# output) for Programming Assignment 2 
# 
# see: https://class.coursera.org/rprog-009/forum/thread?thread_id=164

## Write a short comment describing this function

# This function is a list of functions for storing, managing and accessing matrices 
# and their inverses.  It uses global variables (via the superassignment operator) to 
# store the original value of the matrix.  The matrix may be directly changed using 
# the set function, which will skip the definition of the global variable, meaning that
# the resulting modified object will never be recongized as having been calculated 
# previously. This recalls the warning in the HW example to NOT call the set function
# directly, despite it being accessible. 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        store <- x
        global_x <<- store
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinverse = setinv,
             getinverse = getinv)
} 

## Write a short comment describing this function

# This function takes in the special matrix object created by the makeCacheMatrix,
# determines whether the matrix is different from that already cached (via the
# identical function) and either returns the cached inverse matrix or calculated a 
# new inverse function and returns that value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$get()
        m <- x$getinverse()
        if(!is.null(m) & identical(global_x,y)){
                message("getting cached matrix")
                return(m)
        }
        m2 <- x$get()
        m <- solve(m2, ...)
        x$setinverse(m)
        return(m)
}



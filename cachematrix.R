## This program contains two functions: 
##      a. makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##      b. cacheSolve function computes the inverse of a special matrix. If the inverse of the
##         matrix is exsited in cache, then function returns the previous calcualtion, 
##         otherwise, it solves the problem using solve function.

## makeCacheMatrix function
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set=set,get=get, 
             setInverse = setInverse,
             getInverse = getInverse)
        
}




## cacheSolve function
##      1. Find the inverse matrix of a special "matrix" from cache
##      2. If found, then it returns the inverse matrix and print a message
##      3. If not found, then it computes using solve function



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}

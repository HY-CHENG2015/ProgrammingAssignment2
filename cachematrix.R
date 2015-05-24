
## This assignment is to write an R function that is able to cache potentially time-consuming computations. 
## It includes two functions "makeCacheMatrix" which creates a special matrix object and "cacheSolve" which computes the inverse of a matrix. 

## "makeCacheMatrix" function creates a special matrix object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
         setinv <- function(inv) inverse <<- inv 
         getinv <- function() inverse
         return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
    } 


## "cacheSolve" function computes the inverse of the special matrix created by "makeCacheMatrix". 
## If the inverse has already been calculated, then "cacheSolve" retrievez it from the cache. 
## Else it computes inverse and caches it returns inverse. 

cacheSolve <- function(x, ...) { 
    inverse <- x$getinv() 
         if(!is.null(inverse)) { 
                 message("getting cached data...") 
                 return(inverse) 
             } 
         data <- x$get() 
         inverse <- solve(data, ...) 
         x$setinv(inverse) 
         return(inverse) 
     } 


## Exam result
a <- makeCacheMatrix(2)
cacheSolve(a)

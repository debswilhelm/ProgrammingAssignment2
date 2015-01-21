## R Programming Course - Assignment #2
## debswilhelm  1/18/2015
##



makeCacheMatrix <- function(x = matrix()) {
## Creates a special "matrix", which is really
## a list containing a function to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse (solve)
##      get the value of the inverse (solve)

## Initialize m to be empty
        m <- NULL
  
## Construct the set function
        set <- function(y) {
            x <<- y
            m <<- NULL
  }
## Construct the get function
        get <- function() x
## Construct the setsolve function
        setsolve <- function(solve) m <<- solve
## Construct the getsolve function
        getsolve <- function() m
## Store the function list
        list(set = set, 
		     get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



cacheSolve <- function(x, ...) {
## The following function calculates the matrix inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via the 
## solve function.

        m <- x$getsolve()

## Determine if the the inverse value was previously calculated
        if(!is.null(m)) {
  
## m is not null which means the value was cached, just return with message
             message("getting cached data")
             return(m)
        }
		
## m was null so calculate and return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
  
}


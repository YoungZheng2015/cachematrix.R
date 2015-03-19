## Coursera R Programming Assignment 2

## 'makeCacheMatrix' function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               	# Initialize variable i to NUll.
        set <- function(y) {    	# 'y' is the matrix argument passed
                                	# into 'makeCacheMatrix'.
                x <<- y         	# Set 'x' for the function enviromnent to 'y'.
                i <<- NULL      	# Set 'i' for the 'makeCacheMatrix' environment to
                                	# NULL.
        }
        get <- function() x     	# Create a function 'get' in the 'makeCacheMatrix'
                                	# parent and assign the matrix to it.
        setinverse <- function (inverse) {
        i <<- inverse			        # Take a value ('inverse') and set it to the
                                	# value of 'i' in the 'makeCacheMatrix' frame.
}
        getinverse <- function() i 	# return the value of 'i' from the
                                	  # 'makeCacheMatrix' frame.
        list(set = set, get = get,  # List out the values of the functions in the
        setinverse = setinverse,    # 'makeCacheMatrix' frame.
        getinverse = getinverse)
}

## 'cacheSolve' function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                # Go to the 'x' environment and assigns the 
                                     # 'i' value from that environment to this one.
  if(!is.null(i)) {                  # If the 'x' environment has been evaluated 
                                     # before, the function prints the message and
                                     # of the cached inverse value i.
    message("getting cached data")
    return(i)
  }
  data <- x$get()                    # If this particular 'x' has never been
                                     # evaluted before, pull the x matrix into a
                                     # local varaible called 'data'.
  i <-  solve(data, ...)             # Calculate the inverse of the x matrix by calling
                                     # solve function on the data local variable.
  x$setinverse(i)                    # Assign the calculated inverse to the 'x'
                                     # environment using the 'setinverse' function.
  i                                  # Display the calculated inverse.
}

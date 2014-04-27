## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This code  defines a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to:
##  1. set the value of the vector
##  2. get the value of the vector
##  3. set the value of the mean
##  4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
      
      inverse_matrix <- NULL
      set <- function(y) {
            x <<- y
            inverse_matrix <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inverse_matrix <<- inverse
      getinverse <- function() inverse_matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}



      
## This function calculates the mean of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverser of the data 
## and sets the value in the cache via the setinversr function.

cacheSolve <- function(x, ...) {
      inverse_matrix <- x$getinversr()
      if(!is.null(inverse_matrix)) {
            message("getting cached data")
            return(inverse_matrix)
      }
      data <- x$get()
      inverse_matrix <- solve(data, ...)
      x$setinverse(inverse_matrix)
      inverse_matrix 
         
}

      
     

## Write a short comment describing this function
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverseof a mtrix rather than compute repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
# Thr following function returns the inverse of the matrix. It first 
# checks if the inverse has already been computed. If so, it gets
# the result and skips the computation. If not, it computes the
# the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

        ## Return a matrix that is the inverse of 'x'
        
        ##Sample run:
      ## > x = rbind(c(1,-1/4), c(-1/4, 1))
      ## > m = makeCacheMatrix(x)
      ## > m$get()
      ##       [,1]  [,2]
      ## [1,]  1.00 -0.25
      ## [2,] -0.25  1.00

      ## > cacheSolve(m)
                [,1]      [,2]
      ## [1,] 1.0666667 0.2666667
      ## [2,] 0.2666667 1.0666667
      
      ## Retrieving from thecache in the second run
      ## > cacheSolve(m)
      ## getting cached data.
      ##        [,1]      [,2]
      ## [1,] 1.0666667 0.2666667
      ## [2,] 0.2666667 1.0666667

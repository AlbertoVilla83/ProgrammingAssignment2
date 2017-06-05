## The following pair of functions allow to cache and compute the inverse of a matrix. 
## In particular, makeCacheMatrix creates a special "matrix", which 
## is a list containing a function to set/get the values of the matrix 
## and set/get the values of its inverse.
## cacheSolve returns the inverse of the matrix, skipping computations if the 
## inverse has been already computed and stored 


## Function to create a list of functions to set/get the values of a matrix and
## its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## check if the inverse is already cached: if yes, gets it
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("Getting inverse matrix from cached data")
      return(inv)
    }
    
    ## Compute the inverse if it is not cached
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

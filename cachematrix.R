## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Function which creates a matrix object with utility 
## functions for getting/setting it's value and inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse
  inv <- NULL
  
  ## Get matrix
  get <- function() x
  
  ## Set matrix, always reset the inverse on a 'set'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix inverse
  getInverse <- function() inv
  
  ## Set the matrix inverse
  setInverse <- function(solve) inv <<- solve
  
  ## Returns a special matrix which is essentially a list of utility
  ## functions for getting/setting attributes of a matrix
  return list(get=get, set=set,  getInverse=getInverse, setInverse=setInverse)

}


## cacheSolve - 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Gets the current set value of the matrix inverse
  inv <- x$getInverse()
  
  ## Check if value has been computed and cached already
  if(!is.null(inv)) {
    message("using cached data")
    return(inv)
  }
  
  ##  Cache has not been set, compute the inverse and set it
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setInverse(inv)
  
  return inv
  
}


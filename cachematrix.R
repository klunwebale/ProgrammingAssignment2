## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    matx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(matx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Write a short comment describing this function

cacheSolve <- function(matx, ...) {
        ## Return a matrix that is the inverse of 'matx'
  inverse <- matx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- matx$get()
  invserse <- solve(data, ...)
  matx$setinv(inverse)
  return(inverse)
}

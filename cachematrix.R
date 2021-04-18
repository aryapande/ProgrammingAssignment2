## makeCacheMatrix stores the inverse of a matrix so that it need not be computed everytime. 

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(m){
    x <<- m
    inverted <<- NULL
  }
  get <- function()x
  setinv <- function(i) inverted <<- i
  getinv <- function() inverted 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinv()
  if(!is.null(inverted)){
    message("getting cached data")
    return(inverted)
  }
  mat <- x$get()
  inverted <- solve(mat) 
  #since another argument is missing, solve gives the inversion of said matrix
  x$setinverse(inverted)
  inverted
}

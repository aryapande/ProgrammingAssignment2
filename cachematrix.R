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


## checks to see if inverse of matrix exists already, if not, it calculates and stores for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinv()
  if(!is.null(inverted)){
    return(inverted)
  }
  mat <- x$get()
  inverted <- solve(mat) 
  #since another argument is missing, solve gives the inversion of said matrix
  x$setinverse(inverted)
  inverted
}

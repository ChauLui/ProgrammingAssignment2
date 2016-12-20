## Functions to make use of scoping rules in R to cache matrix inverse values
## rather re-calculation values when inital matrix remains the same.

## this function provides up the set/get functions for the special 
## matrix object.

makeCacheMatrix <- function(x = matrix()) {
  invM = NULL
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  get <- function()x
  setInvM <- function(inv) invM <<- inv
  getInvM <- function() invM
  list(set=set, get=get, setInvM=setInvM, getInvM=getInvM)
}


## This function checks if inverse matrix was calculated.
## If so, retrieve inverse matrix; otherwise calculates inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invcM <-x$getInvM()
  if (!is.null(invcM)){
    message("getting cached inverse matrix")
    return (invcM)
  }
  data <- x$get()
  invcM <- solve(data, ...)
  x$setInvM(invcM)
  invcM
}

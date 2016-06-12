## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeTheCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(mat_again) 
  {
    mat <<- mat_again
    inv <<- NULL
  }
  get <- function() mat
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set=set, get=get,setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(specialM, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv  <- specialM$getInv()
  if(!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  data <- specialM$get()
  inv <- solve(data, ...)
  specialM$setInv(inv)
  inv
}

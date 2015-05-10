## Data science course assignment 2

## create a special object which can cache the inverse of the given matrix
## the problem is how to avoid name clashes

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL  # will get error first time call getinv() if not set
  # cached.m <<- x

  set <- function(y) {
     x <<- y
     im <<- NULL
  }
  get <- function() x
  getinv <- function() im
  setinv <- function(inv) im <<- inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## get the inverse of the matrix object created by makeCacheMatrix
## if the inverse was already made and the matrix was not changed
## just return the cached inverse. Otherwise compute the inverse
## and cache it before returning it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinv()
  if(!is.null(im)){
    message("return cached inverse")
    return(im)
  }
  im = solve(x$get())
  x$setinv(im)
  im
}

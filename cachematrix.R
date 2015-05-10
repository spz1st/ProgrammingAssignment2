## Data science course assignment 2

## create a special object from a matrix
## so the inverse of the matrix can be cached

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL  # will get error first time call of getinv() if not set

    set <- function(y) {  # needed to change the matrix
        x <<- y
        im <<- NULL
    }
    get <- function() x   # x is saved in the object as the local variable
    getinv <- function() im  # im is saved in the object as the local variable
    setinv <- function(inv) im <<- inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## get the inverse of the matrix object created by makeCacheMatrix
## if the inverse was already made, just return the cached inverse.
## Otherwise compute the inverse and cache it before returning it.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    im <- x$getinv()
    if(!is.null(im)){
        message("return cached inverse")
        return(im)
    }
    im = solve(x$get())
    x$setinv(im)
    im
}

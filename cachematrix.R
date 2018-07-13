## This function will create a matrix that can cache its inverse it will
## calculate the inverse of that matrix unless it has already been calculated in
## which case it will retrieve the information from cache

## This function creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
  
    invmtrx <- NULL
    set <- function(y) {
      x <<- y
      invmtrx <<- NULL
    }

    get <- function() x 
    setinvmtrx <- function(solve) invmtrx <<- solve()
    getinvmtrx <- function() invmtrx
    list(set = set, get = get,
         setinvmtrx = setinvmtrx,
         getinvmtrx = getinvmtrx)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  
    invmtrx <- x$getinvmtrx()
    if(!is.null(invmtrx)) {
      message("getting cached data")
      return(invmtrx)
    }
    data <- x$getinvmtrx()
    invmtrx <- solve(data, ...)
    x$setinvmtrx(invmtrx)
    invmtrx
  
}

## These functions calculate the inverse of a matrix and caches it
## in case of having the value in cache and the matrix is the same
## we return the cached matrix, in other case it calculates it.

## This function caches the inverse of a matrix, if it has been
##calculated returns the value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  changed <- FALSE
  set <- function(y){
    x <<- y
    m <<- NULL
    changed <<- identical(x,y)
  }
  
  get <- function() x
  setinverse <- function(inversematrix) m <<- inversematrix
  getinverse <- function() m
  haschanged <- function() changed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       haschanged = haschanged)
}


## This function calculates the inverse of the matrix and caches it.
## In case it has it in cache and the matrix has not changed 
## returns the cached matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m) && x$haschanged()){
    message("Get cached data")
    return(m)
  }
  originalmatrix <- x$get()
  m <- solve(originalmatrix)
  x$setinverse(m)
  m
}
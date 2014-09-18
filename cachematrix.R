## Put comments here that give an overall description of what your
## functions do

## This function creates a list with a set, get, setinverse
## and getinverse function.  m is the inverse that is calculated, so if a new
## matrix is created (using set) then m is set to null again in the calling 
## function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the matrix and calles uses the methods defined in 
## makeCacheMatrix to get the inverse of the matrix.  It starts by checking
## if the inverse has already been created, if it has, it returns the previously
## calculated inverse matrix.  If not, it will calculate it and set the value
## in the makeCacheMatrix list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

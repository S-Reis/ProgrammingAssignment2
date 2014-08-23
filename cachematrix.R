## Put comments here that give an overall description of what your
## functions do

##These functions calculate the inverse of a matrix and caches the value
##so that if the came matrix is run again it will return the cached value
##rather than recalculating the inverse.


## Write a short comment describing this function

##This function stores the object matrix given to the function 
##and resets the holding variable, m, to NULL so it can received 
##the inverse calculation. It also stores the cached inverse 
##matrix calculations already done.

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


## Write a short comment describing this function

##The function first checks if the inverse of the matrix 
##has already been calculated(If m is not NULL). If so, it
##returns the statement “getting cached data” and returns 
##the inverse that was calculated previously and cached.
##If it has not been calculated before (If m is NULL) the 
##function will calculate the inverse and cache it. 

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

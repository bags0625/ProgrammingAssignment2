## makeCache matrix take the inverse of a matrix and cache the result for use from 
## memory rather than recomputing the inverse. chacheSolve gives the solution for
## the cached matrix inverse.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize inv  
  inv = NULL
  set = function(y) {
    #use `<<-` to assign a value to an object in an environment 
    #different from the current environment 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##This object calculates the inverse if the inverse has not yet been calculated, otherwise it
##pulls the value from the cache.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  #if the inverse has already been calculated then pull it from the cache
  #check if null or value
  if (!is.null(inv)){
    message("getting cached data")
    #return value
    return(inv)
  }
  
  #if not cached, calculate the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  #use setinv to calculate the inverse
  x$setinv(inv)
  
  return(inv)
}
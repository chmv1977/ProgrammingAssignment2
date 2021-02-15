## This is my function "makeCacheMatrix", 
## which next action do:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {

    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function return a matrix that is the inverse of 'x'
## if the inverse has already been calculated
## get it from the cache and skips the computation.

cacheSolve <- function(x, ...) {

 inv = x$getinv()
   
  if (!is.null(inv)){
  
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()

  inv = solve(mat.data, ...)
   
  x$setinv(inv)
  
  return(inv)
}

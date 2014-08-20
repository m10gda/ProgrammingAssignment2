## Caching the Inverse of a Matrix

## It creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(X = matrix(1,1,1)) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  It first checks to see if the matrix has already been inversed.
## If so, it gets the result from the cache and skips the computation. 
## Otherwise, it calculates inversed matrix and sets the result in the cache.

cacheSolve <- function(X) {
  inv <- X$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data)
  X$setinv(inv)
  inv
}
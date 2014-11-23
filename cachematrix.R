## Pair of functions allowing the user to create and retrieve a cached value for an object

## Store a user input matrix and cache an empty inverse matrix for that user input

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The first time this function is called it will evaluate the inverse of the matrix entered and rewrite the cache value for that matrix.
## The second time this function is called it will retrieve and return the cached inverse matrix value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.na(inv[1,1])) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

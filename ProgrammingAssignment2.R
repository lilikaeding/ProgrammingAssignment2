makeCacheMatrix <- function(x = matrix()) {
  m <- null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
#creates matrix object
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!isnull(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  #computes inverse of matrix 
}


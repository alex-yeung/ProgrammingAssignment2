## The following two functions creates a special object that stores a matrix and cache its inverse.
## The makeCacheMatrix function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## Initialize the inverse property
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
## Set the inverse of the matrix
  setinverse <- function(inverse)
    inv <<- inverse
## Get the inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special matrix object created from function above. It first checks
## whether the inverse has already been calculated (and the matrix remains unchanged), if so it retrieves the inverse from the cache.
## Otherwise, it calculates the inverse of a matrix.

## Computes the inverse of the matrix by makeCacheMatrix
cacheSolve <- function(x, ...) {
## Give the inverse of the original matrix input in makeCacheMatrix
  inv <- x$getinverse()
## If the inverse has already been calculated
  if(!is.null(inv)) {
## then it is retrieved from the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
## Otherwise, compute the inverse of the matrix by matrix multiplication
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}


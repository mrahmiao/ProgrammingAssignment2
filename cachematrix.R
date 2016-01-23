## Use these two functions to compute the inverse of a given matrix, and cache the inverse for later use
## so that repeated computations can be saved.

## makeCacheMatrix is used to generate a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL

  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    cachedInverse <<- NULL
  }

  getMatrix <- function() {
    x
  }

  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }

  getInverse <- function() {
    cachedInverse
  }

  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve is used to compute the inverse of the given matrix x, which is genreated by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()

  if (!is.null(inverse)) {
    message("Existing cached inverse.")
    return(inverse)
  }

  existingMatrix <- x$getMatrix()
  inverse <- solve(existingMatrix, diag(nrow(existingMatrix)), ...)
  x$setInverse(inverse)
  inverse
}

## These 2 functions implement a matrix that caches it's inverse. The inverse of the
## matrix is computed only the first time and cached. When the inverse of the matrix
## is required subsequently the prior computed value is returned  instead.
## functions do

## This function takes a matrix as an input and returns a list with named parameters.
## The list and its named parameters are used by cacheSolve function to actually
## compute the matrix inverse and cache it.

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inv) matInv <<- inv
  getInverseMatrix <- function() matInv
  list(x, set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This matrix takes the list returned from the makeCachezmatrix method and returns the
## inverse of the matrix. The function computes the inverse if it is not already computed
## and cached.

cacheSolve <- function(x, ...) {
  matInv <- x$getInverseMatrix()
  if (!is.null(matInv)) {
    message("getting cached matrix inverse")
    return (matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInverseMatrix(matInv)
  matInv
}


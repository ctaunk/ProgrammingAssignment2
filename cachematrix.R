## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inv) matInv <<- inv
  getInverseMatrix <- function() matInv
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


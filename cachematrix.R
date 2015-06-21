## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix() makes a matrix object (described in more detail below) and 
#cacheSolve() returns the inverse of the matrix, returning it from cache, if previously calculated.
# see more below

## Pass makeCacheMatrix() a matrix to set it as the matrix and return a list of functions.

#These functions are summarized below:
# get() returns the matrix, 
# set(MatrixToSet) to set the matrix,
# setInverse(MatrixToSet) to set the cached inverse matrix, 
# getInverse() returns the cached inverse matrix (which will be NULL if not set).

makeCacheMatrix <- function(storedMatrix = matrix()) {
  
  #initialize cache matrix variable to NULL
  inverseMatrix <- NULL
  
  # set the stored matrix
  set <- function(theMatrix) {
    x <<- theMatrix
    inverseMatrix <<- NULL
  }
  
  # get the stored matrix
  get <- function() {
    storedMatrix
  }
  
  # set the stored cache of the inverse matrix
  setInverse <-function(inverseMatrixToSet) {
    inverseMatrix <<- inverseMatrixToSet
  }
  
  # get the stored cache of the inverse matrix
  getInverse <-function() {
    inverseMatrix
  }

  # return the list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


#cacheSolve takes in the output of makeCacheMatrix(), sets the inverse 
# of the stored matrix, and returns the inverse. The inverse will be cached and 
# returned on future calls of the function.

cacheSolve <- function(storedMatrix, ...) {
        ## Return a matrix that is the inverse of the input
  inverseMatrix <- storedMatrix$getInverse()
  
  # check if inverse was previously stored- if so, return it
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  #otherwise compute the inverse, store it, and return it
  data <- storedMatrix$get()
  inverseMatrix <- solve(data)
  storedMatrix$setInverse(inverseMatrix)
  inverseMatrix
  
}

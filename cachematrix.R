
## The fist function calcultes the inverse of a given matrix and returns this value
## The second function returns the inverse matrix calculated by the first function, if the matrix has not changed.
## If the matrix has already changed, the second function will calculate again the inverse matrix.

## This funtion receives a matrix and calculates and returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inv <<- inverse
  getinverse <- function() matrix_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function returns the inverse matrix calculated for the first function, if this matrix has not changed.
##If the matrix has changed, this funtion recalculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$getinverse()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data, ...)
  x$setinverse(matrix_inv)
  matrix_inv
}


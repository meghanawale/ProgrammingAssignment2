## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inv_x is variable to store inverse of input matrix
  inv_x <- NULL
  set <- function(y) {      ##this function set the matrix value
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse ## function is created to set inverse
  getinverse <- function() inv_x
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached data") ## If inverse of matrix is present in cache then it returns the matrix inverse
    return(inv_x)
  }
  
  matrix1 <- x$get()
  inv_x <- solve(matrix1, ...)  ## This function create the inverse of matrix
  x$setinverse(inv_x)
  inv_x
}

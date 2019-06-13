## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix
# Arguments: x - an object of type matrix, ideally it is a square, invertible
# matrix. That is, there exists some matrix B such that Bx = I where I is the
# identity matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  X_inv <- NULL
  
  set <- function(A) {
    x <<- A
    X_inv <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inverse) X_inv <<- inverse
  
  get_inv <- function() X_inv
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  
  # get inverse
  inv <- x$getinverse()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  # return inverse
  inv
        
}

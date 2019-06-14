## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix
# Arguments: x - an object of type matrix, ideally it is a square, invertible
# matrix. That is, there exists some matrix B such that Bx = I where I is the
# identity matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  X_inv <- NULL
  A <- NULL
  
  set <- function(A) {
    x <<- A
    X_inv <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inverse) x <<- inverse
  
  get_inv <- function() A
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x=matrix(), ...) {
  
  # get inverse
  X_inv <- x$get_inv()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(X_inv)) 
  {
    if(x$set() == x$get())
    {
      message("getting cached data")
      return(X_inv)
    }
    
  }
  
  # if not, get matrix
  A <- x$get()
  
  x$set(A)
  # compute inverse of matrix
  X_inv <- solve(A, ...)
  
  # cache inverse of matrix
  x$set_inv(X_inv)
  
  ## Return a matrix that is the inverse of 'x'
  # return inverse
  X_inv
        
}

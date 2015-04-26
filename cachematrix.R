## Put comments here that give an overall description of what your
## functions do

# Given a matrix, return a matrix wrapped in an object that contains a 
# cached inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  # Provide a new matrix to the object.  Clear any cached inverse since
  # it is no longer valid.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Return the current matrix embodied in this object.
  get <- function() x
  
  # Cache a provided value of an inverse of this matrix.
  setinv <- function(thisInv) {
    inv <<- thisInv
  }
  
  # Return the inverse of the current matrix.  This will be NULL if no
  # inverse has been cached; the cached value if it has.
  getinv <- function() inv
  
  # Return a list of all functions that can be called on this CacheMatrix
  # object.
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}

# Given a CacheMatrix object, return the inverse of the underlying matrix.
cacheSolve <- function(x, ...) {
    
    # Check the object's cache for an inverse.  If it does not exist,
    # compute it and add it to the object's cache.
    if( is.null(x$getinv()) ) {
      x$setinv(solve(x$get(), ...))
    }
    
    # Return the inverse from the CacheMatrix object.
    x$getinv()
}

testCacheMatrix <- function(x = matrix()) {
  
  M <- makeCacheMatrix(x)
  cacheSolve(M)
}


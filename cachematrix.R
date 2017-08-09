## Goal of this assignment is to demonstrate lexical scoping and to create a pair
## of functions (makeCacheMatrix) and (cacheSolve), which will allow you to compute
## and store the inverse of a matrix, so that if the inverse already exists, it does
## not need to be recomputed, but can simply be looked up in the cache. 

## This function will create a matrix object which can cache the input's inverse

makeCacheMatrix <- function(x = matrix()) {
  store_inv <- NULL
  set <- function(y) {
    x <<- y
    store_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) store_inv <<- inverse
  getinv <- function() store_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will check to see if the inverse of the matrix object created above
## has already been calculated. If it has, it will pull from the cache. If not,
## it will calculate the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    print("Cached result found. Retrieving.")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

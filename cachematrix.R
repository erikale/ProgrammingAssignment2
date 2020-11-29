## MakeCacheMatrix is a function that can cache its inverse

## In this function first we set the value of matrix, get the value of the matrix
##then we set the inverse of the matrix and the we get 
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() (x)
  setInverse <-function(inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list(set=set, get =get, setInverse=setInverse, getInverse=getInverse)
}


## CacheSolve is a function that computes the inverse of the matrix created with the previous function. First we
## obtain the inverse of X if the inverse has been already calculate the computation can be skiped
## but if the inverse is going to be obtained from the cache the message "getting data will be dispplayed"
## and the inverse will be returned.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse (inv)
  inv
  
  ## Return a matrix that is the inverse of 'x'
}

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the 
## inverse of a matrix rather than compute it 
## repeatedly.

## With makeCacheMatrix I want to create a list 
## containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## This function works if X is a square invertible 
## matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
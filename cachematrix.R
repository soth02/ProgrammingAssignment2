## These two functions, makeCacheMatrix and cacheSolve, combine to return the inverse of a matrix, first checking to see if
## the solution has been cached and returning that result if so.

## example:
## mymatrix<-matrix(1:4,2,2)
## mycachematrix<-makeCacheMatrix(mymatrix)
## cacheSolve(mycachematrix)

## makeCacheMatrix takes an invertible matrix as input.  It creates an object which can get/set the original matrix and
##get/set the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {

  ##clear local m
  m <- NULL
  
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'.  First it checks to see if the result has already been cached, and
## returns that value if present.  If not present, the inverse is calculated with solve() and cached.
cacheSolve <- function(x, ...) {
  
  ## retrieve cached inverse if exists
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #get original matrix
  data <- x$get()
  
  ## compute inverse
  m <- solve(data, ...)
  
  ## cache inverse matrix
  x$setinverse(m)
  
  ## return inverse matrix
  m
}

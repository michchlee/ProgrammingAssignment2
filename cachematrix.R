
# Matrix inversion is usually a costly computation
# caching inverse of a matrix instead of computing it repeatedly seems 
# more efficient 

## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  ##initialise the inverse matrix
  m <- NULL
  
  #function to set value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##function to return the value of the matrix
  get <- function() {x}
  
  ##function to set value of the inverse matrix
  setinv <- function(inv) {m <<- inv} 
  
  #function to return the value of the inverse matrix
  getinv <- function() {m}
  
  # lists the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
  
}
## Test 
a <- matrix(c(1,2,3,4),2,2)
aa<-makeCacheMatrix(a)
cacheSolve(aa)



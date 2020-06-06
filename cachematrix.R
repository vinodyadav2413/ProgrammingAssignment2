## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ## initialize inv as NULL,holds inverse value 
  set <- function(y) {                    ##  set function to assign new value
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if matrix exists, assign NULL to inv
  }
  get <- function(){ x}                     ## get fucntion - returns x
  
  setinverse <- function(inverse) {inv <<- inverse } ## assigns value of inv in parent environment
  getinverse <- function() {inv }                    ## returns inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  ## to the functions with the $ operator

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ##returns the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##gets the matrix
  data <- x$get()
  inv <- solve(data, ...) ##calculates inverse
  x$setinverse(inv) ##sets the inverse of matrix
  inv ##returns matrix
}



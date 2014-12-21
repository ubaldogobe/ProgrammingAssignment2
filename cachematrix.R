## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "matrix", which is really a list 
##containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL 
  }
  get <- function() x 
  setInverse <- function(solve) m<<- solve 
  getInverse <- function() m 
  ## create list with all functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  

}


## Write a short comment describing this function

##The following function calculates the inverse matrix of the special "matrix" created with 
##the above function. However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
##via the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
## Try to assign inverse to m
   m <- x$getInverse()
# check if inverse is in cache:
# yes: return the cache
# no: get the matrix used by makeCache Matrix, get the inverse and store it in cache
#with makeCacheMatrix set function

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

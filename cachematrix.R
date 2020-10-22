## Put comments here that give an overall description of what your
## functions do
## The function below are twi functions that develip a special object which stores a matrix and caches its inverse. 
## Write a short comment describing this function
##The makeChacheMatrix function creates a special "matrix" object that can cahce its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse, setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## The cacheSolve function calucalted the inverse of the special "matrix" dervied from the makeCacheMatrix function. If the inverse is alreadyt calculated then it should
##get the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}

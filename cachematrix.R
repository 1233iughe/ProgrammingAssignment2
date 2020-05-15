## Put comments here that give an overall description of what your
## functions do

#The functions makeCacheMatrix and cacheSolve create an special object
#that allows the calculation and storage of an inverse matrix. Also can call
#a previously caculated inverse matrix from the cache.

## Write a short comment describing this function

#This function creates an list of functions to set 
#and get the inverse of a matrix. Also it caches the inverse of a newly 
#calculated inverse matrix, so less computational time is needed if its 
#requested again.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#Use the functions stored in the list produced by makeCacheMatrix() 
#and set or get the innverse of a matrix. If it was registered before
#it takes the inv matrix from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}



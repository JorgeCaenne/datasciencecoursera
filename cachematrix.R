## Creates a list (of actions) where a vector is specified, set and equipped the the value
## of the inverse and conclusivly gets this value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
}

'Asks the general environment if inverse exist, if not performs inverse operation
utilizes subfunctions defined in makeCacheMatrrix that are now generally 
available in the environment'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # solve calculates the inverse as part of the R base
  x$setinv(inv)
  inv
}
## Return a matrix that is the inverse of 'x'
## If already cached return inverse and message specified above

#Defining two matrizes for testing
testmata <- matrix(1:4, nrow=2, ncol=2)
testmata_CM <- makeCacheMatrix(testmata)
testmatb <- matrix(7:15, nrow=3, ncol=3)
testmatb_Cm <- makeCacheMatrix(testmatb)

testmata_CM$getinverse() # Test if an inverse is already there
cacheSolve(testmata_CM) # calc of the inverse or loading from cache

testmatb_CM$getinverse() # Test if an inverse is already there
cacheSolve(testmatb_CM) # calc of the inverse or loading from cache

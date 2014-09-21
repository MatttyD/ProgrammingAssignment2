## Assignment 2: Lexical Scoping
## Cacrhe Calculation of Matrix Inversion

## Creation of referable  vector based upon Matrix vector input.

makeCacheMatrix <- function(x = matrix()) {
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

## Creation of inversion calculation function with caching checking procedures 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # Checks for pre-calculated results 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...) #Inversion calculation
  x$setinverse(m)
  m
}



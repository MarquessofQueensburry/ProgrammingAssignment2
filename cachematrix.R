## Following along the lines of the example shown here we will create two functions
## That will allow us to creat a matix that can cache its inverse, and another one
## That can retrive that cache if the inverse had already been calculated.


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(z) {
        x <<- z
        inv <<- NULL
  }
     get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This is the retrieving function, if there is cached data it will retrive it
## Instead of computing it.


cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
       message("getting cached data")
        return(inv)
  }
     mtz <- x$get()
     inv <- solve(mtz, ...)
     x$setInverse(inv)
      inv
}
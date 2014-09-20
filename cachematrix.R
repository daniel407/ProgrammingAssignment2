## The function make CacheMatrix allows to store a matrix object as well as its inverse. The function cacheSolve determines whether the inverse has been computed already and if not, will compute and store the value


## Calling the function will return a list of objects (functions) which allow to interact with the stored matrix and its inverse. The functions are set and get to set and restore the matrix as well as setInverse and getInverse to set and restore the inverse. If set is called, the value of the stored inverse is set to NULL. 

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x<<-y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calling the function will return the inverse of a given matrix. The argument required by cacheSolve is the list of functions returned by makeCacheMatrix. If the inverse has already been computed the value is being looked up and returned. If the inverse needs to be computed, the method will do so and store the inverse for future queries.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverse <- x$getInverse()
       if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
       }
       data <- x$get()
       inverse <- solve(data)
       x$setInverse(inverse)
       inverse
}

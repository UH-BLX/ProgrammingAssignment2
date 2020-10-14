makeCacheMatrix <- function(x = matrix()) {
                                                        ##  x must be a square invertible matrix
                                                        ##  return: a list containing functions to set and get the matrix and to 
                                                        ##          set and get the inverse. List is used as an input to cacheSolve()
  
  inv <- NULL
  set <- function(y) {
                                                        ## use of "<<-" to assign an object in an environment different from the current one
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


cacheSolve <- function(x, ...) {
                                                         ## x is the output of makeCacheMatrix()
                                                         ## return: inverse of the original matrix input of makeCacheMatrix()
inv <- x$getinv()
                                                         ##if the inverse has been already calculated:
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
                                                         ## if not..
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

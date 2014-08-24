#The makeCacheMatrix function returns a special matrix object (a list) with subfunctions that allow the matrix to
#cache itself.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get<- function() x
  
  set_inv <- function(inverse) m <<- inverse
  
  get_inv <- function() m
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

#The cacheSolve function accepts a special matrix as input (the same format as outputted by the makeCacheMatrix), 
#determines if the inversion calculation has been performed.  If it has been performed, the cache is returned.
#If not, the program performs the calculation, returns the result and caches the result.

cacheSolve<- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}






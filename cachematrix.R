# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

#Like the vector function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the solve(inverse of a matrix)
# get the value of the solve(inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculates the solve of the special "matrix" created with the above function. 
# However, it first checks to see if the solve  has already been calculated.
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the solve of the data and sets the value of the solve
# in the cache via the setsolve function.
# note: solve -> (inverse of a matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


#matrix TEST
matx<-matrix(c(3,2,4,
               5,2,3,
               4,9,7),nrow=3,ncol=3)
matx

matxmake<-makeCacheMatrix(matx)
cacheSolve(matxmake)

cacheSolve(matxmake)


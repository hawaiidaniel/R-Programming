# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set() function to make the matrix passed in as the working matrix (x) 
  set <- function(y) {
    x <<- y
    #assign m to be NULL
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  #make a list of set, get, setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#The cachSolve function returns the inverse of the matrix. First of all, it checks if the inverse is computed.
#If so, it shows the message "getting cached data" and skip the computation. If not, it computes the inverse and
#returns the value.
cacheSolve <- function(x, ...) {
 ## Return a matrix that inverse 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



# Function makeCachematrix creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
    xinverse <- NULL
    set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
    get <- function() x
  
    setinverse <- function(xi) xinverse <<- xi
    getinverse <- function() xinverse
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


# Function cacheSolve calculates the inverse of the special "matrtix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise,it calculates
# the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinverse <- x$getinverse()
    if(!is.null(xinverse)) {
      message("getting cached data")
      return(xinverse)
    }
    data <- x$get()
    xinverse <- solve(data,...)
    x$setinverse(xinverse)
    xinverse
  }

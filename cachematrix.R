## These functions will invert a matrix and store the previous result cached in memory. This allows any code loop
# utilizing (in this case) a matrix inversion to only calculate the inversion once and then reading it from 
# memory rather than having to recalculate it for every iteration. Note that only the most recent
# matrix inversion is stored in this cache, so if multiple inversions are necessary, they must be stored
# in separate objects.

################################
# makeCacheMatrix(x=matrix())
#
# Generate a list of 4 stored functions and associated stored values under names "set", "get", "setinv", "getinv"
# "set": sets the value of x to input and uses <<- so that assignment is made in the enclosing environment
#
# "get": function that outputs the input itself. basically identity function. this stores the value of 
# the input matrix in enclosing environment for use by cacheSolve(x)
# 
# "setinv": see set but assigns output to s in enclosing environment rather than input
#
# "getinv": see get but returns output rather than input. this stores the value of 
# the output matrix if cacheSolve(x) has been executed and is empty otherwise
################################

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinv <- function(solve) s <<- solve
      getinv <- function() s
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

#########
#     cacheSolve(x,...)
#
# Searches for whatever is stored in x$getinv as defined above and assigns it to m. 
# If cacheSolve has never been executed on input, x$getinv() will be empty and then this 
# function will execute the matrix inversion calcs and then store the results in the
# x$getinv(). 
#
# If the cacheSolve() has already been run on the input object, x$getinv() will be non-empty.
# In this case, it pulls that stored value as the output, saving some unnecessary calculations and
# then the function terminates since return() is called in the if statement. 
#########
cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}

# The following code will test that this works, notice that the second print gives a message that
# it was pulled from cached data

x <- matrix(c(1,2,3,4), nrow=2)
y <- makeCacheMatrix(x)
print(solve(x)==cacheSolve(y))
print(cacheSolve(y))


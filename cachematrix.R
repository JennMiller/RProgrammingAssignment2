## The following two functions collectively create a special object that stores a matrix and caches its inverse so that it can be retrieved more quickly after it is first calculated.


## makeCacheMatrix is a function that takes a matrix argument. The function creates a special "matrix", which is really a list containing a function to:
#-set the value of the matrix
#-get the value of the matrix
#-set the value of the inverse
#-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}  


## Testing

# testMatrix = matrix(c(1,2,3,4),nrow=2,ncol=2)
# cacheSolve(makeCacheMatrix(testMatrix))

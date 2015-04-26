##these functions allow you to calculate, store (cache), and retreive from cache the inverse of a matrix

## makeCacheMatrix allows you to store and retrieve the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## cacheSolve allows you to first check to see if the inverse of a matrix has been cached, and then if not, calculate it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
  
}

## tested using the following code from the R Programming discussion forum (https://class.coursera.org/rprog-013/forum/thread?thread_id=127)

# >m <-  matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

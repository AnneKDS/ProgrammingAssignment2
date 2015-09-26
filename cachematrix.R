## Below are two functions that are used to create a 
## special object that stores a special matirx object
## and caches its mean

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <-- NULL
  }
  get <- function() x
  setinverse <- function (solve) m <<- solve
  getinverse <- function () m
  list(set=set, get=get, setinverse = setinverse, 
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix"
## created by makeCacheMatrix.  If the inverse has already been 
## calculated and the matrix hasn't changed, then
## cacheSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,,...)
  x$setinverse(m)
  m
}

## Below are two functions that are used to create a special object 
##that stores a Matrix, cache's its inverse and retrieves it.

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInver <- function(inverse) inver <<- inverse
  getInver <- function() inver
  list(set = set,
       get = get,
       setInver = setInver,
       getInver = getInver)
}

# The following function returns the inverse of the matrix
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise, 
# it calculates the inverse of the matrix and sets the value in the cache via the setInver function.

cacheSolve <- function(x, ...) {
  inver <- x$getInver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInver(inver)
  inver
}

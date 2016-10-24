## First function sets and gets the value of a matrix and it's inverse
## Second function computes the inverse in case it was not cahed earlier and retrieves it in case it was computed before

## Take matrix and create a list 

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinve <- function(inverse) inve <<- inverse
  getinve <- function() inve
  list(set = set, get = get,
       setinve = setinve,
       getinve = getinve)
}


## Function checks if inverse has been computed before and returns the inverse if it is chached
## In case it has not been computed before, it computes it using solve() and then displays the output

cacheSolve <- function(x, ...) {
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinve(inve)
  inve
}

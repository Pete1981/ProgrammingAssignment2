makeCacheMatrix <- function(x = matrix()) {
  mcm <- NULL
  set <- function(y) {
    x <<- y
    mcm <<- NULL
  }
  get <- function() x
  setmcm <- function(solve) mcm <<- solve
  getmcm <- function() mcm
  list(set = set, get = get,
       setmcm = setmcm,
       getmcm = getmcm)
}

cacheSolve <- function(x, ...) {
  mcm <- x$getmcm()
  if(!is.null(mcm)) {
    message("getting cached data")
    return(mcm)
  }
  data <- x$get()
  mcm <- solve(data, ...)
  x$setmcm(mcm)
  mcm
}

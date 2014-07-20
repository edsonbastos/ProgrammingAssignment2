# makeCacheMatrix #################################################################################
makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get    <- function()    x
  setinv <- function(inv) i <<- inv
  getinv <- function()    i
  return(
    list(set     = set
         ,get    = get
         ,setinv = setinv
         ,getinv = getinv)
  )
}

# cacheSolve ######################################################################################
cacheSolve <- function(x, ...){
  i <- x$getinv()
  if(length(i)>0){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  if(length(data)==0){
    return(message("data must be set in the cache memory"))
  }
  
  i <- solve(data, ...)
  x$setinv(i)
  return(i)
}

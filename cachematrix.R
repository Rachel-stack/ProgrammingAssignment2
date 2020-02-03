##Assignment Week 3 ##
## The following pair of functions cache the inverse of a matrix

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()){
  
  invers <- NULL
  
  set <- function(y) {
     x <<- y
     invers <<- NULL
  }
  get <- function(){
    x
  } 
  setinverse <- function(new_invers){
    invers <<- new_invers
  }
  getinverse <- function() {
    invers
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...){

    invers <- x$getinverse()
    if(!is.null(invers)) {
      message("getting cached data")
      return(invers)
    }
    data <- x$get()
    invers <- solve(data, ...)
    x$setinverse(invers)
    return(invers)
}

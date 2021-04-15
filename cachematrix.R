## the function makeCacheMatrix role is to handele with matrix x, store inverse matrix and cache it. 
## By calling the set function the inverse matrix is erased and after calculation replace by new  value.

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL                        
  set <- function(y){               
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}                                   
  setInverse <- function(inverse) {inv <<- inverse}   
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## this fuction calculates the inverse of an assumed square matrix or retrieves
## a previously calculated inverse from cache wheen given a list argument.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

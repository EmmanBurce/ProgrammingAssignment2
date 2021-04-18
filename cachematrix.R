## For this assignment there are two functions created: The first is the
## makeCacheMatrix and cacheSolve. These Two functions are used to generate
## an object that can keeps a matrix and caches its inverse or opposite.

## the first function makeCacheMatrix generates a matrix which can then be
## be used to cache the inverse.

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL                       ## Initial inverse is set to NULL            
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}             ## This functions is used to generate a matrix "x"
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}    ## The inverse of the matrix is obtained using this function       
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## The second function is used to generate an inverse of the matrix that was obtained
## through the makeCacheMatrix prior. In case where the inverse has already been 
## computed, it will retrieve the inverse from the generated cache. 

cacheSolve <- function(x, ...){     ## retrieves cache data        
  inv <- x$getInverse()
  if(!is.null(inv)){                ## if statement checks whether inverse is NULL
    message("getting cached data")
    return(inv)                     ## yields the inverse value of the cache
  }
  mat <- x$get()
  inv <- solve(mat, ...)            ## solves for the inverse value
  x$setInverse(inv)                 ## generates a matrix that is teh inverse value of matrix "x"
  inv
}

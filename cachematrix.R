## The following function calculates the inverse of the special "matrix".
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the mean in the cache via the setinverse function.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## set inverse matrix to NULL
  inv<-NULL
  
  ## implement set function as adding the input matrix to the local x
  ## and set inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## implement get function which returns the original matrix x
  get <- function() x
  
  ## set inverse matrix , by using <<- the setinverse will change the value of the inv variable
  ## which could be used as a cache later
  setinverse <- function(inverse) inv <<- inverse
  
  #returns inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function calculates or returns inverse for matrix
## if the special "matrix" class inverse is already exist 
## it will return the value from the cache without calculation
## if it is not calculated already it will do the inverse calculation
## and set the inv variable in the special "matrix" variable

cacheSolve <- function(x, ...) {
  ## get the inverse if thwe special "matrix" variable
  inv <- x$getinverse()
  ## if it is already calculated return the inverse x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##if inverse is not calculated already, first get the matrix
  ## from special "matrix" container by x$get()
  data <- x$get()
  ## calculate inverse matrix by solve()
  inv <- solve(data, ...)
  ## set inverse matrix in special "matrix" variable by x$setinverse(inv)
  x$setinverse(inv)
  
  ## returns inv
  inv
}
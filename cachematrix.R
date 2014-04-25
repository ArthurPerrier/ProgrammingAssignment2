## Put comments here that give an overall description of what your
## functions do

#I install a package where a function inverses matrix
install.packages("MASS")
library(MASS)

#This function creates a special "vector", which is list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
#The following function calculates the inverse of the matrix using the special "vector" 
#created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the iverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
#via the setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data)
  x$setinverse(inv)
  inv
}

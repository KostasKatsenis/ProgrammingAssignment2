## The function makeCacheMatrix, takes a square invertible matrix as an argument 
## The output is a list and the function cacheSolve takes that list and 
## returns the inverse of the matrix. If the inverse of the matrix has already
## been calculated, the cacheSolve function will find it in the cache and return it 
## and not calculate it again.

## The function makeCacheMatrix takes a square invertible matrix as an argument 
## and returns a list with four elements and every element is a function.So, we  
## have four functions : set , get , setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## The set function sets the value of the matrix. 
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## The function get gets the value of the matrix.
  
  get <- function() x
  
  ## The function setinverse sets the value of the inverse of the matrix.
  ## The function doesn't compute anything. It just stores the value.
  
  setinverse <- function(solve) m <<- solve
  
  ## The function getinverse gets the inverse of the matrix. When we call
  ## this function, it just shows us the stored value.
  
  getinverse <- function() m
  
  ## The function list creates an object with four elements, the above four functions.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix, if the inverse has 
## already been calculated , then the cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Actually, argument 'x' is not a matrix, is the list of function we
  ## created calling makeCacheMatrix function
  
  ## First of all, we assign m to be the stored value of the inverse of the 
  ## matrix in makeCacheMatrix function. 
  ## If m isn't NULL then we have already compute it and we return this value.
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  
  ## If m is NULL then we have to compute the inverse of the matrix. So, we 
  ## call solve() function to our matrix and we get the inverse of our matrix.
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  ## After computing the inverse we must store its value to the cache. So, we 
  ## call the setinverse function , which only stores the value.
  
  x$setinverse(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set the inverse of the matrix as null in case it is present
  set <- function(y) { #set a function with y as argument
    x <<- y
    m <<- NULL
  }
  get <- function() x    #get the value of x from the parent environment
  setInv <- function(solve) solve <<- m   # set an input argument from m in the parent environment
  getInv <- function() m    # get back an input argument from m in the parent environment

  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getINV) #assigning all the functions as a list
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv() # call the x object into the getInv function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } # if statement to allow to check if m is NULL, and in case give an object for the following function
  data <- x$get()
  m <- solve(data, ...) # function to obtain the inverse of the matrix
  x$setInv(m) # set the inverse of the matrix
  m  # print the inverse matrix
}

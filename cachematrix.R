#--------------------------------------------------------------------------------

#The Assignment

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


#------------------------------------------------------------------------

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {

  
  inv = NULL
  set = function(y) {
  
  # use `<<-` to assign a value to an object in an environment 
  # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



 # Return a matrix that is the inverse of 'x'




# @x: output of makeCacheMatrix()
# return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {

  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  
  mat.inv = x$get()
  inv = solve(mat.inv, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

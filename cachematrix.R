## cachematrix.R
## Description:
## A set of functions that makes use of caching in order to speed up repeated solving of a matrix
## Usage: Let x be the matrix you want to solve multiple times
## create a cached matrix xc by 
## xc <- makeCacheMatrix(x)
## get the inverse xi by
## xi <- cacheSolve(xc)
## cacheSolve  will only calculate the inverse at the first time, and use the cached result afterwards 


## makeCacheMatrix: returns a list of setter and getter functions for the matrix 

makeCacheMatrix <- function(x = matrix()) {
  i = NULL #inverse not known yet
  set <- function(y) { #set the matrix
    x  <<- y
    i  <<- NULL
  }
  get <-function() x  #retrieve the matrix
  setInverse <- function(inverse) i <<- inverse #set the inverse, provided by an external source
  getInverse <- function() i #return the inverse, if known. Otherwise NULL
  list(set        = set,     #the function returns a list of functions
       get        = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: return the inverse of a matrix  that was  saved in a makeCacheMatrix-Object. 
## Will either calculate the inverse or make use of the already calculated cached inversee

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()  #tries to retrieve the inverse from the object
  if(!is.null(i)) {    #inverse was already calculated 
      message("getting cached inverse of matrix")
      return(i)
  }
  data <- x$get()      #inverse has not been calculated yet
  i <- solve(data)     #so we're doing it here
  x$setInverse(i)      #set the cached inverse in the object x
  i
}

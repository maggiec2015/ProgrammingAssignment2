## makeCacheMatrix: This function creates a special "matrix" object that can cache its  
## inverse.

makeCacheMatrix <- function(m = matrix()) 
{
    i <- NULL
    set <- function(y) 
{
      m <<- y ## sets value outside of current environment
      i <<- NULL ## sets value outside of current environment
    }
    get <- function() m
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv) ## returns a list of functions for use 
}

## `cacheSolve` function computes the inverse of matrices returned by `makeCacheMatrix` 
cacheSolve <- function(m, ...) 
{ 
    i<- m$getInv()
    if(!is.null(i)) ## if the inverse has already been found
{
      message("Retrieving cached data")
      return(i)  ## return the cached data, no calculation required
    }
    ## else calculate the inverse    
    data <- m$get()
    i<- solve(data, ...)
    m$setInv(i)
    i
}

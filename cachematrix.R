## Functions were made like analog of example function in assignment text
## This functions release opportunity to cache inverse of a matrix
## 

## This function return the list containing 4 functions. It should 
## have the caching matrix as argument
## set - function for saving new matrix
## get - return current value of matrix
## setsolve - function for saving inverse of a matrix. This function 
##            should call only from cacheSolve function
## getsolve - return current value of inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
      if (class(x) == "matrix")
      {
            inv_matrix <- NULL
      
      
            set <- function (y)
            {
                  if(class(y) == "matrix")
                  {
                        x <<- y
                        inv_matrix <<- NULL
                  }
                  else message("Not a matrix")
            }
            get <- function() x
            setsolve <- function(inv_m) inv_matrix <<- inv_m
            getsolve <- function() inv_matrix
      
            list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
      }
      else message("Not a matrix") 

}


## Function for caching the inverse matrix
## Get the list from makeCacheMatrix as argument
## Assume that the matrix supplied is always invertible (as in assigment text)

cacheSolve <- function(x) 
{
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getsolve()
      if(!is.null(inv))
      {
            message("Getting cache data")
            return(inv)
      }
      datamatrix <- x$get()
      inv <- solve(datamatrix)
      x$setsolve(inv)
      inv
}

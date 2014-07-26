#Matrix inversion is usually a costly computation and there is some benefit to caching 
#   the inverse of a matrix rather than computing it repeatedly.
#   This R code file contains helper functions for caching the inverse of a matrix.

#makeCacheMatrix creates a special "matrix" object that is capable of caching its inverse.
#   The resulting object has the following methods:
#      set() -- assign a new simple matrix within the object and discards any cached information.
#      get() -- retrieve the simple matrix stored within the object.
makeCacheMatrix <- function(x = matrix()) 
{
   inverse <- NULL
   set <- function(y) 
          {
             x <<- y
             inverse <<- NULL
          }
   get <- function() { return(x) }
   
   #setInverse and getInverse are implementation details that, ideally, 
   #   would not be accessible outside this code module.
   #   I'm storing them in a sub-list named "private" to communicate that
   #   to consumers of this code.
   setInverse <- function(i) { inverse <<- i }
   getInverse <- function() { return(inverse) }
   private <- list(setInverse = setInverse, getInverse = getInverse)
   
   return (list(set = set, get = get, private = private))
}


#cacheSolve computes the inverse of a special matrix object.
#   You can obtain one of these special matrix objects by calling makeCacheMatrix (above). 
#   Once calculated, cacheSolve will thereafter return a cached copy of the matrix's inverse.
cacheSolve <- function(matrixObject, ...) 
{
   x <- matrixObject
   
   #If a simple matrix was supplied, reinterpret it as one of our special matrix objects
   if (is.matrix(matrixObject))
   {
      x <- makeCacheMatrix(matrixObject)
   }
   
   ## Return a matrix that is the inverse of 'x'
   result <- x$private$getInverse()
   if(is.null(result)) 
   {
      result <- solve(x$get(), ...)
      x$private$setInverse(result)
   }
   else
   {
      message("retrieved cached matrix inverse")
   }
   return (result)
}

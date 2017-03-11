## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
 
 makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
 }
 
 
 ## This function computes the inverse of the special "matrix" created by 
 ## makeCacheMatrix above. If the inverse has already been calculated (and the 
 ## matrix has not changed), then it should retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv_mat <- x$getInverse()
     if (!is.null(inv_mat)) {
         message("getting cached data")
         return(inv_mat)
     }
     mat_rix <- x$get()
     inv_mat <- solve(mat_rix, ...)
     x$setInverse(inv_mat)
     inv_mat
 }
*******************************************************************************************  
                             SAMPLE OUTPUT BELOW
*******************************************************************************************       

 my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
 my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
 my_matrix$getInverse()
NULL
 cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 

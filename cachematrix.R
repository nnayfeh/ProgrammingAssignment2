## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and the functions below
## allow the creating and use of a cached inverse matrix.


## Write a short comment describing this function
## this makes a cached matrix from a matrix input as 'x'

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set= set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## this function will invert a square invertible matrix
## if the matrix has been inverted previously, it uses a cached solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m

}

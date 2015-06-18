## The functions in this file will compute the inverse of an invertible matrix
## If this inverse was already computed and cashed, the second function will just get it

## The following function creates a special matrix with a list of function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      mm <- NULL
      set <- function(y){
            x <<- y
            mm <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) mm <<- inv
      getinverse <- function() mm
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
            )
      
}


## This function Calculates the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available
cacheSolve <- function(x, ...) {
      mm <- x$getinverse()
      if(!is.null(mm)) {
            message("getting cached data")
            return(mm)         ## Return a matrix that is the inverse of 'x'
      }
      n <- x$get()
      mm <- solve(n, ...)
      x$setinverse(mm)
      mm

}

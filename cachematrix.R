## A pair of functions that cache the inverse of a matrix

## Create a special "matrix", which is a list containing a function to
##set the value of the matrix,get the value of the matrix
##set the value of the inverse matrix,get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      j <- NULL
      
      set <- function(y) {
                x <<- y
                j <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inv) 
                j <<- inv
      
      getinverse <- function() j
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
         )
}



## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getinverse()
        if(!is.null(j)) {
                 message("getting cached data")
                 return(j)
        }
        m <- x$get()
        j <- solve(m)
        x$setinverse(j)
        j
  
}

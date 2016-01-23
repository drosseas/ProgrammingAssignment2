## Functions that cache the inverse of a matrix
## Create a matrix object that can cache its inverse
  makeCacheMatrix <- function( m = matrix() ) {
    ## Initialize 
      i <- NULL
      
    ## Set the matrix
      set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
      }
        
    ## Get a matrix
      get <- function() {m}
          
    ## Set the inverse of the matrix
      setInverse <- function(inverse) {
        i <<- inverse
      }
            
    ## Method to get the inverse of the matrix
      getInverse <- function() {i}
                
    ## Return a list of the methods
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
  
    
## Compute the inverse of the matrix from "makeCacheMatrix" above. 
##If the inverse has already been calculated & is unchanged, this function
##should retrieve the cached inverse.
    cacheSolve <- function(x, ...) {
      
      ## Return the inverse matrix of x
        m <- x$getInverse()
        
      ## REturn the cached inverse
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        
      ## Retrieve the matrix from our object
        data <- x$get()
          
      ## Calculate the inverse 
        m <- solve(data) %*% data
            
      ## Set the inverse to m
        x$setInverse(m)
            
      ## Return the inverse matrix
        m
    }

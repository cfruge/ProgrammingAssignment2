## These functions allow the inverse of a matrix to be calcuated and stored in a special memory 
## location that allow it to be retrieved by other functions

## This function sets up 'shareable' functions that can be called by other functions due to how they 
## are stored in memory
## These function store/update/retrieve the starting matrix and its inverse at a special storage spot
## in memory
## To use this function, define a square matrix and pass that variable to the function. Store the function
## output in another variable.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }  
    
    get <- function() x
    
    setinv <- function(solve) m <<- solve
        
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function first checks if the inverse of the matrix has been calculated already. If so, it retrieves
## it from memory.  If the matrix inverse has not been calculated it does the calculation and then stores it
## to a special shareabe spot in memory.
## To use this function, pass it the variable storing the ouput of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
 
  data <- x$get()
  
  m <- solve(data)
  
  x$setinv(m)
  m


}

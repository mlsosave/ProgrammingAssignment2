## This function create a matrix Put and calculate the inverse matrix 
## set, get the matrix and set and get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) i <<- inverse
   getInverse <- function() i 
   list(set = set,
	get = get, 
	setInverse = setInverse,
        getInverse = getInverse)
}

## This function checks is the matrix is already cached, 
## If it is on cache its returned it and show a message 
## If not charge the matrix on cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
   if  (!is.null(i)){
       message("Getting cached data ...")
       return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setInverse(i)
   i       
        
}

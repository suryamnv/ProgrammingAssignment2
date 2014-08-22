## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse matrix  
	invMatrixCache <<- x

      ## This flag is set to TRUE if the inverse is cached
      invCachedFlag  <<- FALSE

      ## get() function retrieves the matrix
      get <- function() x

      ## getInverse() function retrieves the cached 
      ## inverse matrix
	getInverse <- function() invMatrixCache

      ## Returns the invCachedFlag
      isInverseCached <- function() invCachedFlag

      ## Set the Inverse Matrix Cache
	setInverse <- function(inv) {
		invMatrixCache <<- inv
		invCachedFlag  <<- TRUE
	}

      ## Set the matrix Object
      set <- function (y) {
		x <<- y
            invCachedFlag  <<- FALSE
      }

      ## Return the list of functions
	list(getInverse = getInverse, 
           setInverse = setInverse,
	     get = get,
           set = set,
           isInverseCached = isInverseCached)

}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      
	## Return a matrix that is the inverse of 'x'

      ## Check if the inverse is cached. 
      ## If yes, get it from cache & return
	if(x$isInverseCached()){
		message("Getting cached data")
		y <- x$getInverse()
		return(y)	
	 }

	## Get the matrix object
      z <- x$get()

	## Calculate the inverse
      invZ <- solve(z)

      ## Cache the inverse
      x$setInverse(invZ)

      ## Return the inverse
      invZ
}

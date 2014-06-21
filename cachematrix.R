## Takes a square matrix and makes it a special object which
##   has four functions. It can return the square matrix. It can
##   alter the square matrix. It can store a calculated value for
##   the matrix's inverse. It can return the stored value of the matrix's
##   inverse.
makeCacheMatrix <- function(x = matrix()) {
    #Default value for inv is null, indicating it is un cached.
    inv <- NULL
    # Lets you change the matrix, and resets the cache.
    set <- function(y)
    {
      x <<- y
      inv <<- NULL
    }
    # Returns the matrix
    get <- function() x
    # Sets the inverse cache to the value given
    setInverse <- function(i) inv <<- i
    # Returns the inverse cache
    getInverse <- function() inv
    
    #Sets the four functions of the object makeCacheMatrix to be callable.
    list(set = set,get = get, setInverse = setInverse, getInverse=getInverse)
}

## Given the input of an object of MakeCacheMatrix, will return the inverse of
##    the square matrix contained. If the inverse has already been calculated
##    by this function, it will not run again but will use the stored value.
##    If the inverse hasn't been calculated, it will calculate it and store
##    the value.
cacheSolve <- function(specialMat, ...) {
      ## Stores the value of the inverse cache.
      inv <- specialMat$getInverse()
      # Tests to see if the inverse has been computed
      if(!is.null(inv)) {
      # Notifys the user it has already been computed
      message("getting cached data")
      # Returns the Matrix's inverse
      return(inv)
      }
      # Gets the matrix you want to find the inverse of
      mat <- specialMat$get()
      # Computed the inverse
      inv <- solve(mat)
      # Stores the computed inverse value in the cache
      specialMat$setInverse(inv)
      # Returns the Matrix's inverse
      inv
}
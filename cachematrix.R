## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # 1. Initialise the matrix 'cmatrix' and assign the value of NULL
  cmatrix <- NULL
  
  # 2. The setMatrix method is defined to set th value of the matrix
  setMatrix <- function(y) {
    x <<- y
    cmatrix <<- NULL
  }
  
  # 3. getMatrix method is defined to get matrix 'x'
  getMatrix <- function() x
  
  # 4. setInveverse method is defined to set the cache for inverse of matrix 'x' which is assiged to 'cmatrix'
  setInverseCache <- function(inverse) cmatrix <<- inverse
  
  # 5. getInverse method is defined to get the cached inverse of matrix 'x' which has been assigned to 'cmatrix'
  getInverseCache <- function() cmatrix
  
  # 6. list() sets the names of the functions to be used outside of the defined function call in global environment
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseCache = setInverseCache, getInverseCache = getInverseCache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # 1. getInverseMatix() gets the cached inverse of matrix 'x' and assigns the contents to 'cmatirx' variable
  cmatrix <- x$getInverseMatrix()
  
  # 2. Check if the inverse if matrix 'x' exists in the cache
  if(!is.null(cmatrix)) {
    message("getting cached matrix which is inverse of matrix 'x'.")
    return(cmatrix)
  }
  
  # 3. if the inverse of matrix 'x is not available in cache get the matrix using getMatrix method
  data<- x$getMatrix()
  cmatrix <- solve(data, ...)
  x$setInverseMatrix(cmatrix)
  cmatrix
}

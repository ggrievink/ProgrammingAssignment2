## 
## The function make store a matrix and its inverse.
## 
makeCacheMatrix <- function(currentMatrix  = matrix()) {
  
  ## the calculated inverse matix
  inverseMatrix <- NULL
  
  ## set the matrix and clear the inverse matrix.
  set <- function(y) {
      currentMatrix <<- y
      inverseMatrix <<- NULL
  }
  
  ## get the matrix
  get <- function() { currentMatrix }
  
  ## get the inverse matrix.
  getInverseMatrix <-  function() { inverseMatrix }
  
  ## set the solved  matrix
  setInverseMatrix <-  function(y) { inverseMatrix <<- y }
  
  ## list with functions.
  list(set = set, 
       get = get,
       getInverseMatrix = getInverseMatrix,
       setInverseMatrix = setInverseMatrix)
}

## Before it calculates the inverse the function checks 
## if the inverse is already pressent in the catch.
cacheSolve <- function(x, ...) {
  
    ## get the current inver matrix
    invMatrix <- x$getInverseMatrix()
    
    ## test if the inverse matrix is NULL.
    ## when NULL the inverse matrix must be solved.
    if(is.null(invMatrix)){
        ## no calculated value pressent, calculate the inverse matrix,
        ## message("calculate inv matrix")
        ## get the matrix and calculate inverse
        invMatrix <- solve( x$get(), ... )
        ## calulate and set inverse.
        x$setInverseMatrix(invMatrix )
    }
    ## get the inverse matrix
    return(invMatrix)
}

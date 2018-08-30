## function to store a matrix that can be inverted
## assume that the matrix is inverable (square)

makeCacheMatrix <- function(x = matrix()) {

     ## set value of inverted matrix to NULL
     imatrix <- NULL
     
     ## define the function to assign the matrix and the inverted matrix to the parent environment
     set <- function(y) {
          x <<- y
          imatrix <<- NULL
     }
     
     ## define the function that will retrieve the input matrix
     get <- function() x
     
     ## define function to assign inverted matrix to parent environment
     setinv <- function(z) imatrix <<- z
     
     ## define function to retrieve inverted matrix
     getinv <- function() imatrix
     
     ## create a list of functions
     list(set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
     
}


## function to retrieve the inverse of the stored matrix

cacheSolve <- function(x, ...) {
     
     ## check if inverted matrix exists in parent environment
     imatrix <- x$getinv()
     
     ## if an inverted matrix exists retrieve it from the parent environment
     if (!is.null(imatrix)) {
          return(imatrix)
     }
     
     ## get the matrix cached in makeCacheMatrix
     data <- x$get()
     
     ## invert the matrix
     imatrix <- solve(data, ...)
     
     ## return the inverse to the parent environment
     x$setinv(imatrix)
     imatrix
     }

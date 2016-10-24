## The followint two functions work in concert:
## makeCacheMatrix function caches value of inversed matrix
## cacheSolve function tries to calculate the inversed of a given matrix

## This function caches value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
       inversedMatrix <- NULL ## initialize inverse
       
       ## set x in parent env with the desired value, 
       ##if inverse is already set, get rid of it!
       set <- function(newMatrix = matrix()) {
              x <<- newMatrix 
              inversedMatrix <<- NULL
       }
       
       get <- function() {
              x
       }
       
       ##set inverse variable in parent env to desired value 
       ##and return the value as a convenience
       setInverse <- function(inversedValue) {
              inversedMatrix <<- inversedValue 
              return(inversedMatrix)
       }
       
       getInverse  <- function() {
              inversedMatrix
       }
       list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The following function will get a matrix as parameter
## Check if invesed matrx has been calculated and cached
## If not invesed matrix is cached, try to calculate the inversed matrix
## If the matrix is invesable, return the inversed matrix,
## Otherwise return null

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) {
       ## Check if the inversed matrix already cached
       inversedMatrix <- x$getInverse() 
       if(!is.null(inversedMatrix) && is.matrix(inversedMatrix)) { 
              message("Cached inversed matrix found.")
              return(inversedMatrix)
       }
       
       ## Otherwise calculate the inversed matrix
       newMatrix <- x$get()  
       inversedMatrix <- tryCatch({ 
              solve(newMatrix)
       }, error=function(e) {
              message("Error in solving matrix")
              message(e)
              message("\n")
       })
       
       ## setting the value of the inversed matrix
       message("Setting the value of the inversed matrix:") 
       x$setInverse(inversedMatrix)
}

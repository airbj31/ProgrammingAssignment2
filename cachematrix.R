## makeCacheMatrix make a list which have 4 elements, named as get, set, getInv and setInv respectively.
## All of the elements are actually functions used to  
##  * set - used to set a cache matrix
##  * get - used to get a cache matrix
##  * setInv - used to set an inverse matrix
##  * getInv - used to get inverse matrix
## the function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(),force=FALSE) {
  
  # check if input is matrix or not 
  if(!is.matrix(x)) { stop("input should be matrix. your input is ",class(x))}
  
  # check if the matrix is square(=invertible) or not.
  # If force is true, extract square matrix with min(dim(x)) x min(dim(x)) 
  if(nrow(x)!=ncol(x) && force==FALSE) {stop("the Matrix is not invertible, If you want to force it, use folce=TRUE");}
  else if(nrow(x) != ncol(x)){ 
    minX<-min(dim(x))    ## get small number between nrow(x) and ncol(x)
    x<-x[1:minX,1:minX]  ## coerce the matrix.
    message("your input matrix was coerced into ",minX,"x",minX," matrix")
  }
  
  iM<-NULL ## declare variable iM for temporary inversed matrix.
  
  ## declare each functions.
  
  set <- function(y) {
    x <<- y      # <<-- used to save the
    iM <<- NULL  # iM is reseted.
  }
  get <- function() x                      ## call input matrix from caChe
  setInv <- function(solve) iM <<- solve   ## set the Inversed matrix into cache
  getInv <- function() iM                  ## call the inversed matrix from cache 
  
  
  ## make list with above 4 functions.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve check if inverse matrix were calculated by checking x$getInv is null.   
## If false, meaning that x$genInv is not null and inverse matrix is already stored, the function returned calculated inverse matrix
## If true, it calculate the inverse matrix by solve function and save it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## getInverse matrix from x and name it as iM.
  iM <- x$getInv()
  
  ## check if the inverse matrix are stored or not.
  if(!is.null(iM)) {
    message("getting cached data")
    return(iM)                              ## if the inverse matrix is not empty, return inverse matrix to console.
  }
  ## store the inversed matrix into cache
  data <- x$get()                           ## get input Matrix.          
  iM <- solve(data, ...)                    ## invert the matrix using solve function. this function are involved in base package.
  message("Saving data into cache")         
  x$setInv(iM)                              ## save the inverse matrix into cache. 
  iM
}

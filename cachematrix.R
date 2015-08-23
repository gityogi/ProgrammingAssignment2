## There are two Function makeCacheMatrix and cahcesoleve. First Function deals with getting and setting value of a matrix
## and its inverse and second function will check if already the inverse is there and and has been set it will just return 
## the cached inverse of the special matrix.
## 

##
##For getting and setting matrix and its inverse
##
## @param matix(should be ivertible)
## @return list of function 
##  set : for setting the matrix
##  get : for getting the matrix
##  setInv : for setting Inverse
##  getInv : for getting Inverse 


makeCacheMatrix <- function(x = matrix()) {
  
  tempInverseMatrix <- NULL
  
  set <- function(y){
    
    #For checking if the passed matrix is similar to the original one.
    
    if(!all.equal(x,y)){
      tempInverseMatrix <- NULL
    }
    x <<- y
  }
  
  get <- function() x
  
  setInv <- function(i) tempInverseMatrix <<- i
  getInv <- function() tempInverseMatrix
  
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
  
}

## 
## Finds the inverse of matrix and return it and if already there just returned the cached data.
##
## @param specail matrix obtained from makeCacheMatrix function
## ...  For passing additional parameter
## @return Inverser of a matrix
##

cacheSolve <- function(x, ...) {


tempInverseMatrix <- x$getInv()

  if(!is.null(tempInverseMatrix))
{
  message("getting the cached inverse of the matrix")
  return(tempInverseMatrix)
}

## Calculate the inverse:
tempInverseMatrix <- solve(x$get(), ...)
  x$setInv(tempInverseMatrix)
  return(tempInverseMatrix)


}

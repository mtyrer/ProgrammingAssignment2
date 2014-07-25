## These functions allow for the creation and manipulation of a matrix object 
## that is capable of caching its inverse.
## Parsing a matrix variable to makeCacheMatrix() will return a list of 
## functions capable of manipulating the embedded matrix and inverse matrix
## variables. cacheSolve() can be used to return the inverse matrix and if 
## needed, caculate and cache the inverse matrix
## Note that these functions assume that the matrix can be inverted.

## list_variable <- makeCacheMatrix(matrix_variable)
##
## This function is a wrapper for a matrix object that allows it to be cached
## The function is invoked with a matrix object as an argument.
## e.g. x <- makeCacheMatrix(matrix_obj)
## 
## This function returns a list, containing functions that allow for the 
## manipulation of the embedded matrix object.
## set(matrix_value) will set the value of the embedded matrix object
## get() will return the embedded matrix object
## setInvMatrix(matrix_value) sets the value of the embedded inverse matrix
## getInvMatrix() returns the value of the inverse matrix
## Note that at creation time, the value of the embedded matrix value is set
## and the value of the inverse_matrix is set to NULL

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the variable 
    invMatrix <- NULL 
    
    # set the value of the passed in matrix to the function environment
    # initialise the value of the inverse matrix in the function environment
    set <- function (matrix) {
        x <<- matrix
        invMatrix <<- NULL
    } 
    
    # Create the setter and getter functions
    get <- function() x
    setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
    getInvMatrix <- function() invMatrix
    
    # Return a list of setter and getter functions
    list(set = set, 
         get = get, 
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix)
}

## matrixObject <- cacheSolve(makeCacheMatrix_listObject)
##
## This function accepts an argument of type makeCacheMatrix. Additional 
## parameters may be supplied, which will be passed onto the solve function.
## 
## The function returns the inverse of the matrix object passed to the funtion.
## If the inverse matrix has already been calculated, the pre-calculated value
## is returned. If the inverse matrix has not been calculated then 
## this function calculates the inverse matrix, saves it within the matrix 
## variable passed into the function and then returns the calculated 
## inverse matrix

cacheSolve <- function(x, ...) {
    
    # get the current value of the the inverse function
    invMatrix <- x$getInvMatrix()
    
    # check if the inverse function has been previously calculated
    if (!is.null(invMatrix)) {
        message("getting cached data")
    } else {
        # get the value of the matrix variable
        data <- x$get()
        # solve to get the inverse matrix
        invMatrix <- solve(data, ...)
        # cache the result
        x$setInvMatrix(invMatrix)
    }
    
    # return the inverse matrix
    invMatrix
    ## Return a matrix that is the inverse of 'x'
}

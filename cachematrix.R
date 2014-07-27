#' Matrix Utilities
#' 
#' The following functions are utilities that help improve the 
#' performance of the application when constantly manipulating
#' or performing matrix common operations. 
#' 
#' @author lcorrales
#' @seealso https://github.com/lcorrales/ProgrammingAssignment2




#' makeCacheMatrix
#' 
#' This function creates a special 'matrix' to which you can
#' interact with by using the methods exported. For this 
#' particular implementation the matrix must always be square
#' 
#' @param x matrix that will be used. Must be square 
#' @return list that contains a reference to the exported methods:
#'              get : returns the current matrix
#'              set : sets the current matrix
#'              getInverse : returns the inverse of the current matrix
#'              setInverse : sets the inverse of the current matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    #' Returns the current matrix 
    get <- function() { x }
    
    #' Sets the current matrix
    set <- function(newMatrix) {
        x <- newMatrix
        inverseMatrix <- NULL
    }
    
    #' Returns the inverse of the current matrix
    getInverse <- function() { inverseMatrix }
    
    #' Set the inverse of the current matrix
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    # Exports the functions above
    list(
        get = get,
        set = set,
        getInverse = getInverse,
        setInverse = setInverse
        )
}


#' cacheSolve
#' 
#' This function calculates the inverse of a special matrix
#' created using the function makeCacheMatrix. If the inverse
#' has been previously calculated (and the matrix remains the same)
#' then the result will retrieved the cached inverse for that matrix.
#' 
#' @param x a special matrix created with makeCacheMatrix
#' @return the inverse of the matrix.\
cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(is.null(inverse)) {
        #' if the inverse is null then calculate it and cache it
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        
    } else {
        #' if the inverse was previously calculated then print a
        #' message and return the cached value. 
        message ('retrieving cached value')
    }
    
    inverse
}

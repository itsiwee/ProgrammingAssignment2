##These functions (makeCacheMatrix and cacheSolve) allow users to cache the inverse of a square matrix in order to save computational time when the matrix's inverse may need to be calclulated/used multiple times during the session. 

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.  The special "matrix" object is a list of four functions that will: set the value of the matrix; get the value of the matrix; set the value of the matrix's inverse; and get the value of the matrix's inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## cacheSolve is a function that calculates the inverse of the special "matrix" created with the above function, makeCacheMatrix. It will first check to see whether the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix, using R's built-in solve() function, and then sets the value of the inverse in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}

## [The below Functions Calculate the Inverse of the Matrix if the inverse of the matrix is already Cached we get the Inverse 
##  of the matrix from Cache else calcuates the Inverse of the Matrix ]

makeCacheMatrix <- function(x = matrix()) {
        ##  x accepts marix as input
        ## This function creates a special "matrix" object--
        ##  that can cache its inverse. 
        m <- NULL
        ## Set the Function 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##Get the value of the Matrix
        get <- function() x
        
        ## Set the value of the inverse matrix 
        setmatrix <- function(solve) m <<- solve
        ##Get the  Matrix
        getmatrix <- function() m
        
        ## Op as a list a vector with 
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Accepts the matrix which were set by makeCacheMatrix and 
        ## calculate the Inverse of the Matrix
        
        ## Get the Matrix 
        m <- x$getmatrix()
        
        ##Check if the inverse of matrix is cached
        if(!is.null(m)) {
                message("Getting Inverse from Cached data")
                return(m)  ##returns the cached daat
        }
        ##get the matrix data
        data <- x$get()
        ##calculate the inverse of the matrix
        m <- solve(data, ...)
        ##set the matrix
        x$setmatrix(m)
        ## Return the Inverse of the matrix
        m
}

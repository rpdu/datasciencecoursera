## Coursera - R Programming - ProgrammingAssignment 2
##
## This script produces a pair of functions that take
## as input a square matrix and produces the inverse matrix.
## This is done through functions that set the solved matrix into memory
## and verifies, when rerun, if it is resident in memory before reprocessing. If the solved
## matrix already in memory, after having been previously processed, then it calls its value
## from memory, instead of reprocessing it.


## The makeCacheMatrix function is similar to the makeVector function shown course examples.
## The user will provide a square matrix object, such as matrix(6:9, nrow = 2, ncol = 2) 
## or matrix(1:4, nrow = 2, ncol = 2) as input for the MakeCacheMatrix function
## see example below:
##
## > A <- matrix(1:4, nrow = 2, ncol = 2)
## > A
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > My_ListA <- makeCacheMatrix(A)
## 
## The makeCacheMatrix function will produce and load into memory a list of functions that will  
## sevre as input for the cacheSolve function. These functions are set(), get(), setInverse() & getInverse(). 
##      - Set() sets initial variables in parent environment. 
##      - Get() pulls initial variables in parent environment. 
##      - setInverse() process matrix variable with solve function
##      - getInverse() retrieves processed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) I <<- solve
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function takes as input the list produced by the makeCacheMatrix function 
## and calls getInverse() to verifry if the inverse matrix is already populated or is NULL.
## If not NULL, then it returns the already populated inverse matrix value. Otherwise, 
## the function populates a variable using the get() subfunction of makeCacheMatrix with a matrix 
## and processes it in solve function. The resulting inverse matrix is passed to the setInverse()
## subfunction of makeCacheMatrix and returned to the console. 
## As shown in example below:
## > cacheSolve(My_ListA)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
## > cacheSolve(My_ListA)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}

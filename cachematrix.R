## I have used the example for assignment 2 as the basis for these functions.
## I have put the description of what the functions are doing in the function itself.
## This was done to aid in my understanding in what each part of the function was for.
##

## makeCacheMatrix takes a square matrix and stores the inverse

makeCacheMatrix <- function(x = matrix()) {
        ##This nulls out the m variable for use in this function
        m <- NULL
        ## Allows for a new matrix to be used in this instance
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## This gets the current matrix values
        get <- function() x
        ## This is called from cacheSolve and is used to cache the inverse matrix
        setInvMatrix <- function(solution) m <<- solution
        ## This is called by CacheSolve and is used to check for a previously cached version
        getInvMatrix <- function() m
        ## This gives us a list of our functions
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## cacheSolve takes the matrix from the instance of makeCacheMatrix 
## performs the calculation to get the inverse of the matrix, if the 
## inverse matrix is not already cached. If the inverse matrix is not cached, it then
## uses setInvMatrix to cache the inverse matrix in that instance.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## This calls makeCacheMatrix and assigns the inverse matrix to variable m if it exists otherwise it is null
        m <- x$getInvMatrix()
        ## If m is not null it returns the cached value.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##Other wise we get our matrix data from makeCacheMatrix and assign to data
        data <- x$get()
        ##This calculates the inverse matrix 
        m <- solve(data, ...)
        ##This stores the solution in the instance of the makeCacheMatrix
        x$setInvMatrix(m)
        m
}

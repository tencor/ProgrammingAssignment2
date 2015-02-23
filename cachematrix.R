# This function create a new structure data allows to store a matrix and its inverse in a structure that can be used as an object with 4 methods
# set: it allows to store the original matrix value
# get allows to recover the original matrix value
# setsolve: allows to store the inverse matrix
# getsolve: allows to recover the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
# This function calculate the inverse of a matrix if it hasn't been calculated previously
# I receives the special 'matrix' and it test if the inverse of that 'matrix' exists, 
# it returns the cached inverse, if it doen't exists it solve the matrix and store
#inthe special matrix calling the method setsolve

cacheSolve <- function(x, ...) {
        #We recover the inverse if exists
        m <- x$getsolve()
        #If exists the inverse we return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if doesn't exists we recover the original matrix
        data <- x$get()
        #We assign the inverse of original matrix to and intermediate variable
        m <- solve(data)
        #We store the inverse in the 'special matrix'
        x$setsolve(m)
        m
}
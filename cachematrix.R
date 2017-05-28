## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix and cacheSolve functions are used to create objects that are used
## to verify if the inverse of a matrix calculation has already been stored in cache or not.
## These functions allows a mechanism to store objects in memory and allow for quick retrieval without re-running computations on the same input.

## Write a short comment describing this function
## First, this function initializes variables x and m to NULL.
## Then, the set and get functions are defined to set and get the data values for the objects. 
## Within the set function, assign the input argument to the x object in the parent environment.
## Within the set function, assign the value of NULL to the m object in the parent environment.
## Within the get function, retrieve the argument x from the parent environment.
## The set_inverse function assigns the inverse variable for the parent environment.
## The get_inverse function retrieves the inverse variable.
## Finally, a list is returned as the object type to be used further.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## Take a matrix argument x and additional arguments.
## Use variable m to retrieve the inverse of matrix of argument x.
## Then check to see if the variable is NULL.  If it is not NULL, then we already have a cached inverse
## and can return that variable to the parent environment.
## If it is NULL, then retrieve the vector from the input object and calculate the inverse via the set_inverse function.
## Return the matrix inverse object to the parent environment.
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set_inverse(m)
        m 
}

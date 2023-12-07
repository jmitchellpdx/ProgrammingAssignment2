# cachematrix.R
# This file contains two functions that allow efficient retrieval of a
# large inverse matrix by caching it.
# makeCacheMatrix takes an invertible matrix as argument and stores it.
# It returns an object containing list of functions that can be used to access 
# the matrix and its inverse
# Passing this object to cacheSolve will call the solve
# function the first time and store the inverse. 
# Subsequent calls will return the cached inverse.
# 
# Usage Example:
# 
# > my_matrix <- matrix(rnorm(100),10,10)
# > my_cache <-  makeCacheMatrix(my_matrix)
# ### The first call to cacheSolve will cause the inverse matrix to be computed
# > my_inverse <- cacheSolve(my_cache)
# ### The second call will return the cached version, and let the user know!
# > my_inverse <- cacheSolve(my_cache)
#
#
# 
# 
# store where the matrix is and return a list of functions to access it
# and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Use "super assignment" to access the matrix and its inverse from the 
    # parent environment
    set_matrix <- function(y){
        x <<- y
        inv <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(the_inverse) inv <<- the_inverse
    get_inverse <- function() inv
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

#  Takes in an object x constructed by makeCacheMatrix that contains a matrix. 
#  Return the cached inverse matrix if it has been calcalated before.
#  If not,  compute the inverse, store it so the cached version will be 
#  used in future calls, and return the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    mat <- x$get_matrix()
    inv <- solve(mat, ...)
    x$set_inverse(inv)
    inv
}


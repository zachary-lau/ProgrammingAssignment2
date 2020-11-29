# The functions below create a self-defined matrix object
# and cache its inverse, so that when the inverse of a same matrix is
# going to be recomputed, its inverse can be looked up in the 
# cache rather than recomputed

# This function creates a self-defined matrix object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x
    }
    set_inverse <- function(i){
        inv <<- i
    }
    get_inverse <- function(){
        inv
    }
    
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# This function computes the inverse of the self-defined matrix object.
# If the inverse has already been calculated, i.e., found in the cache,
# then this function will retrieve the inverse from cache rather than recomputed
# the inverse again.

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$set_inverse(inv)
    
    inv
}

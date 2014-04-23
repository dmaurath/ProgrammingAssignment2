## makeCacheMatrix
## Creates a special Matrix which is a list containing functions to set and get the values of the matrix, and to set and get the value of the matrix's inverse.

##cacheSolve
## Checks for a matrix's inverse in cache. If not present, it calculates the matrix and adds it to the cache.


## Initiaizes the inverse of the new matrix to NULL since its not yet cached. 
## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of the inverse of the matrix
## Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) Inverse <<- solve
        getInverse <- function() Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##Retrieves the inverse from the makeCacheMatrix function
##Checks if the inverse if NULL (i.e. if its not yet cached)
##If NOT NULL, it retrieves the inverse from cache and stops.
##IF it is NULL, it calculates the inverse and returns it. 

cacheSolve <- function(x, ...) {
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}

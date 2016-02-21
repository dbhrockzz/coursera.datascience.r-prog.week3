## Matrix inversion can prove to be a highly resource-intensive task. To avoid the computation everytime,
## the data can be stored in cache. This way, when the function is called next time, it will
## first look int the cache whether there is any previously computed answer or not.
## If yes, the answer is directly picked up form cache without any computation. If the answer isn't there,
## the answer has to be computed. This can save precious computational power in many scenarios.

## makeCacheMatrix(x) makes a special vector which contains functions to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(x, ...) calculates the inverse of the special vector created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

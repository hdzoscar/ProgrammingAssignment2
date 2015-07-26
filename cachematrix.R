## The next functions allow to store the inverse of a matrix if it has not been
## calculated before, if so, they will cache the results so no new computation
## of the matrix inverse is needed and a faster result is obtained. 

## The makeCacheMatrix function creates an special matrix that will store and return data
## which results from obtaining the inverse of a matrix. Other functions are defined
## to perform read and write actions into the special matrix space.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will evaluate and decide if it needs to recalculate a matrix inverse
## or just return a message indicating that the matrix inverse was calculated previously and
## no more processing over the original matrix is not needed because the results of that process
## are stored in a cache area.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
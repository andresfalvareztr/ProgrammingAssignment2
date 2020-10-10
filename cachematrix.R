## Return the inverse of a matrix. 
## e.g1 -> cacheSolve(makeCacheMatrix(my_matrix))
## e.g2 -> instance1 <- makeCacheMatrix(my_matrix) >> cacheSolve(instance1)


## makeCachematrix() will return a list of functions (set, get, setinverse, getinverse), 
## any instance contains a copy of the environment of makeCachematrix() as well as objects defined within. 

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) { 
        x <<- y 
        inverse_matrix <<- NULL 
    }
    get <- function() x
    setinverse <- function(solve) inverse_matrix <<- solve
    getinverse <- function() inverse_matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() will calculate, store (if apply) and return the inverse of a matrix, 
## (w/o taking into account if the matrix is inversible). 

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getinverse()
    
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}

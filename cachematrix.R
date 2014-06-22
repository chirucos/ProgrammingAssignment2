## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
				x <<- as.matrix(y)
				inv <- NULL
        }
        get <- function() x
        setinv <- function(y) inv <<- y 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}


mat1 <- makeCacheMatrix(matrix(c(1,3,2,4), nrow=2, ncol=2))
mat2 <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3))

cacheSolve(mat1)
cacheSolve(mat1)
cacheSolve(mat2)
cacheSolve(mat2)
cacheSolve(mat2)

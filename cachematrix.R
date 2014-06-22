# function for creating a matrix along with some additional functions: setters & getters for the matrix and for its inverse
makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL 				# this is where I will save the inverted matrix
        set <- function(y) {
			x <<- as.matrix(y) 	# set the matrix to be the one received as input
			inv <- NULL		# and the inverted matrix to be NULL (initialization)
        }
        get <- function() x			# return the initial matrix, so that I compute the inverse if not already cached
        setinv <- function(y) inv <<- y   	# set the inverse matrix to be the one received as input
        getinv <- function() inv  		# get the inverted matrix 
        list(set = set, get = get, setinv = setinv,  getinv = getinv) # return the matrix along with our defined functions
}


# function for computing the inverse of a function if this inverse has not been previously cached, in which case we retrieve it
cacheSolve <- function(x, ...) {
        inv <- x$getinv()	# try fetching the inverted matrix from cache
        if(!is.null(inv)) {	# we have it in the cache, so we don't need to compute it 
                message("getting cached data") 	# so we send the message, 
                return(inv)	# and retrieve it from the cache and close the function by return
        }
	# if we got here it means that the inverted matrix was not found in cache so we need to compute it
        data <- x$get() 	# get the initial matrix
        inv <- solve(data, ...) # compute the inverse
        x$setinv(inv)		# save it in the cache, so if we need it again, we won't have to recompute it
        inv			# return the inverted matrix
}


# next we include 2 tests in order to verify if our work is good
mat1 <- makeCacheMatrix(matrix(c(1,3,2,4), nrow=2, ncol=2)) # build a simple 2*2 matrix
mat2 <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3)) # build a 3*3 matrix

cacheSolve(mat1) # here it computes the inverse and caches it
cacheSolve(mat1) # here it retrieves it from cache
cacheSolve(mat2) # here it computes the inverse and caches it
cacheSolve(mat2) # here it retrieves it from cache
cacheSolve(mat2) # here it retrieves it from cache

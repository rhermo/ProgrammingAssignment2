## functions do
# creates an object to cache a matrix
#       arg: x, matrix to inverse
#       set: it sets the matrix to inverse
#       get: it returns the matrix to inverse
#       setinverse: arg: inverse matrix, it sets the inverse matrix
#       getinverse: it returns inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_matrix <<- inverse
        getinverse <- function() inv_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
} 



cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        # x is a makeCacheMatrix object, previously created
        inverse_matrix <- x$getinverse()
        # testing if the inverse matrix has been created (is not null)
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        # cached data are null, so it has to be solved and cached
        data <- x$get()
        # calculating the inverse
        inverse_matrix <- solve(data, ...)
        # setting the result to makeCacheMatrix
        x$setinverse(inverse_matrix)
        return(inverse_matrix)
} 

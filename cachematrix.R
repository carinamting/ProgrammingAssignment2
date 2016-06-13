## makeCacheMatrix(<matrix>): cache a large matrix and its inverse to save time 
## input argument is matrix (assume square)
## Output is list of functions: set - cache matrix
##                                   get - get original matrix
##                                   setinv - cache inverse of matrix
##                                   getinv - get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(myinverse) inv <<-myinverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(<makeCacheMarix list>)Will calculate inverse matrix for cached matrix (see makeCacheMatrix output)
## input the makecacheMatrix list structure ouput
## output inverse matrix either pulled from makeCacheMatrix output list or calculated and placed in input list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        message("solving and caching data")
        myMatrix<-x$get()
        inv<-solve(myMatrix,...)
        x$setinverse(inv)
        inv
}
.

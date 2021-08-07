## Put comments here that give an overall description of what your
## functions do

## This function caches the the operation which we dictate 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes/solves the computation which we cached and want, 
## in this case, to get the inverse of our matrix which we inputted  

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinverse(inv)
        inv
}

#testing the code
#p<-matrix(1:4, 2, 2)
#p
#pmatrix<-makeCacheMatrix(p)
#pmatrix
#pmatrix$get()
#cacheSolve(pmatrix)
#pmatrix$getinverse()


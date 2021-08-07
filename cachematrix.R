## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

a<-c(3,2,5)
b<-c(2,3,2)
c<-c(5,2,4)
a
b
c
x<-rbind(a, b, c)


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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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



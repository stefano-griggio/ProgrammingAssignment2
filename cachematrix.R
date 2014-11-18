## These function can store the value of the matrix  and its inverse



makeCacheMatrix <- function(x = matrix()) {
                invm <- NULL
                set <- function(y) {
                        x <<- y
                        invm <<- NULL
                }
                get <- function() x
                setinvm<- function(i) invm <<- i
                getinvm <- function() invm
                list(set = set, get = get,
                     setinvm = setinvm,
                     getinvm = getinvm)
        }



## These function read the value of the matrix, if it's null than calculates the inverse, else return the
## stored value of the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinvm()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinvm(inverse)
        inverse
}

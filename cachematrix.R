## Assignment: Catching the inverse of the matrix

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL   #im: inverse matrix                    
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        set_im <- function(solve) im <<- solve
        get_im <- function() im
        list(set = set, get = get,
             set_im = set_im,
             get_im = get_im)
}


## catchSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        im <- x$get_im()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$set_im(im)
        im
}
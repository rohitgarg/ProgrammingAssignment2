
## makeCacheMatrix function 
##Creates a special "matrix" object that can cache its inverse.
##It has 4 functions : setting the matrix object, retuning the matrix, setting the inverse matrix object and returning the inverse matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) c <<- inv
        getinverse <- function() c
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Cachesolve function
## It fisrst checks if the matrix inverse already exists in the cache
## If it doesn't exist, the function calculates inverse of matrix, stores it in inverse matrix object by called serinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	c <- x$getinverse()
        if(!is.null(c)) {
		
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data)
        x$setinverse(c)
        c
}

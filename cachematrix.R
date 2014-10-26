## Programming Assignment 2 for Coursera's "R Programming" Course

## This function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y		## new value set
			i <<- NULL	## inverse set to NULL
	}
	get <- function() x	## get the matrix value 
	setinverse <- function(inverse) i <<- inverse	## set the inverse value
	getinverse <- function() i ## get the inverse value
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Function to calculate the inverse of the matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse() ## gets the invers of the 'x' taken from the calling environment
        if(!is.null(i)) {	## if the inverse is already calculated 
                message("getting cached data")
                return(i) 
        }
        data <- x$get()		## if it is not calculated, we do it
        i <- solve(data, ...)
        x$setinverse(i)		## set the new inverse value
        i
}

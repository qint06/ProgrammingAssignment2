## Put comments here that give an overall description of what your
## functions do
## Functions below would instantiate a matrix, and then calculate its inverse
## If inverse is already existed, the inverse is saved is Cache and could be 
## used without computing. If matrix is changed or no inverse available, inverse
## is calculated and saved in Cache for further use. 

## Write a short comment describing this function
## Function makeCachMatrix is used to instantiate matrix, and return a list containing
## four functions to operate with matrix.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	list(set = set, get = get,
		setInverse = setInverse,
            getInverse = getInverse) 
}


## Write a short comment describing this function
## Function cacheSolve is used to return inverse from Cache if available. Or
## to calculate the inverse and save it for further use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		if(!is.null(inverse)){
				message('getting cached data')
				return(inverse)
		}
		data <- x$get()
		inverse <- solve(data, ...)
		x$setInverse(inverse)
		inverse
}

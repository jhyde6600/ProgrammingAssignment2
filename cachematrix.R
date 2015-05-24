makeCacheMatrix <- function(x = matrix()) {
        ## @x is an invertible matrix
        ## return: a list containing functions to
        ## set or get the matrix
        ## set or get the inverse
        ## use the returned info for cacheSolve()
        
        inv = NULL
        set = function(y) {
        
	# use `<<-` to assign a value to an object in an environment 
        # different from the current one. 
                x <<- y
                inv <<- NULL

        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## @x is the output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
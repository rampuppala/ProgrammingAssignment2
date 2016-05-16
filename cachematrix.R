## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
 
        inv = NULL

	  ## function to set the matrix
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
	  ## function to get the matrix
        get = function() x

	  ## function to set the inverse of matrix
        setinverse = function(inverse) inv <<- inverse 

	  ## function to get the inverse of matrix
        getinverse = function() inv

	  ## list to be used as the input to cacheSolve()
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        ## output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }


        # if not, calculates the inverse 

        message("no cached data found, calculating the inverse ")

        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inverse)
        
        return(inverse)
}

## function to test the logic
test = function(mat){
        
        temp = makeCacheMatrix(mat)
        
	  ## first call to initiate the cache
        starttime = Sys.time()
        cacheSolve(temp)
        elapsedtime = Sys.time() - starttime
        print(elapsedtime)
        
	 ## second call to test if the caching the inverse matrix is working
        starttime  = Sys.time()
        cacheSolve(temp)
        elapsedtime = Sys.time() - starttime 
        print(elapsedtime)
}
## Programming Assignment 2: Lexical Scoping
## this Programming Assignment will take advantage of the scoping rules of the R
##language and how they can be manipulated to preserve state inside of an R object

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
  
        # create the matrix        
        
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        getmatrix <- function() x
        
        #perform the matrix inversion using the solve function
        
        invert <- function(x){
                
                inv <<- solve(x)
        }
        
        getinverse <- function() inv

}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then the
# it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$invert()
        
        ## checking if inverse already exists in cache
        
        if(!is.null(inv)) {
                message("inverse already exists. Getting cached data")
                return(inv)
        }
        
        ## computing inverse when inverse is not in cache
        
        inv<<- solve(x)
        
}

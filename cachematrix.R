#The following pair of functions  caches the inverse of a matrix.
#The first function "makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
#The second function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
#These functions demonstrates R's lexical scoping feature by making use of the "<<- super assignment" operator.

#The first function, makeCacheMatrix creates a list that contains four functions that 
#set the value of the Matrix
#get the value of the Matrix
#set the value of the Inverse Matrix
#get the value of the Inverse Matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  
        setInv <- function(Inv) m <<-  Inv 
        getInv <- function() m  
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


#The following function calculates the inverse of the Matrix created with the above function.
#However,it first checks to see if the inverse has already been calculated. If so, it gets the 
#inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the
#data and sets the value of the #inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {     # check for cache
                message("getting cached Inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # compute inverse
        x$setInv(m)           # cache inverse
        m
}

# testing
# testmatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) #create new matrix
# testmatrix$get() 
# cacheSolve(testmatrix) # check if inverse is created
# testmatrix$getInv()
# cacheSolve(testmatrix) # check to see caching is working
# testmatrix = makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2)) #create  new matrix
# testmatrix$get()
# cacheSolve(testmatrix) # check to see if the cache is ignored

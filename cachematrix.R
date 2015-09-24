## "makeCacheMatrix" function creates a list that contains following functions
### 1.  set the value of the matrix
### 2.  get the value of the matrix
### 3.  set the inverse of the matrix
### 4.  get the inverse of the matrix

## Assumption is that the matrix that is input is inversable. 

makeCacheMatrix <- function(x = matrix()) {
        #create a variable m and set it to NULL in the local environment. 
        #this will store cached inverse matrix in the future.
        m <- NULL
        
        #set takes input matrix y and sets it to matrix x in containment environment. 
        #Further sets m to NULL In containment environment. 
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        
        #get returns matrix x.   
        get <- function() x
        
        #get the inverse of matrix and set it to m in containment environment. 
        setinv <- function(inverse) m <<- inverse
        
        #Return matrix m. 
        getinv <- function () m
        
        #create a generic list that contains different elements set, get, setinv, getinv. 
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)

}

## CacheSolve function computes inverse of the matrix provided inverse was NULL
## if inverse is not NULL, then it will return what was stored (m) in local/containment environment. 

cacheSolve <- function(x, ...) {
        
        # Return m which is inverse of matrix. It could be NULL or cached value. 
        m <- x$getinv()
        
        # if m is not NULL then return m with a message. 
        if(!is.null(m)){
                message("getting cached data")
                return (m)
        }
        
        #if m is NULL then first get matrix using x$get() 
        mat<- x$get()
        #and get the inverse of the above matrix using solve function. 
        m <-solve(mat, ...)
        #set the above inverse matrix to m in the containment environment using x$setinv()
        x$setinv(m)
}

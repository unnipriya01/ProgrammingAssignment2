## Programming Assignment-2
## This contains a pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
    #Purpose    :  creates a special "matrix" object that can cache its inverse
    #Arguments  :  A Square Matrix (Assuming the matrix assume that the matrix supplied is always invertible)
    #Returns    :  The Special Matrix Object that holds the actual matrix and cached inverse of the matrix
    
    InverseOfX<-NULL
    set<-function(y)
    {
        # Assigns the value to x, Assuming the matrix assume that the matrix supplied is always invertible
        x<<-y
    }
    
    get<-function()
    {
        # Returns the Value of x
        x
    }
    
    setInverse<-function(Inverse = matrix())
    {
        # Assigns\caches the value to InverseOfx
        InverseOfX<<-Inverse
    }
    
    getInverse<-function()
    {
        # Returns the value to InverseOfx
        InverseOfX
    }
    
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve function computes the inverse of the special "matrix"
cacheSolve <- function(x, ...)
{
        
    #Purpose    :  Return a matrix that is the inverse of 'x'
    #Arguments  :  The special "matrix" returned by makeCacheMatrix function above
    #Returns    :  Return a matrix that is the inverse of 'x'
    
    #iX - Inverse of Matrix X
    
    iX <- x$getInverse()
    if(!is.null(iX)) {
        message("getting cached data")
        return(iX)
    }
    MatrixData <- x$get()
    # Assuming matrix supplied is always invertible
    iX <- solve(MatrixData)
    x$setInverse(iX)
    iX
    
}

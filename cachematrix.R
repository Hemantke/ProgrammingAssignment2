
## The below function makeCacheMatrix returns a list of 4 functions which can be
## used to get, or set the matrix itself, as well as getINV and setINV which can
## be used to get or set the inverse of the matrix, for caching purposes

makeCacheMatrix <- function(x = matrix()) 
{
    invM=NULL
    
    set<- function(y)
    {
        x<<-y
        invM=NULL
    }
    
    get<- function() x
    
    setINV<- function(calc_INV)
    {
        invM<<- calc_INV
    }
    
    getINV<- function() invM
    
    list(set=set,get=get,setINV=setINV,getINV=getINV)
    
}


## The function cacheSolve first searches if the inverse is already calculated and
## cached in the passed x, which is the list of functions describing the matrix
## If it's not yet calculated, it calculates inverse, caches it, and the returns it.

cacheSolve <- function(x, ...)
{
    inv<-x$getINV()
    
    if(!is.null(inv))
    {
        message("Getting cached data")
        return(inv)
        
    }
    datas<-x$get()
    inv<-solve(datas)
    x$setINV(inv)
    inv
}

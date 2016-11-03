##Given below are two functions that are used to create a object
##that creates and caches the inverse of an invertible matrix

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 4 functions

makeCacheMatrix = function(x = matrix())
{
    temp = NULL
    
    #Set the value of matrix
    set = function(y)
    {
        x <<- y
        temp <<- NULL
    }
    
    #Get the value of matrix
    get = function() x
    
    # Template to Set the inverse of matrix
    set.inv = function(solve) temp <<- solve
    
    #Template to  the Extract Inverse of matrix
    get.inv = function() temp
    
    #Make list
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv) 
}


## This Function generates the inverse of a matrix subject to condition that...
##...the inverse is already not in the cache. If condition is not fulfilled....
##...inverse is calculated otherwise the one already in cache is returned

cacheSolve = function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    final = x$get.inv()
    
    # Check whether inverse is stored in cache or not
    if(!is.null(final))
    {
        message("getting cached data")
        return(final)
    }
    
    #If inverse is not in cache, prepare to.inv to be inverted 
    to.inv = x$get()
    
    #Compute Inverse
    final <- solve(to.inv, ...)
    
    #Store in cache
    x$set.inv(final)
    
    #return Inverse
    return (final)
    
}

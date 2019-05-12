## makecachematrix: This function creates a special "matrix" object
##that can cache its inverse.

makecachematrix<-function(x=matrix()){
    ## Create an empty inverse
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) m<<-inverse
    ##get inverse
    getinverse<-function() m
    list(set = set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cachexolve: This function computes the inverse of the special "matrix" returned
##by makecachematrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.
cachesolve<-function(x,...){
    ## get an inverse of a matrix-x, this inverse is also a matrix-m
    m<-x$getinverse()

    ## return m if the function above return a un-null value
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    ##transfer the calculated m to setinverse
    x$setinverse(m)
    m
}

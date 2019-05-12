## makecachematrix: This function creates a special "matrix" object
##that can cache its inverse.

makecachematrix<-function(x=matrix()){
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) m<<-inverse
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
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()           #query the x vector's cache         
    if(!is.null(m)) {           #if there is a cache
        message("getting cached data") 
        return(m)                #just return the cache, no computation needed
    }
    data <- x$get()             #if there's no cache
    m <- mean(data, ...)        #we actually compute them here
    x$setmean(m)                #save the result back to x's cache
    m                           #return the result
}

makeCacheMatrix <- function(x) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse = function(solve) m <<- solve
    getinverse = function() m
    list(set = set,get = set,
         setinverse=setinverse,
         getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message('getting cached matrix')
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}


#edit

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y=NULL) {
        x <<- y
        m <<- NULL
    }
    get <- function(x) 
    setinverse = function(solve) {
        m <<- solve
    }
    getinverse = function(m)
    list(set = set,get = set,
         setinverse=setinverse,
         getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message('getting cached matrix')
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

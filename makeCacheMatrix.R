## Cache Matrix inverse set function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatx <- function(mx) m <<- mx
    getmatx <- function() m
    list(set = set, get = get,
         setmatx = setmatx,
         getmatx = getmatx)
}


## Cache matrix inverse calculation function
cacheSolve <- function(x,..){
    m <- x$getmatx()
    if(!is.null(m)){
        print("getting cached inverse matrix")
        return(m)
    }
    inv <- x$get()
    m <- solve(inv)
    x$setmatx(m)
    m
    
}

#### Test above functions
### create makeCacheMatrix object-1 with 2*2 matrix passed as parameter
#mmx1<-makeCacheMatrix(matrix(1:4,2,2))

##get makeCacheMatrix object-1 matrix through get()
#mmx1$get()

###Calculate matrix inverse by calling cachesolve function and makeCacheMatrix object-1 in para.
#cacheSolve(mmx1)

###create 2 by 2 matrix using rbind.
#ex = rbind(c(1, -1/4), c(-1/4, 1))

### create makeCacheMatrix object-2 with 'ex' matrix in parameter
#mmx2<-makeCacheMatrix(ex)

##get makeCacheMatrix object-2 matrix through get()
#mmx2$get()

##Calculate matrix inverse by calling cachesolve function and makeCacheMatrix object-2 in para.
#cacheSolve(mmx2)

##get makeCacheMatrix object-1 inverse matrix from cache memory
#mmx1$getmatx()

##get makeCacheMatrix object-2 inverse matrix from cache memory
#mmx2$getmatx()





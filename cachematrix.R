## makeCacheMatrix is used to assign a set of functions and an initial input to
## an object (for clarity, "mkcmobject" in the following). CacheSolve takes this object as its
## input, allowing it access to the functions of makeCacheMatrix as well as its initial
## input (because values were assigned to the parent environment via <<-) - 
## these are used to check whether the function was used before (= cache filled), and if not
## calculate and store the result using the original input x. 
## Both functions are interdependent.  

## Defines 4 functions: 
## set: is unnecessary for the first use (as x is already defined), can be used to later change input 
## via mkcmobject$set(y), with y = new matrix
## get: calls x (initial input or newly defined input from mkcmobject$set(y))
## setinverse: assigns i the inverse matrix by giving it the calculated result (i) of cacheSolve as an input
## getinverse: calls i (= the inverse caclulated in cacheSolve)  
## Then defines the assigned object as a list, naming the functions as elements (so they can be called by 
## mkcmobject$).   

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Takes mkcmobject as an input. Assigns i (the inverse) the value of i defined in mkcmobject and
## prints i. If i = NULL (because function is called for the first time), calculates i by assigning "matrix"
## the initial input x by calling get, calculating the inverse via SOLVE(matrix), and passing the result as 
## the "inverse"-input to getinverse (which in turn assigns it to i in the parent environment, hence, the cache).
## If cacheSolve is now called again, i is already defined (hence, !NULL), and directly printed.  

cacheSolve <- function(mkcmobject, ...) {
        i <- mkcmobject$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- mkcmobject$get()
        i <- solve(matrix) 
        mkcmobject$setinverse(i)
        i
}

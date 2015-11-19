## Invert matrix and cache the inverted matrix value

## makeMatrix creates a list of 4 items that cacheSolve references

makeMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(solve) m <<- solve
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Returns the inverse of the matrix if it doesn't already exists
## Returns the stored value of the inverted matrix if it exists

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
}

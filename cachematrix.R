## makeCacheMatrix function creates a special matrix, which is really a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## i represents the inverse of matrix. This is set to null when matrix is stored in main function
  set <- function(y){ ## set function is used to change the matrix stored in the main function. Inverse is set to null
    x <<-y  ## <<- substitutes the matrix x with y in the main function (if we used <-, then the substitution would only occur in the set function) 
    i <<- NULL ## restores the value of inverse i to NULL, because the old inverse from the old matrix is not needed anymore
  }
  get <- function() x ## get function returns matrix x stored in the main function
  setinverse <-function(inverse) i<<- inverse ## setinverse allows us to store an inverse value
  getinverse <- function() i ## getinverse simply returns the inverse value
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse  )
}

## Using makeCacheMatrix with matrix m as the input 
m<- c(1,2,3,4,5,6,7,8,0) 
dim(m) <- c(3,3) 
a<- makeCacheMatrix(m)
a


## cacheSolve function calculates the inverse of the special matrix created with the above function
## However, it first checks to see if the inverse has already been calculated 

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##first cacheSolve verifies that the value i, stored previously with getinverse, exists and is not NULL
  if(!is.null(i)){
    message("getting cached data")
    return(i) ## if i exists in memory, cacheSolve simply returns a message and the value of i
  }
  data <- x$get() ##if i does not exists in memory, data gets the matrix stored with makeCacheMatrix, i calculates the inverse, and x$setinverse(i) stores it 
  i <-solve(data)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}

cacheSolve(a)

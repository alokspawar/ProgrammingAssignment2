## makeCacheMatrix function accepts the squre coefficent matrix
## this function sets the data and prepare the memory cache m
## this function the return the object of funtion
## e.g if  i pass the  4 * 4 matrix
## genrating the matrix 
## > x<- { i <- 1:4; outer(i - 1, i, "+") }
## > x
## [,1] [,2] [,3] [,4]
## [1,]    1    2    3    4
## [2,]    2    3    4    5
## [3,]    3    4    5    6
## [4,]    4    5    6    7
## > z<-makeCacheMatrix(x)
## above command will create a matrix object and store in variable

makeCacheMatrix <- function(coMatrix = matrix()) {
        m <- NULL
        ## if coMatrixData is null then genrate dynamic matrix of length provided
        ## else use provided matrix in coMatrixData
        set <- function(y) {
                coMatrix <<- y
                m <<- NULL
        }
        get <- function() coMatrix
        setinverse <- function(data) m <<- solve(data)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cahe solve function will accept the object created by make cachematrix object
## this function check if there is matrix already cached then retirve else calculate
## and store in cache

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        x$setinverse(data)
        x$getinverse()
}


### Sample Execution and unit testing of above function
# > y<- { i <- 1:4;1/outer(i - 1, i, "+") }
# > y
# [,1]      [,2]      [,3]      [,4]
# [1,] 1.0000000 0.5000000 0.3333333 0.2500000
# [2,] 0.5000000 0.3333333 0.2500000 0.2000000
# [3,] 0.3333333 0.2500000 0.2000000 0.1666667
# [4,] 0.2500000 0.2000000 0.1666667 0.1428571
# > z<-makeCacheMatrix(y)
# > z
# $set
# function (y) 
# {
#         coMatrix <<- y
#         m <<- NULL
# }
# <environment: 0x10e8abc60>
#         
#         $get
# function () 
#         coMatrix
# <environment: 0x10e8abc60>
#         
#         $setinverse
# function (solve) 
#         m <<- solve
# <environment: 0x10e8abc60>
#         
#         $getinverse
# function () 
#         m
# <environment: 0x10e8abc60>
#         
#         > cacheSolve(z)
# [,1]  [,2]  [,3]  [,4]
# [1,]   16  -120   240  -140
# [2,] -120  1200 -2700  1680
# [3,]  240 -2700  6480 -4200
# [4,] -140  1680 -4200  2800


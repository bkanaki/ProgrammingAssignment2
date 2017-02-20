## Put comments here that give an overall description of what your
## functions do

# The following function creates a special matrix, which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL # chaches matrix for inversion
  set <- function(new_mat){ # set function for matrix
    x <<- new_mat
    mat <<- NULL
  }
  get <- function() x # get function for matrix
  set_inv <- function(inv_mat) mat <<- inv_mat # set function for inverse
  get_inv <- function() mat # get function for inverse
  # return a list of methods
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


# The following function calculates the invrse of the special "matrix" 
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates the 
# inverse of the matrix and sets the value of the inverse in the cache 
# via the set_matrix function.

cacheSolve <- function(x, ...) {
  mat <- x$get_inv() # get the value if inverse to check if it is calculated
  if(!is.null(mat)) { # if it is calculated, return fro cache
    message("getting cached data")
    return(mat)
  }
  data <- x$get() # get the matrix from the object
  mat <- solve(data, ...) # calculate the inverse using solve function
  x$set_inv(mat) # set the inverse so that it can be cached
  mat # return the inverse matrix
}

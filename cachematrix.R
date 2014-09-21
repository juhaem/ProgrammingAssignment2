## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates an empty matrix with same dimensions as the matrix to be solved 
makeCacheMatrix <- function(x = matrix())
  {
#   the matrix name will be stored here
  changed <<-NULL 
  
  # checks that the matrix is invertible from it dimensions (should be n by n) 
  # - if not, print out error message
  # - if yes, create an empty matrix solved_x the same size as x   
  #   and store the name
dims <- dim(x)
  if(!dims[1]==dims[2])print("must be a square matrix") 
  else 
    {
    matrix(nrow=dims[1],ncol=dims[2]) ->> solved_x
    substitute(x) ->> changed
    }
  }

## Write a short comment describing this function

cacheSolve <- function(x, ...)
  {
        ## Return a matrix that is the inverse of 'x'
# check the matrix name, if makeCacheMatix needs to be called
#   otherwise check the cache
#   if empty, solve
#   finally print the inverse in any case
  
  if(changed==substitute(x)) 
    {
    if(!is.na(solved_x[1,1]))
      {
      message("from cache")
      print(solved_x)
      }
    else
      {
      solve(x) ->> solved_x
      substitute(x) ->> changed
      message(paste("matrix", changed,"solved and cached"))
      print(solved_x)
      }
    }
  else
    {
    makeCacheMatrix(x)
    solve(x) ->> solved_x
    changed <<- substitute(x)
    message(paste("new matrix", changed,"solved and cached"))
    print(solved_x)
    }
  }


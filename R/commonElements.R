# The CommonElements function
#' Compares two character vectors and returns a logical vector that is true when both string have common leading and trailing elements.
#' 
#' @param x, a string to compare.
#' @param y, another string to compare.
#' 
commonElements <- function(x, y){
  common.x <- rep(FALSE, length(x))
  common.y <- rep(FALSE, length(y))
  
  # Leading common elements
  leading <- leadingCommonElements(x, y)
  
  common.x[leading] <- rep(TRUE, length(leading))
  common.y[leading] <- rep(TRUE, length(leading))
  
  # If one of the strings is a truncated version of the other, no need to check trailing
  if (sum(common.x) == length(common.x) || sum(common.y) == length(common.y)) 
    return(list("x" = common.x, "y" = common.y))
  
  # Trailing common elements
  rest.x <- x[-which(common.x)]
  rest.y <- y[-which(common.y)]
  trailing <- leadingCommonElements(rev(rest.x), rev(rest.y))
  trailing.x <- rep(length(x) + 1, length(trailing)) - trailing
  trailing.y <- rep(length(y) + 1, length(trailing)) - trailing
  
  common.x[trailing.x] <- rep(TRUE, length(trailing.x))
  common.y[trailing.y] <- rep(TRUE, length(trailing.y))
  
  return(list("x" = common.x, "y" = common.y))
}
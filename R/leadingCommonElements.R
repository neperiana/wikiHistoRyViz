# The leadingCommonElements
# returns leading common elements of strings x and y through a numeric vector of indexes
leadingCommonElements <- function(x, y){
  
  min.length <- min(length(x), length(y))
  
  if (min.length == 0) return(integer())
  
  min.x <- x[1:min.length]
  min.y <- y[1:min.length]
  
  common.elements <- unlist(Map('==', min.x, min.y))
  
  if (sum(common.elements) == length(common.elements)) return(1:min.length)
  
  no.common.elements <- match(FALSE, common.elements) - 1
  
  if (no.common.elements == 0) return(integer())
  else return (1:no.common.elements)
}
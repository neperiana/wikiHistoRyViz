# LCS: computes longest common subequence
# Takes two character vectors
# First it checks common leading and trailing elements
# and then applies dynammic LCS to the rest of the text
# Returns numeric index of longest common subsequence for both vectors a and b
LCS <- function(a, b){
  if(length(a) == 0 || length(b) == 0) 
    return(list("v.a" = numeric(), "v.b" = numeric()))
  
  # Find leading and trailing common subsequences
  common.elements.ab <- commonElements(a, b)
  common.elements.a <- common.elements.ab$x
  common.elements.b <- common.elements.ab$y
  
  # If any is completely common to the other we are finished
  if(sum(common.elements.a)==length(common.elements.a) ||
       sum(common.elements.b)==length(common.elements.b)) 
    return(list("v.a" = which(common.elements.a), 
                "v.b" = which(common.elements.b)))
  a.first.f <- match(FALSE, common.elements.a)
  b.first.f <- match(FALSE, common.elements.b)
  
  # Smaller elements that we want to inspect with dinamyc LCS
  middle.a <- a[!common.elements.a]
  middle.b <- b[!common.elements.b]
  middle.LCS <- dynamicLCS(middle.a, middle.b)
  
  common.elements.a[middle.LCS$v.a + a.first.f - 1] <- rep(TRUE,length(middle.LCS$v.a))
  common.elements.b[middle.LCS$v.b + b.first.f - 1] <- rep(TRUE,length(middle.LCS$v.b))
  
  return(list("v.a" = which(common.elements.a), 
              "v.b" = which(common.elements.b)))
}
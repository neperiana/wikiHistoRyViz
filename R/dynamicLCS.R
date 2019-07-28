#' The dinamycLCS function
#' Computes longest common subequence with dynammic algorithm for two vectors.
#' 
#' @param a, a vector to compare.
#' @param b, another vector to compare.
#'
dynamicLCS <- function(a, b){
  
  length.a <- length(a) + 1
  length.b <- length(b) + 1
  
  if(length.a == 1 || length.b == 1) return(0)
  
  M <- matrix(rep(0,length.a*length.b), nrow = length.a)
  
  for(i in 2:length.a){
    for(j in 2:length.b){
      if(a[i-1] == b[j-1]) M[i,j] <- M[i-1,j-1] + 1
      else M[i,j] <- max(M[i-1,j], M[i,j-1])
    }
  }
  
  lcs.a <- numeric()
  lcs.b <- numeric()
  
  i <- length.a
  j <- length.b
  m <- M[i,j]
  
  while (m > 0){
    if (M[i-1,j] == m){
      i <- i-1
      m <- M[i,j]
    }
    else if(M[i,j-1] == m){
      j <- j-1
      m <- M[i,j]
    }
    else{
      i <- i-1
      j <- j-1
      m <- M[i,j]
      lcs.a <- c(lcs.a, i)
      lcs.b <- c(lcs.b, j)
    }
  }
  
  return(list("v.a" = lcs.a[order(lcs.a)], "v.b" = lcs.b[order(lcs.b)]))
}
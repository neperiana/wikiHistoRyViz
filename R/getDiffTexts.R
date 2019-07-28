#' @title The getDiffTexts function
#' 
#' @description Return the indexes of common and different elements of two texts.
#' 
#' @param a.text, a string to compare.
#' @param b.text, another string to compare.
#' @param sep, short string containing separator. We will use this string to split texts into smaller pieces. " " would split texts into words and "\\." would split texts into sentences. 
#' 
#' @section Warnings:
#' geDiffTexts uses LCS algortihm to compute differences between two texts. If texts are considerably long be aware that this implementation of LCS is O(n^2) expensive, both time and memory wise, so it is recommended to use sep = "\\." to speed up the execution. 
#'
#' @keywords text processing, text differences, LCS
#' @export
#' @examples
#' text.1 <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque nec vehicula purus. Praesent vehicula risus a purus sollicitudin cursus. Nullam hendrerit efficitur gravida. Sed ut quam a urna sollicitudin dapibus. Integer vitae ultricies sapien, eu tincidunt lacus. Suspendisse eget dapibus diam."
#' text.2 <- "Nullam hendrerit efficitur gravida. Sed ut quam a urna sollicitudin dapibus. Integer vitae ultricies sapien, eu tincidunt lacus. Pellentesque sed dui turpis."
#' getDiffTexts(text.1, text.2)
#'  
getDiffTexts <- function(a.text, b.text, sep = " "){
  if (length(b.text) > 0 && b.text != ""){  # If b.text is null then there's no difference to compute
    #  Transforming to vectors
    a.text.v <- c(unlist(strsplit(a.text, split = sep)))
    b.text.v <- c(unlist(strsplit(b.text, split = sep)))
    
    if(length(a.text.v)==0 || a.text == ""){
      return(list("b.text.diff" = 1:length(b.text.v), 
                  "b.text.same" = numeric(), 
                  "a.text.same" = numeric())) 
    }
    #  Getting Longest Common Subsequence
    LCSOut <- LCS(a.text.v, b.text.v)
    b.text.same <- LCSOut$v.b
    a.text.same <- LCSOut$v.a
    #  Computing and returning complementary to LCS
    b.text.all <- 1:length(b.text.v)
    b.text.diff <- b.text.all[is.na(pmatch(b.text.all, b.text.same,
                                       nomatch = NA_integer_))]
    
    return(list("b.text.diff" = b.text.diff, 
                "b.text.same" = b.text.same, 
                "a.text.same" = a.text.same))  
  }
  else return(list("b.text.diff" = numeric(), 
                   "b.text.same" = numeric(), 
                   "a.text.same" = numeric()))
}
#' The getRawDataPath function
#' 
#' Returns the system path of the raw data included in this package for testing purposes.
#' This raw data includes last 1000 revisions of 
#' 
#' @param article.name, article name for which you want to retrieve raw data.
#' 
#' @keywords text processing, text differences, LCS
#' @export
#' @examples
#' path <- getRawDataPath("Vic")
#' 
getRawDataPath <- function(article.name){
  if (!is.character(article.name)) 
    stop("article.name must be character.")
  if (length(article.name) != 1) 
    stop("article.name must contain only/at least one element.")
  
  file <- switch(article.name,
                 "Brazil"    = "Brazil.xml",
                 "Ukraine"   = "Ukraine.xml",
                 "Chocolate" = "Chocolate.xml",
                 "Coca-Cola" = "Coca-Cola.xml",
                 "Facebook"  = "Facebook.xml",
                 "Wikipedia" = "Wikipedia.xml",
                 "Vic"       = "Vic.xml",
                 "Tarragona" = "Tarragona.xml")
  if (is.null(file))
    stop(paste(article.name, "not found in raw data.", sep = " "))
  
  system.file("extdata", file, package = "wikiHistoRyViz")
}
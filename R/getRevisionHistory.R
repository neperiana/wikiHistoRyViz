#' @title The getRevisionHistory function
#' 
#' @description Returns wikiRevisionHistory R6-class for an specific wikipedia page, after having parsed an xml wikipedia extraction.
#' 
#' @param page.title, name of the wikipedia page to be parsed. If name includes spaces, article.name needs to include "_".
#' @param file, xml file containing wikipedia revision history.
#' @param granularity, indicated whether to look for differences in either words or sentences. If sentences are chosen, the change in only one word will look as if the whole sentence was modified.
#' 
#' @section Warnings:
#' This function uses getDiffTexts (which uses LCS). If granularity = "word" is chosen, computation time ofr long wikipedia articles can be high. The use of "sentence" is recommended in these cases. 
#'
#' @keywords xml parser, R6, Wikipedia revision history
#' @export
#' @import R6
#' @import XML
#' @examples
#' getPageXML("Patum_de_Berga", file = 'Patum_de_Berga.xml')
#' Patum <- getRevisionHistory("Patum de Berga", file = 'Patum_de_Berga.xml')
#' Patum.words <- getRevisionHistory("Patum de Berga", file = 'Patum_de_Berga.xml', granularity = "word")
#' 
getRevisionHistory <- function(page.title, file, granularity = c("sentence","word")){
  if (!grepl("\\.xml$", file)) stop("Error: file not in xml format.")
  
  pageParser <- wikiRevisionHistory$new(page.title)
  
  d <- xmlEventParse(file = file, branches = pageParser$saxHandler())
  pageParser$convert.to.data.frames()
  
  return(pageParser)
}
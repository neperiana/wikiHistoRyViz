#' @title The getPageXML function
#' 
#' @description Connects to wikipedia API and retrieves an xml file with the whole revision history of an specific page.
#' 
#' @param titles, titles of pages that need to be retrieved.
#' @param file, indicates where the xl file needs to be saved.
#' @param language, indicates the version of wikipedia to retrieve results from. Default is English wikipedia.
#' @param limit, indicated the maximum amount of revisions that need to be retrieved. If set to "max" the function retrieved the whole revision history.
#' 
#' @section Warnings:
#' This function relies on current version of the Wikipedia API and may need to be updated when API changes. 
#'
#' @keywords Wikipedia API, xml
#' @export
#' @import XML
#' @import httr
#' @examples
#' getPageXML("Patum_de_Berga", file = 'Patum_de_Berga.xml')
#'  
getPageXML <- function(titles, file, language = "en", limit = "max"){
  
  path <- paste(paste("http://", language, ".wikipedia.org/w/index.php?title=Special:Export", sep = ""),
                paste("pages=", paste(titles, collapse = "|"), sep = ""),
                ifelse(limit == "max", "history", as.character(limit)),
                "action=submit",
                sep = "&")
  
  request <- GET(path)
  if(request$status_code != 200) 
    stop(paste("Error! Wiki API request not successful. Status code:", request$status_code, sep = " "))
  
  content.xml <- content(request)
  saveXML(content.xml, file = file)
}
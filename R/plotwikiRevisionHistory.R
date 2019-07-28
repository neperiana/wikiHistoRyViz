#' @title The plot.wikiRevisionHistory function
#' 
#' @description plots history flow for article revisions contained in wikiRevisionHistory R6-class.
#' 
#' @param x, name of the wikipedia page to be parsed. If name includes spaces, article.name needs to include "_".
#' @param y, .
#' @param fill.by, .
#' @param x.axis, .
#' 
#' @keywords plot, R6, Wikipedia revision history
#' @export
#' @import R6
#' @import ggplot2
#' @examples
#' getPageXML("Patum_de_Berga", file = 'Patum_de_Berga.xml')
#' Patum <- getRevisionHistory("Patum de Berga", file = 'Patum_de_Berga.xml')
#' 
plot.wikiRevisionHistory <- function(x, y, fill.by = c("name","id"), 
                                     x.axis = c("time","equal"), ...){
  wiki.revisions <- x$getArticleRevisions()
  wiki.texts <- x$getArticleContributions()
  wiki.contributors <- x$getContributors()
  
  wiki.revisions$time <- strptime(wiki.revisions$revTime, "%Y-%m-%dT%H:%M:%S")
  wiki.revisions$prevTime <- c(wiki.revisions$time[1], wiki.revisions$time[1:dim(wiki.revisions)[1] - 1])
  
  wiki.texts <- merge(wiki.revisions[,c("revId","time", "prevTime")], wiki.texts, by = "revId")
  
  # Sorting
  wiki.texts <- wiki.texts[order(wiki.texts$time),]
  
  # Creating polygons
  wiki.texts$i <- seq(dim(wiki.texts)[1])
  wiki.text.1 <- wiki.texts[,c('i','revId', 'time', 'contributorId', 'posMin')]
  wiki.text.2 <- wiki.texts[,c('i','revId', 'time', 'contributorId', 'posMax')]
  wiki.text.3 <- wiki.texts[,c('i','revId', 'prevTime', 'contributorId', 'prevPosMax')]
  wiki.text.4 <- wiki.texts[,c('i','revId', 'prevTime', 'contributorId', 'prevPosMin')]
  names(wiki.text.1)[5] <- c("pos")
  names(wiki.text.2)[5] <- c("pos")
  names(wiki.text.3)[5] <- c("pos")
  names(wiki.text.4)[5] <- c("pos")
  names(wiki.text.3)[3] <- c("time")
  names(wiki.text.4)[3] <- c("time")
  wiki.text.1$type <- c("current", dim(wiki.texts)[1])
  wiki.text.2$type <- c("current", dim(wiki.texts)[1])
  wiki.text.3$type <- c("previous", dim(wiki.texts)[1])
  wiki.text.4$type <- c("previous", dim(wiki.texts)[1])
  wiki.poly <- rbind(wiki.text.1, wiki.text.2, wiki.text.3, wiki.text.4)
  
  wiki.poly <- wiki.poly[order(wiki.poly$i), ]
  wiki.texts$words <- wiki.texts$posMax - wiki.texts$posMin + 1
  
  plot <- ggplot(data = wiki.texts, aes(x = time, y = words, group = contributorId)) + 
            geom_polygon(data = wiki.poly, aes(x = time, y = pos, 
                                               fill = contributorId, group = i, alpha = 0.25)) +
            geom_bar(stat = "identity", aes(colour = contributorId), width = 100) +
            theme(legend.position = "none")
  return(plot)
}
#' The wikiRevisionHistory function
#' 
#' R6 class that parses a wikipedia xml extraction and retrieves revision history for a specific wikipedia page.
#'
wikiRevisionHistory <- 
  R6Class("wikiRevisionHistory",
          public = list(sep = NA,
                        articleId = NA,
                        articleName = NA,
                        revisions = NA,
                        revisionContributions = NA,
                        contributors = NA,
                        previousArticle = NA,
                        initialize = function(articleName, granularity = c("word","sentence")) { 
                          self$sep <- switch(match.arg(granularity),
                                             word = " ",
                                             sentence = "\\.")
                          
                          self$articleName <- articleName
                          self$previousArticle <- list("text" = "",
                                                       "ids" = character(),
                                                       "names" = character(),
                                                       "begIndex" = numeric(),
                                                       "endIndex" = numeric())
                          self$revisions <- list( "revId" = numeric(), 
                                                  "revTime" = character(),
                                                  "contributorIdType" = factor(),
                                                  "contributorId" = character(),
                                                  "contributorName" = character())
                          self$revisionContributions <- list("revId" = numeric(), 
                                                             "contributorId" = character(),
                                                             "posMin" = numeric(),
                                                             "posMax" = numeric(),
                                                             "prevPosMin" = numeric(),
                                                             "prevPosMax" = numeric())
                        },
                        saxHandler = function() {
                          getContributions <- function(currentArticle, contrName, contrId){
                            priorArticle <- paste(self$previousArticle[["text"]], 
                                                  collapse = gsub("\\", "", self$sep, fixed = TRUE))
                            
                            # get differences between current and prior
                            Diffs <- getDiffTexts(priorArticle, currentArticle, sep = self$sep)
                            diffNew <- Diffs[[1]]
                            comNew <- Diffs[[2]]
                            comOld <- Diffs[[3]]
                            
                            # assign contributor to new elements
                            contributions <- unlist(strsplit(currentArticle, split = self$sep))
                            
                            names <- character(length(contributions))
                            
                            names[comNew] <- self$previousArticle[["names"]][comOld]
                            names[diffNew] <- rep(contrName, length(diffNew))
                            ids <- character(length(contributions))
                            ids[comNew] <- self$previousArticle[["ids"]][comOld]
                            ids[diffNew] <- rep(contrId, length(diffNew))
                            
                            if(length(contributions)>0){
                              words <- sapply(contributions, function(str) length(unlist(strsplit(str, split = " "))))
                              begIndex <- cumsum(c(1,words[-length(words)]))
                              endIndex <- cumsum(words)
                              prevBegIndex <- rep(NA,length(contributions))
                              prevBegIndex[comNew] <- self$previousArticle[["begIndex"]][comOld]
                              prevEndIndex <- rep(NA,length(contributions))
                              prevEndIndex[comNew] <- self$previousArticle[["endIndex"]][comOld]
                            }
                            else{
                              begIndex <- numeric()
                              endIndex <- numeric()
                              prevBegIndex <- numeric()
                              prevEndIndex <- numeric()
                            }
                            
                            # compute contributor contributions
                            changes <- ids[1:length(ids)-1]!=ids[2:length(ids)]
                            contributionId <- cumsum(c(1,changes))
                            contributors <- list("contrId" = unique(contributionId), 
                                                 "ids" = ids[c(TRUE,changes)],
                                                 "posMin" = ave(begIndex, contributionId, FUN = min)[c(TRUE,changes)],
                                                 "posMax" = ave(endIndex, contributionId, FUN = max)[c(TRUE,changes)],
                                                 "prevPosMin" = ave(prevBegIndex, contributionId, FUN = min)[c(TRUE,changes)],
                                                 "prevPosMax" = ave(prevEndIndex, contributionId, FUN = max)[c(TRUE,changes)])
                            
                            # return values
                            self$previousArticle <- list("text" = contributions,
                                                         "ids" = ids,
                                                         "names" = names,
                                                         "begIndex" = begIndex,
                                                         "endIndex" = endIndex)
                            return(contributors)
                          }
                          extractRevisions <- function(revNode){
                            revId <- xmlValue(revNode[["id"]])
                            self$revisions[["revId"]] <- c(self$revisions[["revId"]],
                                                           revId)
                            
                            revTime <- as.character(xmlValue(revNode[["timestamp"]]))
                            self$revisions[["revTime"]] <- c(self$revisions[["revTime"]],
                                                             revTime)
                            
                            contributorNode <- revNode[["contributor"]]
                            
                            contrName <- contributorNode[["username"]]
                            
                            
                            # Only want ip if contributor is not registered
                            if(length(contrName)>0){ 
                              Type <- "registered"
                              contrId <- xmlValue(contributorNode[["id"]])
                            }
                            else {
                              Type <- "anonymous"
                              contrId <- xmlValue(contributorNode[["ip"]])
                            }
                            contrName <- xmlValue(contrName)
                            self$revisions[["contributorName"]] <- c(self$revisions[["contributorName"]],
                                                                     contrName)                      
                            self$revisions[["contributorIdType"]] <- c(self$revisions[["contributorIdType"]],
                                                                       Type)
                            self$revisions[["contributorId"]] <- c(self$revisions[["contributorId"]],
                                                                   contrId)
                            
                            
                            print(revId)
                            timestamp()
                            
                            currentArticle <- unlist(xpathApply(revNode, "text", xmlValue))
                            
                            contributions <- getContributions(currentArticle, contrName, contrId)
                            self$revisionContributions[["revId"]] <- 
                              c(self$revisionContributions[["revId"]], 
                                rep(revId,length(contributions$ids)))
                            self$revisionContributions[["contributorId"]] <- 
                              c(self$revisionContributions[["contributorId"]], 
                                contributions$ids)
                            self$revisionContributions[["posMin"]] <- 
                              c(self$revisionContributions[["posMin"]], 
                                contributions$posMin)
                            self$revisionContributions[["posMax"]] <- 
                              c(self$revisionContributions[["posMax"]], 
                                contributions$posMax)
                            self$revisionContributions[["prevPosMin"]] <- 
                              c(self$revisionContributions[["prevPosMin"]], 
                                contributions$prevPosMin)
                            self$revisionContributions[["prevPosMax"]] <- 
                              c(self$revisionContributions[["prevPosMax"]], 
                                contributions$prevPosMax)
                          }
                          page <- function(node) {
                            articleId = xmlValue(node[["id"]])
                            articleName = xmlValue(node[["title"]])
                            if(articleName == self$articleName){
                              self$articleId <- articleId
                              lapply(node[names(node) == "revision"],
                                     extractRevisions)  
                            }
                          }
                          return(c("page" = page))
                        },
                        convert.to.data.frames = function(){
                          self$revisions <- as.data.frame(self$revisions)
                          
                          self$revisionContributions <- as.data.frame(self$revisionContributions)
                          self$contributors <- 
                            unique(self$revisions[,c("contributorIdType",
                                                     "contributorId",
                                                     "contributorName")])
                        },
                        getArticleRevisions = function() self$revisions,
                        getArticleContributions = function() self$revisionContributions,
                        getContributors = function() self$contributors,
                        getArticleInfo = function() list(self$articleId, self$articleName)
          )
  )

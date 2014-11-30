### This script analyzes human rights coverage with word separating algorithms.
  
#  Prepping.
rm(list=ls())
library("matrixStats")

# read in data
uni.dtm <- read.csv("/Users/rterman/Desktop/uni_dtm.csv")

# subsetting for convenience
mena.uni <- uni.dtm[grep("mena",uni.dtm$doc),] # Only corp.2 Unigrams
mena.uni$doc <- NULL

la.uni <- uni.dtm[grep("la",uni.dtm$doc),] # Only corp.1 Unigrams
la.uni$doc <- NULL

eeca.uni <- uni.dtm[grep("eeca",uni.dtm$doc),] # Only corp.1 Unigrams
eeca.uni$doc <- NULL

africa.uni <- uni.dtm[grep("africa",uni.dtm$doc),] # Only corp.1 Unigrams
africa.uni$doc <- NULL

asia.uni <- uni.dtm[grep("asia",uni.dtm$doc),] # Only corp.1 Unigrams
asia.uni$doc <- NULL

west.uni <- uni.dtm[grep("west",uni.dtm$doc),] # Only corp.1 Unigrams
west.uni$doc <- NULL

corp.1.uni <- mena.uni 
corp.2.uni <- uni.dtm[-(grep("west",uni.dtm$doc)),]
corp.2.uni$doc <- NULL

# turning count dtm to rate dtm
corp.2.uni.rate <- corp.2.uni/rowSums(corp.2.uni)
corp.1.uni.rate <- corp.1.uni/rowSums(corp.1.uni)


  
#################################################
#### Independent linear discriminant measure ####
#### used in Mosteller and Wallace (1963) #######
#################################################

# This function takes in the corp.1 and corp.2 document term matricizes and outputs the independent linear discriminant score to each word.

l.d.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
  # calculate means and vars
  means.corp.1 <- colSums(corp.1) / sum(colSums(corp.1))
  var.corp.1 <- colVars(as.matrix(corp.1))
  means.corp.2 <- colSums(corp.2) / sum(colSums(corp.2))
  var.corp.2 <- colVars(as.matrix(corp.2))
  
  #calculate overall score
  scores <- (means.corp.1 - means.corp.2) / (var.corp.1 + var.corp.2)
  
  #putting that score in a dataframe
  scores <- data.frame(cbind(scores))
  names(scores) <- "ld.rate"
  return(scores)
}

uni <- l.d.scores(corp.1.uni.rate,corp.2.uni.rate) # apply to unigrams rate dtm
uni.count <- l.d.scores(corp.1.uni,corp.2.uni) # apply to unigrams count dtm
uni$ld.count <- uni.count[,1] # add score to data

## This function gets the top 20 discriminating words from each corpora, for each measure
top.40 <- function(matrix,score){ # where matrix is the score matrix and score is the specific measure (column in the score matrix)
  author1 <- matrix[order(score,decreasing = TRUE),][1:200,]
  data <- rbind(author1)
  return(data)
}

# Applying function to find top 40 discriminating words using Linear Discriminant scores (using rate DTM)
top.uni.ld <- top.40(uni,uni$ld.count)[,2,drop=FALSE] 
top.uni.ld # most discriminating unigrams


######################################
#### Standardized mean difference ####
######################################

# A function to get Standardized mean difference scores
s.m.d.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
  # calculate means and vars
  n.corp.1 <- sum(colSums(corp.1))
  n.corp.2 <- sum(colSums(corp.2))
  means.corp.1 <- colSums(corp.1) / n.corp.1
  var.corp.1 <- colVars(as.matrix(corp.1))
  means.corp.2 <- colSums(corp.2) / n.corp.2
  var.corp.2 <- colVars(as.matrix(corp.2))
  
  #calculate overall score
  score <- (means.corp.1 - means.corp.2) / sqrt((var.corp.1/n.corp.1) + (var.corp.2/n.corp.1))
  return(score)
}

# I didn't know whether to use rate or count DTM for this score, so I did both:
uni$smd.rate <- s.m.d.scores(corp.1.uni.rate,corp.2.uni.rate) # unigrams rate dtm
uni$smd.count <- s.m.d.scores(corp.1.uni,corp.2.uni) # unigrams count dtm

# Finding top 40 discriminating uni/trigrams using Standard mean difference and count DTM
top.uni.smd  <- top.40(uni,uni$smd.count)[,4,drop=FALSE]
top.uni.smd # top 40 unigrams (count DTM)
write.csv(top.uni.smd,"west.smd.csv")
rownames(top.uni.smd)

# Finding top 40 discriminating uni/trigrams using Standard mean difference and rate DTM
top.uni.smd.rate  <- top.40(uni,uni$smd.rate)[,3,drop=FALSE]
top.uni.smd.rate # top 40 unigrams (rate DTM)
```
#################################
##### Standardized Log Odds #####
#################################


# A function to find Standardized Log Odds scores
s.l.o.scores <- function(corp.1,corp.2){ # where x,y args are dtm dataframes for the two authors
  # calculate means and vars
  n.corp.1 <- sum(colSums(corp.1))
  n.corp.2 <- sum(colSums(corp.2))
  x.corp.1 <- colSums(corp.1)
  x.corp.2 <- colSums(corp.2)
  pi.corp.1 <- (x.corp.1 + 1) / (n.corp.1 + ncol(corp.1))
  pi.corp.2 <- (x.corp.2 + 1) / (n.corp.2 + ncol(corp.2))  
  log.odds.ratio <- log(pi.corp.1/(1-pi.corp.1)) - log(pi.corp.2 / (1-pi.corp.2))
  st.log.odds <- log.odds.ratio/sqrt(var(log.odds.ratio))
  return(st.log.odds)
}

uni$stlogoddse <- s.l.o.scores(corp.1.uni,corp.2.uni) # unigrams count dtm score

# Finding top 40 most discriminating uni/trigrams using Standardized Log Odds scores
top.uni.slo  <- top.40(uni,uni$stlogoddse)[,5,drop=FALSE] 
top.uni.slo # top 40 unigrams



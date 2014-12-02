#### This script writes text files for my text analysis
rm(list=ls())
## read in data

total <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/Data/New\ York\ Times/NYT.csv")

total.all <- total # retain all data
total.without.us <- subset(total,!grepl("united states",total$COUNTRY_FINAL,ignore.case=TRUE)) # without USA
total.violations <- subset(total,grepl("HUMAN RIGHTS VIOLATIONS",total$SUBJECT)) # only human rights violations
total.violations.no.us <- subset(total.without.us,grepl("HUMAN RIGHTS VIOLATIONS",total.without.us$SUBJECT)) # without USA, and only human rights violations
total <- total.violations.no.us

################
##### NYT ######
################

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/Data/New\ York\ Times/Texts")

write.text.each.article <- function(region){
  sub <-subset(total,REGION==region)
  n = nrow(sub)
  rowlist = 1:n
  write <- function(row){
    text <- sub$TEXT[row]
    write.csv(text,file=(paste(tolower(region),row,".txt",sep="")),row.names=FALSE)
  }
  lapply(rowlist,write)
}

write.text.each.article("EECA")
write.text.each.article("LA")
write.text.each.article("MENA")
write.text.each.article("West")
write.text.each.article("Africa")
write.text.each.article("Asia")


summary(total$REGION)

####################
##### Amnesty ######
####################

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/Data/Amnesty/Texts")

amnesty <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/Data/Amnesty/total_amnesty2.csv")

names(amnesty)
summary(amnesty$year)

write.amnesty <- function(row){
  title <- amnesty$title[row]
  title.scrape <- amnesty$title.scrape[row]
  teaser <- amnesty$teaser[row]
  teaser.scrape <- amnesty$teaser.scrape[row]
  text <- paste(title,title.scrape,teaser,teaser.scrape,sep=" \n ")
  write.csv(text,file=(paste(row,".txt",sep="")),row.names=FALSE)
}

write.amnesty(1)

rowlist <- 1:nrow(amnesty)
lapply(rowlist,write.amnesty)

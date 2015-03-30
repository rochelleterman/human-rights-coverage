### This script plots human rights violations over time by region and country.
### I use this analysis to make a count matrix, which I then use to fill in the rt$nyt column.

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/")
library(plyr)
library(ggplot2)
library(reshape2)

# Load data (optional)
total <- read.csv("Data/New\ York\ Times/NYT.csv")

############################
##### Subsetting data ######
############################
total.all <- total # retain all data
total.without.us <- subset(total,!grepl("united states",total$COUNTRY_FINAL,ignore.case=TRUE)) # without USA
total.violations <- subset(total,grepl("HUMAN RIGHTS VIOLATIONS",total$SUBJECT)) # only human rights violations
total.violations.no.us <- subset(total.without.us,grepl("HUMAN RIGHTS VIOLATIONS",total.without.us$SUBJECT)) # without USA, and only human rights violations

# the rest of the script will use the total.violations subset
total <- total.violations
write.csv(total,"Data/New\ York\ Times/nyt.violations.csv")
total$REGION <- as.factor(total$REGION)

########################################
######## Quick Sum and Barplots #######
########################################

# number of articles per region

n.region <- ddply(.data=total, .variables=.(REGION), .fun=nrow)
n.region

barplot(summary(total$REGION))
write.csv(n.region,"Results/n-region.csv")

# Number of articles per country

n.country <- ddply(.data=total, .variables=.(COUNTRY_CODE), .fun=nrow)
n.country
write.csv(n.country,"Results/n-country.csv")

#########################################################
##### Compute number of articles per country per year ###
#########################################################

counts <- data.frame(cbind(as.character(total$COUNTRY_CODE),total$YEAR)) # get all codes + yers
names(counts) <- c("iso3c","year")
counts <- counts[-which(is.na(counts$iso3c)),] # get rid of NA's
counts <-  unique(counts) # keep only unique pairs

# define function to number of articles per country year
country.per.year <- function(x,y){
  subset.data <- subset(total,as.character(COUNTRY_CODE)==x & YEAR==y)
  return(nrow(subset.data))
}

country.per.year("USA",1980) # testing - 10

counts$count <- unlist(mapply(country.per.year,x=counts$iso3c,y=counts$year))
write.csv(counts,"Results/n-country-year.csv")

################################################################
##### Compute total number of articles per year per region #####
################################################################

total <- total.violations.no.us

n.region.year <- ddply(.data=total, .variables=.(YEAR), .fun=summarize,"MENA"=sum(REGION=="MENA",na.rm=TRUE),"Asia"=sum(REGION=="Asia",na.rm=TRUE),"Africa"=sum(REGION=="Africa",na.rm=TRUE),"EECA"=sum(REGION=="EECA",na.rm=TRUE),"West"=sum(REGION=="West",na.rm=TRUE),"LA"=sum(REGION=="LA",na.rm=TRUE))
n.region.year

write.csv(n.region.year,"Results/n-region-year.csv")

melted <- melt(n.region.year,id.vars="YEAR",measure.vars=c("MENA","Asia","Africa","EECA","West","LA"))
names(melted) <- c("year","region","count")
melted

# regions over time.
ggplot(data=melted, aes(x=year,y=count,group=region,color=region)) + geom_line()



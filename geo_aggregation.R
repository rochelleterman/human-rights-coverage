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

n.country.year <- ddply(.data=total, .variables=.(YEAR,COUNTRY_CODE), .fun=nrow)
n.country.year

write.csv(n.country.year,"Results/n-country-year.csv")

################################################################
##### Compute total number of articles per year per region #####
################################################################

total <- total.violations.no.us

n.region.year <- ddply(.data=total, .variables=.(YEAR,REGION), .fun=nrow)
n.region.year

ggplot(data=n.region.year, aes(x=YEAR,y=V1,group=REGION,color=REGION)) + geom_line()

casted <- dcast(data = n.region.year, formula = YEAR ~ REGION, value.var = "V1")
casted

write.csv(n.region.year,"Results/n-region-year.csv")





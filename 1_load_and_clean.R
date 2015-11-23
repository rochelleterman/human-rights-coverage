### This script takes the csv's of articles (downloaded through lexisnexis and turned into cproduced via python script) puts them in one data frame, adds a year, etc.

rm(list=ls())
library("plyr")
setwd("/Dropbox/berkeley/Git-Repos/human-rights-coverage")

# Read files named xyz1111.csv, xyz2222.csv, etc. Change the pattern according to your data.
filenames <- list.files(path="Data/New\ York\ Times/CSVs\ -\ Raw\ Data", #change for your dir
                        pattern="The_New_York_Times_.*csv") 
l <- nchar(filenames[1]) # gets the number of characters of each filename
names <-substr(filenames,0,(l-4)) # Create list of file names without the ".csv" part
names
total <- data.frame() # initialize dataframe

# Load all files and take only necessary columns
for(i in names){
  filepath <- file.path("Data/New\ York\ Times/CSVs\ -\ Raw\ Data",paste(i,".csv",sep=""))
  x <- read.csv(filepath)
  x <- subset(x, select = c(DATE,PUBLICATION,BYLINE,LENGTH,ORGANIZATION,PERSON,GEOGRAPHIC,SUBJECT,TITLE,TEXT))
  total <- rbind(total,x)
}
names(total)
# Keep only data with "human rights" as a subject:
total <- subset(total,grepl("HUMAN RIGHTS", total$SUBJECT))

# Remove duplicates
# total <- total[!duplicated(total$TITLE),]

# Add new column for year
total$DATE <- as.character(total$DATE)
total$YEAR <- substr(total$DATE, nchar(total$DATE)-4, nchar(total$DATE))
total$YEAR <- as.integer(total$YEAR)
summary(total$YEAR)

# Export new data set
write.csv(total, file="Data/New\ York\ Times/NYT.csv", col.names=TRUE)

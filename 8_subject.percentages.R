### This script turns the "Subject" column into something usable.

setwd("/Dropbox/berkeley/Git-Repos/human-rights-coverage")

# Load data (optional)
total <- read.csv("Data/New\ York\ Times/NYT.csv")
total$X <- NULL

#########################
#### Coding Subjects ####
#########################

Sys.setlocale('LC_ALL','en_US.UTF-8') # This is preventative de-bugging 

# Def function to get all major subjects (>85%)
subject.major <- function(x){
  subject <- as.character(total$SUBJECT[x]) # take one row
  subjects <- unlist(strsplit(subject, ';\\s*')) # split on ';'
  subject.percents <- sub('.*\\((\\d+)%.*', '\\1', subjects) # list of just the percentages
  subject.percents <- as.list(subject.percents) # make a list
  subject.percents[(nchar(subject.percents) > 2)] <- NA # take out weirdo-s
  subject.no.percents <- sub('\\s\\((\\d+)%.', '\\1', subjects) # remove parantheses
  subject.no.percents <- sub('\\d+', '\\1', subject.no.percents) # list just the subject
  subject.percents.top <- subject.percents > 85 #take only the ones > 85
  subject.percents.top <- unlist(subject.percents.top) #unlist
  subjects.top <- subject.no.percents[subject.percents.top==TRUE] # take only subjects with perc > 85
  subjects.top <- subjects.top[!is.na(subjects.top)] # remove NAs
  subjects.top <- unlist(subjects.top)
  return(subjects.top)
}

subject.major(2)

# apply function to data
total$MAJOR_SUBJECT <- NA
row <- nrow(total)
n = seq(1:row)
total$MAJOR_SUBJECT[1:row] <- lapply(n,subject.major)

# run descriptive stats on it

subjects <- as.factor(unlist(total$MAJOR_SUBJECT))
subjects[2]
summary(subjects)

# Def function to take the top subject by percentage

subject.percentages <- function(x){
  subject <- as.character(total$SUBJECT[x])
  subjects <- unlist(strsplit(subject, ';\\s*'))
  subject.percents <- sub('.*\\((\\d+)%.*', '\\1', subjects)
  subject.percents <- as.list(subject.percents)
  subject.percents[(nchar(subject.percents) > 2)] <- NULL
  subject.percents <- unlist(subject.percents)
  subject <- grep(max(subject.percents), subjects, value=T)[1]
  subject.no.percents <- sub('\\s\\((\\d+)%.', '\\1', subject)
  subject <- sub('\\d+', '\\1', subject.no.percents)
  return(subject)
}

# apply function to data
total$TOP_SUBJECT <- NA
row <- nrow(total)
n = seq(1:row)
total$TOP_SUBJECT[1:row] <- lapply(n,subject.percentages)
total$TOP_SUBJECT <- as.factor(unlist(total$TOP_SUBJECT))

summary(total$TOP_SUBJECT)

############################
#### Analyzing Subjects ####
############################

total.violations <- subset(total,grepl("HUMAN RIGHTS VIOLATIONS",total$SUBJECT)) # only human rights violations

# test logic
total.all <- total
total <- total.violations
mena.subjects <- as.factor(unlist(total$MAJOR_SUBJECT[total$REGION=="MENA"]))
mena.subjects <- cbind(summary(mena.subjects)[1:50])

setwd("Results/subjects")

write.subjects <- function(region){
  x <- as.factor(unlist(total$MAJOR_SUBJECT[total$REGION==region]))
  x <- cbind(summary(x)[1:50])
  write.csv(x,file=(paste(tolower(region),"_subjects",".csv",sep="")))
}

write.subjects("MENA")
write.subjects("EECA")
write.subjects("LA")
write.subjects("Africa")
write.subjects("Asia")
write.subjects("West")

########################
#### Write new data ####
########################

setwd("~/Dropbox/berkeley/Git-Repos/human-rights-coverage/")
total <- total.all
total$TOP_SUBJECT <- as.character(total$TOP_SUBJECT)
total$MAJOR_SUBJECT <- as.character(total$MAJOR_SUBJECT)
write.csv(total, file="Data/New\ York\ Times/NYT.csv")

### This script plots human rights violations over time by region

# Load data (optional)
# total <- read.csv("Data/NYT.csv")

# Subsetting data

total.all <- total # retain all data
total.without.us <- subset(total,!grepl("united states",total$COUNTRY_FINAL,ignore.case=TRUE)) # without USA
total.violations <- subset(total,grepl("HUMAN RIGHTS VIOLATIONS",total$SUBJECT)) # only human rights violations
total.violations.no.us <- subset(total.without.us,grepl("HUMAN RIGHTS VIOLATIONS",total.without.us$SUBJECT)) # without USA, and only human rights violations

##### Compute total number of articles per year per country
total <- total.violations

counts <- data.frame(cbind(total$COUNTRY_CODE,total$YEAR))
names(counts) <- c("iso3c","year")
counts <- counts[-which(is.na(counts$iso3c)),]
counts <-  unique(counts)

# define function
country.per.year <- function(x,y){
  subset.data <- subset(total,COUNTRY_CODE==x & YEAR==y)
  return(as.integer(nrow(subset.data)))
}

counts$count <- unlist(lapply(counts$iso3c,country.per.year,y=counts$year))
write.csv(counts,"country_year_counts.csv")

##### Compute total number of articles per year per region

total <- total.violations.no.us #take out US for this one due to validity problems.

# define function
region.per.year <- function(x,y){
  subset.data <- subset(total,REGION==x & YEAR==y)
  return(as.integer(nrow(subset.data)))
}

# create dataframe
regions <- unique(total$REGION[!is.na(total$REGION)])
number.news <- data.frame(regions)

# fill in cells

start <- min(total$YEAR, na.rm=TRUE)
end <- max(total$YEAR, na.rm=TRUE)
for(i in seq(start,end)){
  number.news <- cbind(number.news,unlist(lapply(regions,region.per.year,y=i)))
}
names(number.news) <- c("regions",start:end)

write.csv(number.news,"region_year_counts.csv")

# Plot change in number over time

rownames(number.news) <- number.news$regions
number.news$regions
number.news <- number.news[, !(colnames(number.news) %in% c("regions"))]

x <- seq(1980,2010)
m <- number.news[3,]
l <- number.news[1,]
c <- number.news[5,]
a <- number.news[4,]
w <- number.news[2,]
f <- number.news[6,]

plot(x,m,
     xlab="year",
     ylab="number of articles in NYT", # Change this for your data
     main="Human Rights Violations Articles Over Time",
     type="l",
     col="red"
)
lines(x, l, type="l",col="green" )
lines(x, c, type="l",col="yellow" )
lines(x, a, type="l",col="blue" )
lines(x, w, type="l",col="orange" )
lines(x, f, type="l",col="purple" )

legend("topleft", c("Middle East", "Latin America", "Former Soviet Union","Asia","West","Africa"), col = c("red", "green","yellow","blue","orange","purple"),
       text.col = "black", lty = 1,
       merge = TRUE, bg = "gray90")


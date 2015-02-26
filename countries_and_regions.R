### This script will code articles by country based on (in order):
###      1) the highest percentage country in the GEOGRAPHY term
###      2) the title
### And then it will apply a region based on the country.

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage")

Sys.setlocale('LC_ALL','C') # This is preventative de-bugging 
## Load data (optional)

total <- read.csv("Data/New\ York\ Times/NYT.csv")
total$X <- NULL
total <- total.all

# This country code spreadsheet will help me categorize countries + regions
countries <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)

##########################
### Countries By TITLE ###
##########################

# initialize columns
total$COUNTRY_TITLE <- NA
total$COUNTRY_TITLE <- as.character(total$COUNTRY_TITLE)

# Define function for taking country in title
country.title <- function(x,y,z){
  country.index <- (grepl(x, z$TITLE,ignore.case=T))
  z$COUNTRY_TITLE[country.index] <-as.character(y)
  return(z$COUNTRY_TITLE)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  total$COUNTRY_TITLE <- country.title(countries$Key[i],countries$Value[i],total)
}

sum(is.na(total$COUNTRY_TITLE)) # 10203

###############################################
### Countries by LexisNexis GEOGRAPHIC term ###
###############################################

# initialize columns
total$COUNTRY_TOP_PERCENT <- NA # This column just takes the string with the top percent
total$COUNTRY_PERCENT_ST <- NA # This column will display the country in COUNTRY_TOP_PERCENT in standardized format

# Define function to take top-percentage country
country.percentages <- function(x){
  geo <- as.character(total$GEOGRAPHIC[x])
  countries <- unlist(strsplit(geo, ';\\s*'))
  country.percents <- sub('.*\\((\\d+)%.*', '\\1', countries)
  country.percents <- as.list(country.percents)
  country.percents[(nchar(country.percents) > 2)] <- NULL
  country.percents <- unlist(country.percents)
  country <- grep(max(country.percents), countries, value=T)[1]
  return(country)
}

# apply function to data
row <- nrow(total)
n = seq(1:row)
total$COUNTRY_TOP_PERCENT[1:row] <- lapply(n,country.percentages)
total$COUNTRY_TOP_PERCENT <- as.character(total$COUNTRY_TOP_PERCENT)

# Define function for turning value in COUNTRY_TOP_PERCENT into standardized format and putting it into COUNTRY_PERCENT_ST
country.percent <- function(x,y,z){
  country.index <- (grepl(x, z$COUNTRY_TOP_PERCENT,ignore.case=T))
  z$COUNTRY_PERCENT_ST[country.index] <- as.character(y)
  return(z$COUNTRY_PERCENT_ST)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in seq(1,n)){
  total$COUNTRY_PERCENT_ST <- country.percent(countries$Key[i],countries$Value[i],total)
}

sum(is.na(total$COUNTRY_PERCENT_ST)) # 2029

#######################
### Final Countries ###
#######################

# Takes COUNTRY_TITLE as priority, and then COUNTRY_PERCENT_ST
total$COUNTRY_FINAL <- NA
total$COUNTRY_FINAL <- total$COUNTRY_TITLE
na.index <- which(is.na(total$COUNTRY_FINAL))
total$COUNTRY_FINAL[na.index] <- total$COUNTRY_PERCENT_ST[na.index]

nrow(total[total$COUNTRY_FINAL=="United States of America",]) #7721

#####################
### Country Codes ###
#####################

total$COUNTRY_CODE <- NA

# Define function to get country code (ccode) from COUNTRY_FINAL and put it in column COUNTRY_CODE

country.code <- function(x,y,z){
  country.index <- (grepl(x, z$COUNTRY_FINAL,ignore.case=T))
  z$COUNTRY_CODE[country.index] <- as.character(y)
  return(z$COUNTRY_CODE)
}

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  total$COUNTRY_CODE <- country.code(countries$Key[i],countries$iso3c[i],total)
}

# Fix problematic codes
unique(total$COUNTRY_FINAL[is.na(total$COUNTRY_CODE)])

total$COUNTRY_CODE[total$COUNTRY_FINAL=="DRC"] <- "COD"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Macedonia" & total$YEAR < 1992] <- "MKD"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Macedonia" & total$YEAR > 1991] <- "MAC"

total$COUNTRY_CODE[total$COUNTRY_FINAL=="Yugoslavia" & total$YEAR < 2003] <- "MKD"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Yugoslavia" & total$YEAR > 2002] <- "YUG"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Yugoslavia" & total$YEAR > 2005] <- "SRB"

total$COUNTRY_CODE[total$COUNTRY_FINAL=="Serbia" & total$YEAR < 2003] <- "MKD"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Serbia" & total$YEAR > 2002] <- "YUG"
total$COUNTRY_CODE[total$COUNTRY_FINAL=="Serbia" & total$YEAR > 2005] <- "SRB"

total$COUNTRY_CODE[total$COUNTRY_FINAL=="Kosovo" & total$YEAR > 2007] <- "MNE"


#####################
### Apply Regions ###
#####################

### This method is from my country_codes.csv file. It only applies a region from ISO3c code, i.e. "COUNTRY_CODE".

total$REGION <- NA
for (i in 1:n){
  country <- as.character(countries$iso3c[i])
  total$REGION[total$COUNTRY_CODE==country]<-as.character(countries$Region[i])
}

unique(total$COUNTRY_FINAL[is.na(total$REGION)])

# Fixing problematic Regions
total$REGION[total$COUNTRY_CODE=="SRB"] <- "EECA"
total$REGION[total$COUNTRY_CODE=="MAC"] <- "EECA"
total$REGION[total$COUNTRY_CODE=="YUG"] <- "EECA"


## This method is from my original method. It applies regions directly from COUNTRY_FINAL, including cases with no specific country, i.e. "Balkans"

# Defining Regions

africa <- c("burundi","comoros","dijibouti","eritrea","ethiopia","kenya","madagascar","kenya","malawi","mauritius","mayotte","mozambique","reunion","rwanda","seqychelles","somalia","south sudan", "uganda","tanzania","united republic of tanzania","zambia","zimbabwe","angola","cameroon","central african republic","chad","congo","democratic republic of congo","equatorial guinea","gabon","sao tome and principe","botswana","lesotho","namibia","south africa","swaziland","benin","burkina faso","cabo verde","cote d'ivoire","gambia","ghana","guinea","gunea-bissau","liberia","mali","mauritania","niger","nigeria","saint helena","senegal","sierra leone","togo","cape verde","guinea-bissau","djibouti","democratic republic of the congo","ivory coast","africa","losotho","ziare")
africa <- paste(africa, collapse='|')

latin.america <- c("Caribbean", "anguilla","antigua and barbuda","aruba","bahamas","barbados","bonaire,saint eustatius and saba","british virgin islands","cuba","curacao","dominica","dominican republic","grenada","guadeloupe","haiti","jamaica","montserrat","puerto rico","saint-barthelemy","saint kitts and nevis","saint lucia","trinidad and tobago","trinidad","turks and calcos islands","united states virgin islands","belize","costa rica","el salvador","guatemala","honduras","mexico","nicaragua","panama","argentina", "bolivia","brazil","chile","colombia","ecuador","falkland islands","malvinas","french guiana","paraguay","peru","suriname","uruguay","venezuela","mexico","uruguay","st. kitts and nevis","seychelles","antigua & barbuda","st. vincent and the grenadines","st. lucia","guyana","uruguay","Samoa","latin america","south america","maldives","ST KITTS-NEVIS","ST LUCIA","ST VINCENT","fiji","central america")
latin.america <- paste(latin.america, collapse='|')

central.asia <- c("kazakhstan","kyrgyzstan","tajikistan","turkmenistan","uzbekistan","belarus","bulgaria","czech republic","hungary","poland","republic of moldova","romania","rumania","russian federation","slovakia","ukraine","albania","andorra","bosnia","croatia","gibralter","greece","holy see","malta","montenegro","san marino","serbia","slovenia","yugoslavia","hungary","czechoslovakia","macedonia","kosovo","moldova","russia","georgia","cyprus","azerbaijan","chechnya","soviet union","kosovo","yugoslavia","croatia","czech","serbia","soviet","estonia","ussr","u.s.s.r.","kazakstan","SLOVAK REPUBLIC")
central.asia <- paste(central.asia, collapse='|')

mena <- c("armenia","bahrain","iraq","jordan","kuwait","lebanon","oman","qatar","saudi arabia","palestine","state of palestine","syria","syrian arab republic","turkey","united arab emirates","yemen","algeria","egypt","libya","morocco","sudan","tunisia","western sahara","iran","afghanistan","yemen arab republic","yemen people's republic","turkish","palestine","palestinian","israel","middle east","mena")
mena <- paste(mena, collapse='|')


west <- c("channel islands","denmark","estonia","finland","iceland","isle of man","jersey","latvia","lithuania","norway","sark","svaldbard and jan mayen islands","sweden","united kingdom","united kingdom of great britain and northern ireland","canada","japan", "australia", "new zealand","ireland","netherlands","belgium","luxembourg","france","monaco","liechtenstein","switzerland","spain","italy","andorra","portugal","germany","german federal republic","german democratic republic","austria-hungary","austria","papal states","irish","Europe","england","west","Nordic Countries", "United States of America")
west <- paste(west, collapse='|')

asia <- c("cambodia","china","japan","sri lanka","vietnam","india","pakistan","bangladesh","malaysia","indonesia","korea","laos","burma","myanmar","philippines","korea","thailand","TIMOR-LESTE","timor","tibet","bhutan","nepal","Singapore","asia","viet nam","samoa","taiwan","kashmir","hong kong","mongolia","Mariana islands","Solomon Islands","MACAO","guam")
asia <- paste(asia,collapse='|')

# Appying regions based on COUNTRY_FINAL

total$REGION2 <- NA

asia.index <- (grepl(asia, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[asia.index] <- "Asia"

la.index <- (grepl(latin.america, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[la.index] <- "LA"

mena.index <- (grepl(mena, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[mena.index] <- "MENA"

africa.index <- (grepl(africa, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[africa.index] <- "Africa"

ca.index <- (grepl(central.asia, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[ca.index] <- "EECA"

west.index <- (grepl(west, total$COUNTRY_FINAL,ignore.case=T))
total$REGION2[west.index] <- "West"

total$REGION2 <- as.character(total$REGION)

####
write.csv(total, file="Data/New\ York\ Times/NYT.csv")

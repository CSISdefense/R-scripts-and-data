##############################################################################
# GOOGLE ANALYTICS DATA PROCESSING - Data prep for google analytics plots app
#  
# Reads 'GAnalytics All Numbers Sheet.xlsx' and outputs 'GAplotsdata.csv'
# which is used by the google analytics plots app.
#
# Intended to run one time only, after updating GAnalytics All Numbers Sheet.
#
# September 2016 
# L. Lipsey for DIIG
##############################################################################


##############################################################################
# 1. Load Packages
##############################################################################

library(dplyr) # wrangle data
library(openxlsx) # read excel file

##############################################################################
# 2. Read in Data
##############################################################################

RawData <- read.xlsx(
"K:/R-Shiny/4.Google Analytics/2.Box.Scatter Plots/GAnaltics All Numbers Sheet.xlsx",
  sheet = "Raw Report Info", detectDates = TRUE, skipEmptyRows = TRUE)

##############################################################################
# 3. Process Data
##############################################################################

# create new data frame with only the columns we want
# and rename columns to shorter/easier names
GAData <- select(RawData,
                  Title, Type, Date, Weekday, Author = contains('Author(s)'),
                  Program, Views = contains('Total.Views'), Event,
                  Twitter, Facebook, Evening, MailOpens = contains('Mail-Opens'),
                  MailClicks = contains('Mail-Clicks'),
                  Block1 = contains('Block.1.Used?'),
                  Block234 = contains('Bck.2-4.Used?'))

# data didn't read in that cleanly; drop anything with no title
GAData <- filter(GAData, is.na(Title) == FALSE)

# convert Excel date format to R date format
# http://stackoverflow.com/questions/19172632/
# converting-excel-datetime-serial-number-to-r-datetime
GAData$Date <- as.Date(GAData$Date, origin = '1899-12-30')

# construct a binary variable, 1 if it's from ISP and 0 if it's not
# grepl is a true/false check whether a string contains a pattern of text
# lapply applies a command to each element of a vector
GAData$fromISP <- lapply(GAData$Program, grepl, pattern ="ISP")

# change fromISP to character to give it more informative values
GAData$ISP <- ifelse(GAData$fromISP, 'ISP', 'non-ISP')

# change Facebook to a binary variable
GAData$Facebook <- as.factor(ifelse(GAData$Facebook == 0, 'No', 'Yes'))

# create block variable in a really kludgey way from block1 and block234
GAData$Block <- ifelse(GAData$Block1 == 'Yes', 'One',
                       ifelse(GAData$Block234 == 'Yes', 'Other', 'None'))

# add unknown for block field for reports prior to July 15
GAData$Block[GAData$Date < '2016-07-15'] <- 'Unknown'

# change Facebook to a binary variable
GAData$Facebook <- as.factor(ifelse(GAData$Facebook == 0, 'No', 'Yes'))

# drop unused columns
GAData <- select(GAData, -fromISP, -Block1, -Block234)

##############################################################################
# 4. Write CSV
##############################################################################

write.csv(GAData, file =
  "K:/R-Shiny/4.Google Analytics/2.Box.Scatter Plots/GAplotsdata.csv")

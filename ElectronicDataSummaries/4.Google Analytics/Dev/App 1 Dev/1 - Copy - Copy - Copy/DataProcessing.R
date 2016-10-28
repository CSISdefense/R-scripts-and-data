setwd("K:\\R-Shiny\\Google Analytics")
library(plyr)
library(tidyr)

#************************************************************#

google <- read.csv("Google2.csv")
str(google)
head(google)

colnames(google) <- c("Category", "Report", 2001:2014)

google <- gather(google, key = "Day", value = Amount, `2001`:`2014`)

google$Day <- as.integer(google$Day)

write.csv(google, "Google3.csv")

str(google) 


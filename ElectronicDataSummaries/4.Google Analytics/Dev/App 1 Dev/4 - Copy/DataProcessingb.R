setwd("K:\\R-Shiny\\Google Analytics\\4 - Copy")
library(plyr)
library(tidyr)

#************************************************************#

google <- read.csv("Google1b.csv")
str(google)
head(google)

colnames(google) <- c("Category", "Report", 2001:2014)

google <- gather(google, key = "Day", value = Amount, `2001`:`2014`)

google$Day <- as.integer(google$Day)

write.csv(google, "Google3b.csv")

str(google) 


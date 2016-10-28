setwd("K:\\R-Shiny\\Google Correlate\\A")
google <- read.csv("GoogleA.csv")
colnames(google) <- c("Report.Name", "Day", "Author", "Program", "Program1", 
                      "Evening", "Twitter", "Pages", "Downloads", "Amount")
str(google) 
write.csv(google, "GoogleA2.csv")

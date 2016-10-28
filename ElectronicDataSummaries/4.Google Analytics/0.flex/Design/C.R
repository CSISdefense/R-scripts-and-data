##############################################################################
# PAGE VIEWS BOXPLOTS - Google Analytics display app for DIIG 
# September 2016 
# L. Lipsey / G. Coll
##############################################################################


##############################################################################
# 1. Load Packages
##############################################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(Cairo)


##############################################################################
# 2. Set up the user interface for the app
##############################################################################


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Boxplot Comparison",
                 
                 selectInput("cat", "",
                             c("Weekday","Block", "Type", "ISP"),
                             selected = "Weekday")
                 #          ,
                 
                 
                 # selectInput("include", "Data",
                 #             c("ISP only","non-ISP only","All"),
                 #             selected = "All",
                 #             selectize = FALSE,
                 #             size = 3)
    ),
    
    mainPanel(
      plotOutput("plot")
      
    )
  )
)

##############################################################################
# 3. Read in data
##############################################################################

server <- function(input, output, session){
  
# set file to the current excel file
file <- "9-15-16 GAnaltics All Numbers Sheet.xlsx"
  
# read data from the second sheet of the excel file
GAdata <- read_excel(file, sheet = 2)
  
##############################################################################
# 4. Process data
#       This should be broken out into a seperate data processing script
#       if we are trying to optimize loading speed, but for now it's
#       fine here
##############################################################################

# data didn't read in that cleanly; drop anything with no title
GAdata <- filter(GAdata, is.na(Title) == FALSE)

# Subset to only the variables we care about and give some shorter names
#
### Selecting columns by name instead of number is clunky and long, but seems
### necessary to avoid breaking the app when someone adds more columns to the
### excel file.
###
### The 'contains' function is a work-around to select a column by name when
### the name contains spaces or non-alphanumeric characters.
GAdata <- select(GAdata,
                 Title, Type, Date, Weekday, Author = contains('Author(s)'),
                 Program, Views = contains('Total Views'), Event,
                 Twitter, Facebook, Evening, MailOpens = contains('Mail-Opens'),
                 MailClicks = contains('Mail-Clicks'),
                 Block1 = contains('Block 1 Used?'),
                 Block234 = contains('Bck 2-4 Used?'))

# construct a binary variable, 1 if it's from ISP and 0 if it's not
#
### this command uses lapply to apply the grepl function over each row of the 
### data
###
### grepl searches a character string for a pattern and returns TRUE if
### it finds the pattern
GAdata$fromISP <- lapply(as.character(GAdata$Program), grepl, pattern ="ISP")

# convert it to a factor variable so ggplot can use it for grouping
GAdata$fromISP <- as.factor(as.character(GAdata$fromISP))

# convert other variables to factors too
GAdata$Weekday <- as.factor(GAdata$Weekday)
GAdata$Type <- as.factor(GAdata$Type)

# create block variable in a really kludgey way from block1 and block234
GAdata$Block <- ifelse(GAdata$Block1 == 'Yes', 'One',
                       ifelse(GAdata$Block234 == 'Yes', 'Other', 'None'))

# and convert that to a factor too
GAdata$Block <- as.factor(GAdata$Block)

##############################################################################
# Build the plot based on user input
##############################################################################

plotsettings <- reactive({
  
  
  
p <- switch(input$cat, 
              Weekday = ggplot(data = GAdata, aes(x = Weekday, y = Views)), 
              Block = ggplot(data = GAdata, aes(x = Block, y = Views)), 
              Type = ggplot(data = GAdata, aes(x = Type, y = Views)),
              ISP = ggplot(data = GAdata, aes(x = fromISP, y = Views))
                 
  )  
  

p <- p + geom_boxplot() + geom_point(alpha = 0.5)
  
# p <- switch(input$cat,
#               'Weekday' = {
#                 p +
#                   geom_point(aes(color = fromISP, size = fromISP),
#                              alpha = 0.3) + 
#                   theme_bw() +
#                   scale_size_discrete(range = c(2,4)) +
#                   scale_color_manual(values = c("black","red")) +
#                   stat_smooth(method = "lm", se= FALSE, color = "gray60") +
#                   theme(legend.position = "bottom")
#               },
#               'Block' = {
#                 p +
#                   geom_point(aes(color = fromISP), size = 4,
#                              alpha = 0.3) + 
#                   theme_bw() +
#                   stat_smooth(method = "lm", se= FALSE, color = "red") +
#                   scale_color_manual(values = "red") +
#                   theme(legend.position = "bottom")
#               },
#               'Type' = {
#                 p +
#                   geom_point(aes(color = fromISP), size = 2,
#                              alpha = 0.3) + 
#                   theme_bw() +
#                   stat_smooth(method = "lm", se= FALSE, color = "black") +
#                   scale_color_manual(values = "black") +
#                   theme(legend.position = "bottom")
#               }
# )
  
  
  
p
  
})


##############################################################################
# Output the built plot and start the app
##############################################################################

output$plot <- renderPlot({
  plotsettings()
}, height = 600) 



}

shinyApp(ui= ui, server = server)
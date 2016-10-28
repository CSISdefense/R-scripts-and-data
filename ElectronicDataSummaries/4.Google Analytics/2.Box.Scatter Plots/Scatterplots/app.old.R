##############################################################################
# PAGE VIEWS SCATTERPLOT - Google Analytics display app for DIIG 
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
    sidebarPanel("Scatterplot Comparison",
      
      selectInput("xvar", "ER action",
                  c("Twitter","Evening", "MailOpens", "MailClicks"),
                  selected = "Twitter"),
      
      
      selectInput("include", "Data",
                    c("ISP only","non-ISP only","All"),
                    selected = "All",
                  selectize = FALSE,
                  size = 3),
      br(),
      br(),
      textOutput("stats")
    ),
    
    mainPanel(
      plotOutput("plot", hover = hoverOpts(id = "plot_hover"),
                 click = clickOpts(id = "plot_click")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      verbatimTextOutput("hover_info"),
      htmlOutput("click_info")
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
GAdata$Weekday <- factor(GAdata$Weekday, levels = c('Monday','Tuesday',
                                                    'Wednesday','Thursday','Friday','Saturday'))
GAdata$Type <- as.factor(GAdata$Type)

# create block variable in a really kludgey way from block1 and block234
GAdata$Block <- ifelse(GAdata$Block1 == 'Yes', 'One',
                       ifelse(GAdata$Block234 == 'Yes', 'Other', 'None'))

# convert that to a factor too
GAdata$Block <- ordered(GAdata$Block, 
                        levels = c('One','Other','None','Unknown'))

# convert Date to date class
GAdata$Date <- as.Date(GAdata$Date)

# add unknown for block field for reports prior to July 15
GAdata$Block[GAdata$Date < '2016-07-15'] <- 'Unknown'

# create a binary variable for whether a report was mentioned on Facebook
GAdata$Facebooked <- as.factor(ifelse(GAdata$Facebook == 0, 'No', 'Yes'))

##############################################################################
# 4a. Subset data based on user input
##############################################################################

dataset <- reactive({
  switch(input$include,
            'ISP only' = {filter(GAdata, fromISP == "TRUE")},
            'non-ISP only' = {filter(GAdata, fromISP == "FALSE")},
            'All' = GAdata
  )
    
})  

##############################################################################
# Build the plot based on user input
##############################################################################

plotsettings <- reactive({



p <- switch(input$xvar, 
 Twitter = {ggplot(data = dataset(), aes(x = Twitter, y = Views)) +
     coord_cartesian(xlim = c(0,max(GAdata$Twitter)),
                     ylim = c(0,max(GAdata$Views))) +
   ylab("Page Views")},
 Evening = {ggplot(data = dataset(), aes(x = Evening, y = Views)) +
     coord_cartesian(xlim = c(0,max(GAdata$Evening)),
                     ylim = c(0,max(GAdata$Views))) +
   ylab("Page Views")},
 MailOpens = {ggplot(data = dataset(), aes(x = MailOpens, y = Views)) +
     coord_cartesian(xlim = c(0,max(GAdata$MailOpens)),
                     ylim = c(0,max(GAdata$Views))) +
     ylab("Page Views")}, 
 MailClicks = {ggplot(data = dataset(), aes(x = MailClicks, y = Views)) +
     coord_cartesian(xlim = c(0,max(GAdata$MailClicks)),
                     ylim = c(0,max(GAdata$Views))) +
     ylab("Page Views")}
 )  


p <- switch(input$include,
  'All' = {
    p +
      geom_point(aes(color = fromISP, size = fromISP),
                  alpha = 0.3) + 
      theme_bw() +
      scale_size_discrete(range = c(2,4)) +
      scale_color_manual(values = c("black","red")) +
      stat_smooth(method = "lm", se= FALSE, color = "gray60") +
      theme(legend.position = "bottom")
  },
  'ISP only' = {
    p +
      geom_point(aes(color = fromISP), size = 4,
                  alpha = 0.3) + 
      theme_bw() +
      stat_smooth(method = "lm", se= FALSE, color = "red") +
      scale_color_manual(values = "red") +
      theme(legend.position = "bottom")
  },
  'non-ISP only' = {
    p +
      geom_point(aes(color = fromISP), size = 2,
                  alpha = 0.3) + 
      theme_bw() +
      stat_smooth(method = "lm", se= FALSE, color = "black") +
      scale_color_manual(values = "black") +
      theme(legend.position = "bottom")
  }
)
  
 

p
  
})


##############################################################################
# Output the built plot and start the app
##############################################################################

output$plot <- renderPlot({
  plotsettings()
}, height = 600) 

# runs the hover function
output$hover_info <- renderPrint({
  if(!is.null(input$plot_hover)){
    hover <- input$plot_hover
    hovdata <- dataset()
    dist <- switch(input$xvar,
           MailOpens = {sqrt((hover$x - hovdata$MailOpens)^2 + 
                                       (hover$y - hovdata$Views)^2)}, 
           MailClicks = {sqrt(((hover$x - hovdata$MailClicks)*5)^2 + 
                                        (hover$y - hovdata$Views)^2)}, 
           Twitter = {sqrt(((hover$x - hovdata$Twitter)*400)^2 + 
                     (hover$y - hovdata$Views)^2)},
           Evening = {sqrt(((hover$x - hovdata$Evening)*1000)^2 + 
                             (hover$y - hovdata$Views)^2)}
    )
    cat("Report:\n")
    if(min(dist) < 250)
      hovdata$Title[which.min(dist)]
  }
  
})

# runs the click function
output$click_info <- renderUI({
  if(!is.null(input$plot_click)){
    click <- input$plot_click
    clickdata <- dataset()
    dist <- switch(input$xvar,
                   MailOpens = {sqrt((click$x - clickdata$MailOpens)^2 + 
                                       (click$y - clickdata$Views)^2)}, 
                   MailClicks = {sqrt(((click$x - clickdata$MailClicks)*5)^2 + 
                                        (click$y - clickdata$Views)^2)}, 
                   Twitter = {sqrt(((click$x - clickdata$Twitter)*400)^2 + 
                                     (click$y - clickdata$Views)^2)},
                   Evening = {sqrt(((click$x - clickdata$Evening)*1000)^2 + 
                                     (click$y - clickdata$Views)^2)}
    )
    cat("Report:\n")
    if(min(dist) < 250)
      HTML(paste("<i>Author:</i> ", clickdata$Author[which.min(dist)], '<br/>',
                 "<i>Title:</i> ",clickdata$Title[which.min(dist)], '<br/>',
                 "<i>Program:</i> ",clickdata$Program[which.min(dist)], 
                 '&nbsp &nbsp &nbsp &nbsp &nbsp',
                 "<i>Date:</i> ",clickdata$Date[which.min(dist)], '<br/>',
                 "<i>Views: </i> ",clickdata$Views[which.min(dist)],
                 '&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp',
                 "<i>Twitter: </i>",clickdata$Twitter[which.min(dist)], '<br/>',
                 "<i>Evening: </i>",clickdata$Evening[which.min(dist)],
                 '&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp',
                 "<i>Facebook: </i>",clickdata$Facebooked[which.min(dist)], '<br/>',
                 "<i>MailOpens: </i>",clickdata$MailOpens[which.min(dist)],
                 '&nbsp &nbsp &nbsp &nbsp &nbsp',
                 "<i>MailClicks: </i> ",clickdata$MailClicks[which.min(dist)], '<br/>',
                 "<i>Block: </i>",clickdata$Block[which.min(dist)],
                 '&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp',
                 "<i>Type: </i>",clickdata$Type[which.min(dist)]
      )) 
  }
  
})

# runs the equation textbox
output$stats <- renderPrint({
  statdata <- dataset()
  model <- switch(input$xvar,
                  MailOpens = lm(statdata$Views ~ statdata$MailOpens),
                  MailClicks = lm(statdata$Views ~ statdata$MailClicks),
                  Twitter = lm(statdata$Views ~ statdata$Twitter),
                  Evening = lm(statdata$Views ~ statdata$Evening)
                    )
  cat("Views = ", round(model$coefficients[1])," + ",
      round(model$coefficients[2],1), "*", input$xvar, sep = "")
})

}

shinyApp(ui= ui, server = server)
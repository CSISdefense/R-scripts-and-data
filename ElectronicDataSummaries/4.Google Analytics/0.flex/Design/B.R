##############################################################################
# Google Analytics display app for DIIG 
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
                  size = 3)
    ),
    
    mainPanel(
      plotOutput("plot", hover = hoverOpts(id = "plot_hover")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      verbatimTextOutput("hover_info")
    )
  )
)



##############################################################################
# 3. Read in data
##############################################################################

server <- function(input, output, session){


# set file to the current excel file
file <- "9-13-16 GAnaltics All Numbers Sheet2.xlsx"

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

# subset to only the variables we care about and give some shorter names
GAdata <- select(GAdata,1:3, Author = 4, 5, Views = 6,
                  9, 11, MailOpens = 12, MailClicks = 13,
                  Block1 = 16, Block234 = 18)

# construct a binary variable, 1 if it's from ISP and 0 if it's not
# this command uses lapply to apply the grepl function over each row of the 
# data; grepl searches a character string for a pattern and returns TRUE if
# it finds the pattern
GAdata$fromISP <- lapply(as.character(GAdata$Program), grepl, pattern ="ISP")

# convert it to a factor variable so ggplot can use it for grouping
GAdata$fromISP <- as.factor(as.character(GAdata$fromISP))


##############################################################################
# 4a. Subset data based on user input
##############################################################################

dataset <- reactive({
  shown <- switch(input$include,
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

output$hover_info <- renderPrint({
  if(!is.null(input$plot_hover)){
    hover <- input$plot_hover
    hovdata <- dataset()
    dist <- switch(input$xvar,
           Twitter = {sqrt(((hover$x - hovdata$Twitter)*100)^2 + 
                     (hover$y - hovdata$Views)^2)},
           Evening = {sqrt(((hover$x - hovdata$Evening)*100)^2 + 
                             (hover$y - hovdata$Views)^2)}, 
           MailOpens = {sqrt(((hover$x - hovdata$MailOpens)*100)^2 + 
                             (hover$y - hovdata$Views)^2)}, 
           MailClicks = {sqrt(((hover$x - hovdata$MailClicks)*100)^2 + 
                               (hover$y - hovdata$Views)^2)}
    )
    cat("Report:\n")
    if(min(dist) < 30)
      hovdata$Title[which.min(dist)]
  }
  
})


}

shinyApp(ui= ui, server = server)
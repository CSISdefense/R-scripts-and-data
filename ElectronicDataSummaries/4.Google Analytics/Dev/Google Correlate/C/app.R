################################################################################
#Gcoll - Google Analytics 
################################################################################

require(shiny)
require(ggplot2)
require(plyr)
require(dplyr)
require(scales)
require(plotly)
require(grid)
require(gridExtra)
require(shinythemes)

################################################################################
# Visual settings for user interface
################################################################################

data <- read.csv("GoogleA2.csv")

Report.Name <- levels(data$Report.Name)
Author <- levels(data$Author)
Program <- levels(data$Program)
Program1 <- levels(data$Program1)

# here's the ui section - visual settings for the plot + widgets
ui <- fluidPage(
      fluidRow(
          
          # left column - column sizes should add up to 12, this one is 3 so
          # the other one will be 9
          column(3, align = 'center',
                  br(),
                 
                 selectizeInput("Author","Author", Author,
                             multiple = TRUE,
                             selected = "Hicks",
                             width = '100%',
                             size = 18)
            ),
            
          # left column - column sizes should add up to 12, this one is 9 so
          # the other one will be 3 
            column(9, align = "center",
                  plotOutput("plot"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")
            )))

            
      
      
# end of ui section



# server function starts

server <- function(input, output, session){

################################################################################
# Read in and clean up data
################################################################################      

# read in data            
FullData <- read.csv("GoogleA2.csv")

# save FY as 2 digits instead of 4, for better visual scale
FullData$Day <- factor(substring(as.character(FullData$Day), 3, 4))

updateSelectizeInput(session, "Author", choices = Author, 
                     selected = "Hicks", server = TRUE)

################################################################################
# Subset data based on user input
################################################################################

dataset <- reactive({
    
    ## subset by year, based on year slider ##
    # findInterval is a confusing (but supposedly faster-running) way to do this
    # that I found on google.  Probably a normal conditional test would be fine.
  shown <- filter(FullData,
                  findInterval((as.numeric(as.character(Day)))+2001,
                               c(2001, 2014)) == 1L)
      
      ## subset data based on which categories the user selected ##
      
      # the selectInput widget holds the selected choices as a vector of
      # strings. This code checks whether the each observation is in the
      # selected categories, and discards it if isn't in all three.  The %in%
      # operator is a nice way to avoid typing lots of conditional tests all
      # strung together 
      shown <- filter(shown, Author %in% input$Author)
      
      # aggregate rows by summing Amount.  The only breakouts left will be the
      # ones in the .(  ) call - FY and VendorSize in this case
      shown <- ddply(shown, .(Twitter, Program), summarize, Amount = sum(Amount))
      
      
      # calculate percent of obligations for each VendorSize category
      #shown <- ddply(shown, .(Day),
      #               function(x){
      #                  x$Percent <- x$Amount / sum(x$Amount, na.rm = TRUE)
      #                  x
      #                  })
      
      # return the subsetted dataframe to whatever called dataset()
      shown

# end of dataset() function      
})

################################################################################
# Set colors  
################################################################################
colorset <- 
  c(
    #Set Category Colors 
    "ISP" = "#CE884E", 
    "Non-ISP" = "#63c5b8")

#Set Category Colors 
#"Products" = "#554449", 
#"Services" = "#CE884E", 
#"R&D" = "#63c5b8")

DIIGcolors <- scale_color_manual(values = colorset, name = NULL)

################################################################################
# Build the plot for output
################################################################################

plotsettings <- reactive({
  p <- ggplot(data = dataset(),
              aes(x=Twitter, y=Amount, 
                  color=Program, group=Program, fill =Program)) +
    geom_jitter(size = 1.5) +
    expand_limits(y=0) + 
    
    #coll: Added title 
    ggtitle("Google Correlate") + 
    theme(plot.title = element_text(
      family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,30,0))) + 
        
    #coll: Custom background color/layout 
    theme(panel.border=element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(size=.1, color="grey80"), 
          panel.grid.minor.y = element_line(size=.1, color="grey80")) + 
    
    #scale_y_continuous(labels=percent) +
    
    DIIGcolors+
    
    theme(legend.text = element_text(size = 18, color="#554449")) +
    theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
    theme(legend.key = element_rect(fill="white")) +
    theme(legend.key.width = unit(3,"line")) +
    theme(axis.text.x = element_text(size = 14, color="#554449", margin=margin(-10,0,0,0))) +
    theme(axis.ticks.length = unit(.00, "cm")) +
    theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
    theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
    theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
    xlab("Evening") +
    ylab("Page Views") + 
    
    grid.newpage() 
  footnote <- "Source: FPDS; CSIS analysis"
  g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12, col = "#554449")))
  grid.draw(g) +
  
  p 
  
  ggplotly()
})


################################################################################
# Run download buttons
################################################################################

# run csv download button
output$CSVDownloadBtn <- downloadHandler(
  filename = paste('CSIS.Contract Obligations by Area.', Sys.Date(),'.csv', sep=''),
  content = function(file) {
    writedata <- dataset()
    writedata$Day <- as.numeric(as.character(writedata$Day)) + 2000
    writedata$Percent <- writedata$Percent * 100
    writedata <- select(writedata, FY, Category, Amount, Percent)
    write.csv(writedata, file)
  }
)



################################################################################
# Output the built plot and start the app
################################################################################


output$plot <- renderPlot({
  plotsettings()
}, height = 600) 



}

# starts the app
shinyApp(ui= ui, server = server)

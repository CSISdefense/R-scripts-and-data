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

data <- read.csv("Google3b.csv")

Category <- levels(data$Category)
#Category <- c(Category)

Report <- levels(data$Report)
#Report <- c(Report)

Day <- levels(data$Day)
#Day <- c(Day)

# here's the ui section - visual settings for the plot + widgets
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("

        body{ 
        background-color: #4D6685;}
                    
                    "))
  ),
      fluidRow(
          
            column(12, align = "center",
                   br(), 
                  plotOutput("plot"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"),
                  br(), 
                  br(), 
                  br(), 
                  br(), 
                  br(), 
                  br(),
                  br(), 
                  br(), 
                  br(),
                  br(), 
                  br())), 
      fluidRow(
        
            #column(1), 
            column(12, align = "center", 
        
                  selectizeInput("Report","Report", Report,
                                 multiple = FALSE,
                                 #selectize = FALSE,
                                 selected = "Undersea Warfare in Northern Europe",
                                 width = '33%',
                                 size = 39)) 
            #column(8)
            )  
            )

            
      
      
# end of ui section



# server function starts

server <- function(input, output, session){

################################################################################
# Read in and clean up data
################################################################################      

# read in data            
FullData <- read.csv("Google3b.csv")

# save FY as 2 digits instead of 4, for better visual scale
FullData$Day <- factor(substring(as.character(FullData$Day), 3, 4))

updateSelectizeInput(session, "Report", choices = Report, 
                     selected = "Undersea Warfare in Northern Europe", server = TRUE)

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
      shown <- filter(shown, Report %in% input$Report)
      
      # aggregate rows by summing Amount.  The only breakouts left will be the
      # ones in the .(  ) call - FY and VendorSize in this case
      shown <- ddply(shown, .(Day, Category), summarize, Amount = sum(Amount))
      
      
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
    "ISP Short Writings" = "#CE884E", 
    "Major CSIS Reports" = "#63c5b8", 
    "Major ISP Reports" = "#628582", 
    "Report Views " = "#C74F4F")

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
              aes(x=Day, y=Amount, 
                  color=Category, group=Category, fill =Category)) +
    geom_line(size = 1.5) +
    expand_limits(y=0) + 
    
    #coll: Added title 
    ggtitle("Page Views after Publication") + 
    theme(plot.title = element_text(
      family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,30,0))) + 
        
    #coll: Custom background color/layout 
    theme(panel.background = element_rect(fill = "#4D6685")) + 
    theme(plot.background = element_rect(fill = "#4D6685", color = "#4D6685")) + 
#    theme(panel.border = element_rect(fill = "#4D6685")) + 
          theme(
            panel.grid.major.x = element_line(size=.25, color="white", linetype = "dotted"), 
            panel.grid.minor.x = element_line(size=.25, color="white", linetype = "dotted"),  
            panel.grid.major.y = element_line(size=.25, color="white", linetype = "dotted"), 
            panel.grid.minor.y = element_line(size=.25, color="white", linetype = "dotted")) + 
    theme(plot.title = element_text(
      family = "Arial", color = "white", size = 26, face="bold", margin=margin(0,0,0,0))) +
    theme(axis.text.x = element_text(
      size = 15, family = "Arial", color = "white", vjust=7, margin=margin(0,0,0,0))) +
    theme(axis.text.y = element_text(
      size = 15, family = "Arial", color ="white", margin=margin(0,0,0,0))) +
    theme(axis.title.x = element_text(
      size = 16, face = "bold", color = "white", family = "Arial",
      margin=margin(0,0,0,0))) +
    theme(axis.title.y = element_text(
      size = 16, face = "bold", color = "white", family = "Arial",
      margin=margin(0,15,0,0))) +
    theme(axis.ticks.x = element_blank()) + 
    theme(axis.ticks.y = element_blank()) + 
    theme(legend.text = element_text(size = 15, family = "Arial", color ="white")) +
    theme(legend.position = 'bottom') +
    theme(legend.background = element_rect(fill = "#4D6685")) + 
    theme(legend.key = element_rect(fill = "#4D6685", colour ="#4D6685")) + 
    theme(legend.title = element_blank()) + 
    
    #scale_y_continuous(labels=percent) +
    
    DIIGcolors+
    

    theme(legend.key.width = unit(3,"line")) +

    xlab("Days") +
    ylab("Page Views")  
    
#    grid.newpage() 
#  footnote <- "Source: FPDS; CSIS analysis"
#  g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12, col = "white")))
#  grid.draw(g) +
  
  p 
  
#  ggplotly()
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

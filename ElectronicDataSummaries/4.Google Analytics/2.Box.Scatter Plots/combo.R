##############################################################################
# PAGE VIEWS BOX AND SCATTER - Google Analytics display app for DIIG 
# 
# Requires: GAplotsdata.csv (produced by GA Plots Data Processing.R) in folder
#
# September 2016 
# L. Lipsey / G. Coll
##############################################################################


##############################################################################
# 1. Load Packages
##############################################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(Cairo)


##############################################################################
# 2. Set up the user interface for the app
##############################################################################


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
                 titlePanel("Google Analytics Comparisons"),
                 
                 selectInput("cat", "Page Views by",
                             c("Weekday","Block", "Type", "Facebook",
                               "Twitter","Evening", "MailOpens", "MailClicks"),
                             selected = "Twitter"),
                 
                 
                 
                 selectInput("include", "Data",
                             c("ISP only","non-ISP only","All"),
                             selected = "All",
                             selectize = FALSE,
                             size = 3),
                 br(),
                 h5("Hover over a point for the report title", align = "center"),
                 h5("Click a point for report details", align = "center"),
                 h5("Highlight an area to zoom in", align = "center"),
                 h5("Double click to zoom out", align = "center")
    ),
    
    mainPanel(
      plotOutput("plot", 
                 hover = hoverOpts(id = "plot_hover"),
                 dblclick = "plot_doubleclick",
                 click = clickOpts(id = "plot_click"),
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE,
                                   delay = 700)),
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
  
# Read the CSV
GAdata <- read.csv("GAplotsdata.csv")

# Adjust classes and the order of factor levels after reading CSV -
# this can't be done in Data Processing script.
GAdata$ISP <- factor(GAdata$ISP, levels = c("non-ISP","ISP"))
GAdata$Date <- as.Date(GAdata$Date)
GAdata$Weekday <- factor(GAdata$Weekday, levels =
                           c("Monday", "Tuesday","Wednesday","Thursday",
                             "Friday","Saturday", "Sunday"))
GAdata$Block <- factor(GAdata$Block, 
                       levels = c("One","Other","None","Unknown"))
GAdata$Type <- factor(GAdata$Type, 
                      levels = c("Report","Commentary","Critical Questions"))

# for point highlighting
GAdata$symbol <- rep("19", length(GAdata$Title))

##############################################################################
# 4. Subset data based on user input
##############################################################################

dataset <- reactive({
  switch(input$include,
         'ISP only' = {filter(GAdata, ISP == "ISP")},
         'non-ISP only' = {filter(GAdata, ISP == "non-ISP")},
         'All' = GAdata
  )
  
})  


##############################################################################
# 5. Build the plot based on user input
##############################################################################

plotsettings <- reactive({

##############################################################################    
# 5a. settings that differ depending on the x variable the user picks
# (i.e. Twitter, Weekend, Block, etc.) 
##############################################################################
  p <- switch(input$cat, 
        Weekday = {ggplot(data = dataset(), aes(x = Weekday, y = Views,
                                                shape = symbol)) +
          #coord_cartesian(ylim = c(0,max(GAdata$Views))) +
          scale_x_discrete(drop = FALSE) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Block = {ggplot(data = dataset(), aes(x = Block, y = Views)) +
          #coord_cartesian(ylim = c(0,max(GAdata$Views))) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")}, 
        
        Type = {ggplot(data = dataset(), aes(x = Type, y = Views)) +
          #coord_cartesian(ylim = c(0,max(GAdata$Views))) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Facebook = {ggplot(data = dataset(), aes(x = Facebook,
                                                 y = Views)) +
          #coord_cartesian(ylim = c(0,max(GAdata$Views))) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Twitter = {ggplot(data = dataset(), aes(x = Twitter, y = Views)) +
          #coord_cartesian(xlim = c(0,max(GAdata$Twitter)),
          #                ylim = c(0,max(GAdata$Views))) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")},
        
        Evening = {ggplot(data = dataset(), aes(x = Evening, y = Views)) +
          #coord_cartesian(xlim = c(0,max(GAdata$Evening)),
          #                ylim = c(0,max(GAdata$Views))) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")},
        
        MailOpens = {ggplot(data = dataset(), aes(x = MailOpens, y = Views)) +
          #coord_cartesian(xlim = c(0,max(GAdata$MailOpens)),
          #                ylim = c(0,max(GAdata$Views))) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")}, 
        
        MailClicks = {ggplot(data = dataset(), aes(x = MailClicks, y = Views)) +
          #coord_cartesian(xlim = c(0,max(GAdata$MailClicks)),
          #                ylim = c(0,max(GAdata$Views))) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")}
           
  )  

#############################################################################
# 5b. settings that differ depending on what data the user wants included
# (i.e. ISP only, non-ISP only, All)  
##############################################################################
  p <- switch(input$include,
            'All' = {
              p +
                scale_size_discrete(range = c(2,5)) +
                scale_color_manual(values = c("#554449","#628582"))  
            },
            'ISP only' = {
              p +
                scale_size_discrete(range = c(5,2)) +
                scale_color_manual(values = "#628582") 
            },
            
            'non-ISP only' = {
              p +
                scale_size_discrete(range = c(2,5)) +
                scale_color_manual(values = "#554449") 
            } 
)

##############################################################################  
# 5c. settings that are the same for all plots
##############################################################################
  
p <- p +
  geom_point(aes(color = ISP, size = ISP),
             alpha = 0.7) +
  coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(plot.background = element_rect(fill = "white", color = "white" )) + 
  theme(panel.border = element_blank()) + 
  theme(
    panel.grid.major.x = element_line(size=.25, color="#554449", linetype = "dotted"), 
    panel.grid.minor.x = element_line(size=.25, color="#554449", linetype = "dotted"),  
    panel.grid.major.y = element_line(size=.25, color="#554449", linetype = "dotted"), 
    panel.grid.minor.y = element_line(size=.25, color="#554449", linetype = "dotted")) + 
  theme(plot.title = element_text(
    family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(0,0,0,0))) +
  theme(axis.text.x = element_text(
    size = 15, family = "Arial", color = "#554449", vjust=7, margin=margin(0,0,0,0))) +
  theme(axis.text.y = element_text(
    size = 15, family = "Arial", color ="#554449", margin=margin(0,0,0,0))) +
  theme(axis.title.x = element_text(
    size = 16, face = "bold", color = "#554449", family = "Arial",
    margin=margin(15,0,0,0))) +
  theme(axis.title.y = element_text(
    size = 16, face = "bold", color = "#554449", family = "Arial",
    margin=margin(0,15,0,0))) +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.ticks.y = element_blank()) + 
  theme(legend.text = element_text(size = 15, family = "Arial", color ="#554449")) +
  theme(legend.position = 'bottom') +
  theme(legend.background = element_rect(fill = "white")) + 
  theme(legend.key = element_rect(fill = "white", colour ="white")) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom") +
  ylab("Page Views")
  

##############################################################################
# 5d. Return and output the plot
##############################################################################

# Return the built plot
p
  
})


# Output the built plot
output$plot <- renderPlot({
  plotsettings()
}, height = 600)

##############################################################################
# 6. Give report details when user hovers the plot
##############################################################################

output$hover_info <- renderPrint({
  if(!is.null(input$plot_hover)){
    near <- nearPoints(dataset(), input$plot_hover,
                       threshold = 10, maxpoints = 1)
    near$Title[1]
    }
})



##############################################################################
# 7. Zoom in when user highlights an area and clicks;
# reset when user double clicks or changes the plot type.
# See http://shiny.rstudio.com/gallery/plot-interaction-zoom.html
##############################################################################

ranges <- reactiveValues(x = NULL, y = NULL)

observeEvent(input$plot_brush, {
  brush <- input$plot_brush
  if(!is.null(brush) & ((brush$ymax - brush$ymin) > 40)){
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
  } 
})

observeEvent(input$plot_click, {
  np <- nearPoints(dataset(), input$plot_click, maxpoints = 1, threshold = 10)
  
  GAdata$symbol <- rep("19", length(GAdata$Title))
  GAdata$symbol[GAdata$Title == np$Title] <- "20"
})

observeEvent(input$plot_doubleclick, {
  ranges$x <- NULL
  ranges$y <- NULL
})

observeEvent(input$cat, {
  ranges$x <- NULL
  ranges$y <- NULL
})


##############################################################################
# 8. Start the app
##############################################################################
}

shinyApp(ui= ui, server = server)
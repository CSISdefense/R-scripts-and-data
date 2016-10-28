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
library(DT)
library(htmlwidgets)


##############################################################################
# 2. Set up the user interface for the app
##############################################################################

ui <- fluidPage(
  fluidRow(
    column(3,
                 titlePanel("Google Analytics"),
                 
                 selectInput("cat", "Page Views by",
                             c("Weekday","Block", "Type", "Facebook",
                               "Twitter","Evening", "MailOpens", "MailClicks"),
                             selected = "Twitter"),
                 
                 br(),
                 h5("Hover over a point for the report title", align = "center"),
                 h5("Click a point for report details", align = "center"),
                 h5("Highlight an area to zoom in", align = "center"),
                 h5("Double click to zoom out", align = "center")
    ),
    
    column(9,
      div(
        style = "position:relative",
        plotOutput("plot", 
                 hover = hoverOpts(id = "plot_hover", delay = 20),
                 dblclick = "plot_doubleclick",
                 click = clickOpts(id = "plot_click"),
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE,
                                   delay = 700),
                 height = "auto"),
        uiOutput("hover_info")
      )
    )
  ),  
  hr(),
  DT::dataTableOutput("table")
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
GAdata$Title <- as.character(GAdata$Title)

# for point and table row highlighting
GAdata$symbol <- factor(rep("excluded", length(GAdata$Title)),
                        levels = c("included","excluded","selected"))
                          
makeReactiveBinding("GAdata")


##############################################################################
# Define visual settings for different categories
##############################################################################

pointColor <- c(
  "included" = "#554449",
  "excluded" = "#628582",
  "selected" = "#C74F4F"
  )

pointAlpha <- c(
  "included" = 0.75,
  "excluded" = 0.5,
  "selected" = 0.95
  )

pointSize <- c(
  "included" = 5,
  "excluded" = 2.5,
  "selected" = 8
  )


##############################################################################
# 4. Subset or highlight data based on user input
##############################################################################


# dataset <- reactive({
#     GAdata
#   
# })



##############################################################################
# 5. Build the plot based on user input
##############################################################################

plotsettings <- function(shown){
  
  
  # filter rows
  currentRows <- input$table_rows_all
  cat(currentRows)

  selected_rows <- which(shown$symbol == "selected")
  if(length(currentRows) > 0 & length(currentRows) < nrow(GAdata)){
    shown$symbol <- factor(rep("excluded", length(shown$Title)),
                            levels = c("included","excluded","selected"))
    shown$symbol[shown$X %in% currentRows] <- "included"
  } else {
    shown$symbol <- factor(rep("excluded", length(shown$Title)),
                            levels = c("included","excluded","selected"))
  }
  shown$symbol[shown$X %in% input$table_rows_selected] <- "selected"

  #dynamically change legend
   searchText <- input$table_search
   includeLegend <- ifelse(is.null(searchText) |
                              searchText == '', 'Searched', searchText)
  
##############################################################################    
# 5a. settings that differ depending on the x variable the user picks
# (i.e. Twitter, Weekend, Block, etc.) 
##############################################################################
  p <- switch(input$cat, 
        Weekday = {ggplot(data = shown, aes(x = Weekday, y = Views)) +
          scale_x_discrete(drop = FALSE) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Block = {ggplot(data = shown, aes(x = Block, y = Views)) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")}, 
        
        Type = {ggplot(data = shown, aes(x = Type, y = Views)) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Facebook = {ggplot(data = shown, aes(x = Facebook,
                                                 y = Views)) +
          geom_boxplot(outlier.shape = NA, colour = "#554449")},
        
        Twitter = {ggplot(data = shown, aes(x = Twitter, y = Views)) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")},
        
        Evening = {ggplot(data = shown, aes(x = Evening, y = Views)) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")},
        
        MailOpens = {ggplot(data = shown, aes(x = MailOpens, y = Views)) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")}, 
        
        MailClicks = {ggplot(data = shown, aes(x = MailClicks, y = Views)) +
            stat_smooth(method = "lm", se= FALSE, color = "#C74F4F")}
           
  )  


##############################################################################  
# 5c. settings that are the same for all plots
##############################################################################
  
p <- p +
  geom_point(aes(color = symbol, size = symbol, alpha = symbol)) +
  scale_alpha_manual(values = pointAlpha) +            
  scale_size_manual(values = pointSize) +
  scale_color_manual(values = pointColor,
                     labels = c(includeLegend, 'All/Other','Selected'),
                     drop = FALSE) +
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
  theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + 
  theme(legend.key = element_rect(fill = "white", colour ="white")) + 
  theme(legend.title = element_blank()) +
  ylab("Page Views") +
  guides(size = FALSE, alpha = FALSE)
  

##############################################################################
# 5d. Return and output the plot
##############################################################################

# Return the built plot
p
  
}


# Output the built plot
output$plot <- renderPlot({
  plotsettings(GAdata)
},height = 482)

##############################################################################
# 6. Give report details when user hovers the plot
# See https://gitlab.com/snippets/16220
##############################################################################

output$hover_info <- renderUI({
  hover <- input$plot_hover
  point <- nearPoints(GAdata, hover, threshold = 10,
                      maxpoints = 1, addDist = TRUE)
  if(nrow(point) == 0) return(NULL)
  
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / 
    (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / 
    (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * 
    (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * 
    (hover$range$bottom - hover$range$top)
  
  # Use HTML/CSS to change style of tooltip panel here
  style <- paste0(
    "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
   wellPanel(
    style = style,
    p(HTML(point$Title))
  )
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

observeEvent(input$plot_doubleclick, {
  ranges$x <- NULL
  ranges$y <- NULL
})

observeEvent(input$cat, {
  ranges$x <- NULL
  ranges$y <- NULL
})


##############################################################################
# 8. When user clicks a point, change the shape of that point
# See http://stackoverflow.com/questions/33480644/change-plot-on-click-in-shiny
##############################################################################

observeEvent(input$plot_click, {
  nearest <- nearPoints(GAdata,
                        input$plot_click, maxpoints = 1, threshold = 10)
  GAdata$symbol[GAdata$Title == nearest$Title] <- "selected"
  
  # tableSelected[[2]][1] <- which(GAdata$Title == nearest$Title)
})


##############################################################################
# 9. Run the datatable
# see https://yihui.shinyapps.io/DT-info/
#
# javascript for datable options argument found at:
# https://datatables.net/reference/option/
# https://datatables.net/reference/option/dom#Styling
# http://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
##############################################################################


output$table <- DT::renderDataTable({
  tablesettings()
})

tablesettings <- function(){
  shown <- GAdata
  shown <- select(shown, Title, Author, Program, Date, Views)
  datatable(shown,
            options = list(
              "lengthChange" = FALSE,
              "pageLength" = 5,
              "dom" =
                "t+<'row'<'small-6 columns'i><'small-6 columns'p>>",
                #<'row'<'col-sm-4'f>>+
              ordering = TRUE,
              order = list(list(4, 'desc')),
              autoWidth = FALSE,
              columnDefs = list(list(width = '160px', targets = 2),
                                list(width = '70px', targets = 3),
                                list(width = '35px', targets = 4))
            ),
            class = "display",
            rownames = FALSE,
            
            filter = 'top'
  )
}



##############################################################################
# 10. Start the app
##############################################################################
}

shinyApp(ui= ui, server = server)
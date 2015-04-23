
#*************************************Source Files*****************************************************
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"

#*************************************Required Libraries******************************************
require(plyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)
# basicConfig()
# logdebug("not shown, basic is INFO")
# logwarn("shown and timestamped")

# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# debug("CreateCSV")

# debug(apply_lookups)
# debug(CreateDuration)

#Read in functions
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))

create_procedural_graphs<-function(data.name,section.variable.name,start.year=NULL){
        

        # debug(CreateChart)
        
        # debug(LatticePlot)
        # debug(CreateChart)
        # debug(new_page)
        # debug(PrepareLabelsAndColors)
        # debug(LatticePercentLinePlot)
        # debug(Boxplot)
        # debug(ClassifySize)
        # debug(apply_lookups)
        # debug(HistogramOrDensity)
        
        #*************************************Lookup Files*****************************************************
        
        
        choice.data <-read.csv(
                paste(Path,"Lookups\\Choice_Data.csv",sep=""),
                header=TRUE, sep=",", dec=".", strip.white=TRUE, na.strings = c("NA","NULL"),
                stringsAsFactors=FALSE
        )

        which.data<-choice.data$Name==data.name
        which.section.variable<-section.variable.name
        
        # start.year<-2004
        # start.year<-2000
        # start.year<-2009
        if (is.null(start.year)) start.year<-choice.data$Start.Year[which.data]

        OutputPath<-paste(
                #   Path
                #             ,
                "Output\\"
                #             ,as.character(choice.data$Name[which.data]),"\\"
                ,gsub("[.]","_",as.character(which.section.variable)),"\\"
                ,sep="")
        
        logfile<-paste(
                OutputPath
                ,as.character(choice.data$Prefix[which.data])
                ,as.character(choice.data$ProdServCode.Prefix[which.data])
                ,gsub("[.| |<|>|$]","_",which.section.variable),"_"
                ,"create_contracting_figures.csv",sep="")
        
        
        # debug(CreateTable)
        Coloration<-read.csv(
                paste(Path,"Lookups\\","lookup_coloration.csv",sep=""),
                header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE, 
                stringsAsFactors=FALSE
        )
        
        #Clear out lines from the coloration CSV where no variable is listed.
        Coloration<-subset(Coloration, variable!="")
        
        Coloration<-ddply(Coloration
                          , c(.(R), .(G), .(B))
                          , transform
                          , ColorRGB=as.character(
                                  if(min(is.na(c(R,G,B)))) {NA} 
                                  else {rgb(max(R),max(G),max(B),max=255)}
                          )
        )
        
        choice.figures <-read.csv(
                paste(
                        #     Path
                        #         ,"Lookups\\"
                        #         ,
                        as.character(choice.data$Choice.Figures.File[which.data])
                        ,sep=""),
                header=TRUE, sep=",", dec=".", strip.white=TRUE, 
                stringsAsFactors=FALSE, na.strings=""
        )
        choice.figures$figure.number<-formatC(as.numeric(choice.figures$figure.number),width=max(nchar(choice.figures$figure.number)),format='f'
                                              ,digits=max(nchar(choice.figures$figure.number))-2
                                              ,flag='0')
        if(!is.na(start.year)){
                start.year<-as.Date(paste("09","30",start.year,sep="/"),format='%m/%d/%Y')
        }
        
        choice.figures<-subset(choice.figures,section.variable==which.section.variable)
        
        
        #*************************************Import Data Files*************************************************
        
        # debug(ScatterPlot)
        data.frame.list<-list()
        choice.input<-unique(subset(choice.figures, select=c(file.type,use.all.agencies,data.file)))
        
        for(figureNUM in 1:nrow(choice.input)){
                # if(figureNUM==4)
                #   debug(import_figures_and_tables)
                #     debug(import_figures_and_tables)
                choice.input$figureNUM[figureNUM]<-figureNUM
                data.frame.list[figureNUM]<-list(
                        import_figures_and_tables(Path
                                                  , choice.data
                                                  , which.data
                                                  , choice.input
                                                  , figureNUM
                                                  , logfile
                                                  #                           , subset(Categories, 
                                                  #                                    Report==paste(
                                                  #                                      as.character(choice.data$Prefix[which.data])
                                                  #                                      ,as.character(choice.data$ProdServCode.Prefix[which.data])
                                                  #                                      ,sep=""
                                                  #                                    )
                                                  #                           )
                        )
                )
                if(!is.na(start.year) &&  "Fiscal.Year" %in% names(data.frame.list[[figureNUM]])){
                        data.frame.list[[figureNUM]]<-subset(data.frame.list[[figureNUM]],Fiscal.Year>=start.year)
#                         data.frame.list[[figureNUM]]<-subset(data.frame.list[[figureNUM]],Fiscal.Year<=as.Date(paste("09","30","2013",sep="/"),format='%m/%d/%Y'))
                        
                }
                
        }
        choice.figures<-join(choice.figures,choice.input,match="first")
        # stop("hammer")
        
        #*************************************Quality Control*************************************************
        # 
        # 
        #   FPDS.gov.buckets.df<-load.FPDS.gov.buckets.df(Path,choice.data,which.data)
        #   FPDS.gov.customers.df<-load.FPDS.gov.customers.df(Path,choice.data,which.data)
        # 
        # CompareTopline(FPDS.gov.buckets.df,"FPDS.gov.bucket",FPDS.gov.customers.df,"FPDS.gov.customer",logfile)
        # 
        # # #FPDS.gov.Customers has already had non-services stripped out if the Big.ProdServ is services  
        # 
        # for(figureNUM in 1:nrow(choice.figures)){
        #   
        #   if(choice.figures$use.all.agencies[figureNUM]==TRUE && choice.data$Name[which.data]!="Services" && choice.data$Name[which.data]!="Overall"){
        #     data.frame.list[[figureNUM]]<-subset(data.frame.list[[figureNUM]],
        #                                          data.frame.list[[figureNUM]][,choice.figures$subset.agency.variable[figureNUM]]==choice.data$Customer[which.data]
        #                                          )
        #   }
        # #   debug(LimitScope)
        #   if(("ServicesCategory" %in% names(data.frame.list[[figureNUM]])) && 
        #        (choice.figures$form[figureNUM]=="chart") &&      
        #        (!is.null(FPDS.gov.buckets.df))    
        #      ){
        # #     debug(CompareAbstractVariables)
        #     
        #     CompareAbstractVariables(data.frame.list[[figureNUM]]
        #                              ,choice.figures$legend.name[[figureNUM]]
        #                              ,LimitScope(FPDS.gov.buckets.df
        #                                          , data.frame.list[[figureNUM]]
        #                              )
        #                              ,"FPDS.gov.buckets.df"
        #                              ,"ServicesCategory"
        #                              ,logfile
        #     )
        #     
        #     
        #   } 
        # # debug(LimitScope)
        #   if("Customer" %in% names(data.frame.list[[figureNUM]]) &&
        #        (choice.figures$form[figureNUM]=="chart")    &&
        #        (!is.null(FPDS.gov.customers.df))){
        # #         debug(CompareAbstractVariables)
        #     CompareAbstractVariables(data.frame.list[[figureNUM]]
        #                              ,choice.figures$legend.name[[figureNUM]]
        #                              ,LimitScope(FPDS.gov.customers.df
        #                                          , data.frame.list[[figureNUM]]
        #                              )
        #                              ,"FPDS.gov.customers.df"
        #                              ,"Customer"
        #                              ,logfile
        #     )
        #   }
        # #   debug(CompareAbstractVariables)
        #   if(!is.na(choice.figures$section.compare[figureNUM])&&(!choice.figures$section.variable[figureNUM]=="Overall")){
        #     CompareAbstractVariables(data.frame.list[[figureNUM]]
        #                              ,choice.figures$legend.name[[figureNUM]]
        #                              , data.frame.list[[choice.figures$section.compare[[figureNUM]]]]
        #                              ,choice.figures$legend.name[[choice.figures$section.compare[[figureNUM]]]]
        #                              ,choice.figures$section.variable[[figureNUM]]
        #                              ,logfile
        #                              
        #     )
        #   }
        # }
        #   
        # 
        # 
        # 
        # 
        # 

        
        
        #*************************************Graphical Settings*****************************************************
        
        choice.layout <-read.csv(
                paste(Path,"Lookups\\Choice_Layout.csv",sep=""),
                header=TRUE, sep=",", dec=".", strip.white=TRUE, 
                stringsAsFactors=FALSE
        )
        
        # which.layout<-choice.layout$Layout.Name=="portrait single"
        which.layout<-choice.layout$Layout.Name=="portrait small"
        # which.layout<-choice.layout$Layout.Name=="portrait grid"
        # which.layout<-choice.layout$Layout.Name=="single column"
        # which.layout<-choice.layout$Layout.Name=="landscape grid"
        
#         axis.text.size<-0.65
        #legend.text.size<-0.8                    
        #table.text.size<-0.750
        #title.text.size<-1.15
        
        axis.text.size<-5
        strip.text.size<-4
        legend.text.size<-4
        # table.text.size<-5.75
        title.text.size<-10
        geom.text.size<-1.75
        
        main.text.size<-1
        note.text.size<-0.70
        standard.mar<-c(2,2.5,3,2)
        standard.mgp<-c(1.5,0.5,0)
        
        if(choice.layout$page.layout[which.layout]=="grid"){
                if(choice.layout$page.orientation[which.layout]=="portrait"){
                        
                        graph.width<-unit(
                                c(0.5,0.5), 
                                c("npc","npc")
                        )
                        graph.height<-unit(
                                c(0.33,0.33,0.33), 
                                c("npc","npc","npc")
                        )
                        par.col.vals=c(1,2,1,2,1,2)
                        par.row.vals=c(1,1,2,2,3,3)
                        
                } else if(choice.layout$page.orientation[which.layout]=="landscape"){
                        
                        graph.width<-unit(
                                c(0.33,0.33,0.33), 
                                c("npc","npc","npc")
                        )
                        graph.height<-unit(
                                c(0.5,0.5), 
                                c("npc","npc")
                        )  
                        par.col.vals=c(1,2,3,2,1,3)
                        par.row.vals=c(1,1,1,2,2,2)
                }
        } else if (choice.layout$page.layout[which.layout]=="single"){
                graph.width<-unit(
                        c(1), 
                        c("npc")
                )
                graph.height<-unit(
                        c(1), 
                        c("npc")
                )
                par.col.vals<-c(rep(1,nrow( choice.figures)))
                par.row.vals<-c(rep(1,nrow( choice.figures)))       
        }
        
        Layout<-grid.layout(
                nrow = choice.layout$page.nrows[which.layout], 
                ncol = choice.layout$page.ncols[which.layout],
                widths = graph.width,
                heights = graph.height
        )
        if(which.section.variable=="Overall"){
                Categories<-c("Overall")
        } else {#Get a list of graphed categories
                Categories<-unique(
                        as.character(subset(data.frame.list[[1]][,which.section.variable]
                                            ,is.null(data.frame.list[[1]]$Graph)|data.frame.list[[1]]$Graph==TRUE)
                        )
                )
        }
        
        for(sectionNUM in 1:length(Categories)) {
                sectionSTR<-Categories[[sectionNUM]]
                
                if (choice.layout$page.layout[which.layout]=="grid"){
                        
                        note<-""
                        new_page(""
                                 ,choice.layout
                                 ,which.layout
                                 ,OutputPath
                                 , paste(
                                         #                 as.character(choice.data$Prefix[which.data])
                                         ,as.character(choice.data$ProdServCode.Prefix[which.data])
                                                ,sep=""
                                 )
                                 , sectionSTR
                                 , Layout
                        )
                }
                
                #   if(max(data.frame.list[[1]]$Total)<30){
                #     MaxY<-ceiling(max(data.frame.list[[1]]$Total/5))*5
                #   } else if(max(data.frame.list[[1]]$Total)>=30){
                #     MaxY<-ceiling(max(data.frame.list[[1]]$Total/10))*10
                #   }    
                
                #note<-paste(note,CheckTotals(categoryDF,services_master,"Area"),sep="")
                
                Prefix<-paste(as.character(choice.data$Prefix[which.data])
                              ,as.character(choice.data$ProdServCode.Prefix[which.data])
                              ,sep=""
                )
                if (which.section.variable %in% c("ServicesCategory.detail","ServicesCategory.sum")){
                        Prefix<-as.character(choice.data$Prefix[which.data])
                }
                
                #********************Create Charts**********************************
                #   debug(CreateTable)
                for(figureNUM in 1:min(nrow( choice.figures),nrow( par.row.vals))){
                        note<-CreateChartOrTable(
                                note
                                , choice.layout
                                , which.layout
                                , choice.data
                                , which.data
                                , OutputPath
                                , Prefix
                                , sectionSTR
                                , Layout
                                , Coloration
                                , data.frame.list[[choice.figures$figureNUM[figureNUM]]]
                                
                                , choice.figures
                                , figureNUM
                                , par.row.vals[figureNUM]
                                , par.col.vals[figureNUM]
                                #       , Categories$start.date[[sectionNUM]]
                        )       
                }
                #   Title(Categories, note,x.dimension)  
                
                #********************************Clean up**************************
                #Close all open devices (devices in this program are files opened for output)
                while(!(dev.cur()[[1]]==1)){
                        dev.off()
                }
                #Clean up
                #   rm(MaxY, note)
                rm(note,Prefix)
        }                                                      
        #******************************* Clean Up***********************
        # rm(list=ls(all=TRUE))
}



# which.data<-choice.data$Name=="Services"
# which.data<-choice.data$Name=="Defense Products"
# which.data<-choice.data$Name=="Defense Competition MCC"
# which.data<-choice.data$Name=="Defense Competition State"
# which.data<-choice.data$Name=="D3"
# which.data<-choice.data$Name=="Defense"
# which.data<-choice.data$Name=="Defense Fixed Price"
# which.data<-choice.data$Name=="Defense Components"
# which.data<-choice.data$Name=="DHS"
# which.data<-choice.data$Name=="State and IAP"
# which.data<-choice.data$Name=="Finance"
# which.data<-choice.data$Name=="Europe"


# which.section.variable<-"SubCustomer.component"
# which.section.variable<-"SubCustomer.detail"
# which.section.variable<-"ServicesCategory.detail" 
# which.section.variable<-"MajorCommandCode" 
# which.section.variable<-"StateCode" #doneCode" 
# which.section.variable<-"ProductsCategory.detail" 
# which.section.variable<-"ServicesCategory.sum" 
# which.section.variable<-"Overall"  
# which.section.variable<-"systemequipmentshorttext"  
# which.section.variable<-"Contract.Size.sum" 
# which.section.variable<-"Vendor.Size.sum"
# which.section.variable<-"Competition.sum"
# which.section.variable<-"PlatformPortfolio.sum"
# Not Typically used
# which.section.variable<-"Agency"

# create_procedural_graphs(which.data,which.section.variable)
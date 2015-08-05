require(ggplot2)
require(grid)
require(scales)
require(reshape2)
require(plyr)
require(scales)

CleanFileName<-function(Name){
    gsub("__+","_",
         gsub("Overall_Overall_","Overall_",
              gsub("NA_","", 
                   gsub("[/|:|*|\n]","-",
                        gsub("[.| |<|>|$|,]","_",
                             
                             Name
                             
                        )
                   )
              )
         )
    )
}


VariableNumericalFormat<-function(VAR.number,VAR.detail=0){
    if(!is.numeric(VAR.number)){
        stop("VariableNumericalFormat is for number values only.")
    }
    #   stop("test")
    if(all(VAR.number<1,na.rm=TRUE)){
        VAR.number<-percent(VAR.number)
    }
    else{
        VAR.number[VAR.number>=-3&VAR.number<=3&!is.na(VAR.number)]<-round(VAR.number[VAR.number>=-3&VAR.number<=3&!is.na(VAR.number)],1+VAR.detail)
        #   format(
        #     round(VAR.number[VAR.number>=-3&VAR.number<=3],2)
        #     , digits=2
        #     , drop0trailing=TRUE
        #     , trim=TRUE
        #     , big.mark=","
        #     )
        VAR.number[((as.numeric(VAR.number)<(-3)|as.numeric(VAR.number)>3))&!is.na(VAR.number)]<-
            round(as.numeric(VAR.number[((as.numeric(VAR.number)<(-3)|as.numeric(VAR.number)>3))&!is.na(VAR.number)]),0+VAR.detail)
        first<-length(VAR.number)
        #   VAR.number[as.numeric(VAR.number)<(-3)|as.numeric(VAR.number)>3]<-format(
        #     round(as.numeric(VAR.number[as.numeric(VAR.number)<(-3)|as.numeric(VAR.number)>3]),0)
        # #     , digits=1
        #     , drop0trailing=TRUE
        #     , trim=TRUE
        #     , big.mark=","
        #   )
        second<-length(VAR.number)
        if(first!=second){stop("mismatch")}
        #   if(VAR.number>=-3&VAR.number<=3){
        #     VAR.number<-format(round(VAR.number,2), digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
        #   }
        #   else{
        #     VAR.number<-format(round(VAR.number,1), digits=0, trim=TRUE, big.mark=",")
        #   }
        #   
        VAR.number<-format(VAR.number, trim=TRUE, big.mark=",",digits=VAR.detail)
    }
    VAR.number
}

new_page <- function(VAR.topic
                     ,VAR.choice.layout
                     ,VAR.which.layout
                     ,Var.OutputPath
                     , VAR.prefix
                     , VAR.sectionSTR
                     , VAR.Layout
) {
    if (!(dev.cur()[[1]]==1)){
        dev.off()
    }
    endtext<-VAR.choice.layout$page.orientation[VAR.which.layout]
    #   if(!(is.null(VAR.topic))){
    #     endtext<-paste("_",VAR.topic,sep="")
    #   }
    
    VAR.width<-VAR.choice.layout$page.width[VAR.which.layout]
    VAR.height<-VAR.choice.layout$page.height[VAR.which.layout]
    if (VAR.topic=="Top10"){
        VAR.width<-VAR.choice.layout$table.width[VAR.which.layout]
        VAR.height<-VAR.choice.layout$table.height[VAR.which.layout]
    } 
    if(!(is.null(VAR.topic))){
        VAR.topic<-paste("_",VAR.topic,sep="")
        endtext<-paste("_",endtext,sep="")
    }
    
    
    
    # pdf(paste(Var.OutputPath,VAR.prefix,VAR.sectionSTR,"_",VAR.topic,endtext,".pdf", sep=""), width=VAR.choice.layout$page.width[VAR.which.layout], height=VAR.choice.layout$page.height[VAR.which.layout])
    # svg(paste(Var.OutputPath,VAR.prefix,VAR.sectionSTR,"_",VAR.topic,endtext,".svg", sep=""), width=VAR.choice.layout$page.width[VAR.which.layout], height=VAR.choice.layout$page.height[VAR.which.layout])
    
    
    png(
        paste(Var.OutputPath,
              
              CleanFileName(paste(
                  VAR.prefix
                  ,VAR.sectionSTR
                  ,VAR.topic
                  ,endtext
                  , sep="")
              )       
              ,".png"
              , sep=""
        )
        , type="cairo"
        , width=VAR.width
        , height=VAR.height
        , units='in'
        , pointsize=12
        , res=300
    )
    grid.newpage()
    pushViewport(viewport(layout = VAR.Layout,gp=gpar(fontsize=10)))
    VAR.Layout
}

annual.growth.rate <- function(a){
    #Source: http://stackoverflow.com/questions/27058750/plot-compound-annual-growth-rate-3-independent-variables-in-r
    T1 <-as.numeric( format(max(a$Fiscal.Year), "%Y")) - as.numeric( format(min(a$Fiscal.Year), "%Y")) +1
    FV <- a[which(a$Fiscal.Year == max(a$Fiscal.Year)),"value"]
    SV <- a[which(a$Fiscal.Year == min(a$Fiscal.Year)),"value"]
    cagr <- ((FV/SV)^(1/T1)) -1
    
}

ProbabilityDf <- function(VAR.df,VAR.dep.name,VAR.dep.value, VAR.value="value"){
    #P(dep|ind)
    probability<-sum(VAR.df[,VAR.value][VAR.df[,VAR.dep.name]==VAR.dep.value])
    probability<-probability/sum(VAR.df[,VAR.value])
    probability
}


ConditionalProbabilityDf <- function(VAR.df,VAR.dep.name,VAR.dep.value,VAR.ind.name,VAR.ind.value, VAR.value="value"){
    #P(dep|ind)
    conditional.probability<-sum(VAR.df[,VAR.value][VAR.df[,VAR.dep.name]==VAR.dep.value & VAR.df[,VAR.ind.name]==VAR.ind.value])
    conditional.probability<-conditional.probability/sum(VAR.df[,VAR.value][VAR.df[,VAR.ind.name,]==VAR.ind.value])
    conditional.probability
}


conditional.probability.matrix <- function(VAR.matrix,VAR.row,VAR.col){
    sum(VAR.matrix[VAR.row,VAR.col])/sum(VAR.matrix[,VAR.col])
}


cleanup.top.20.rank <- function(VAR.rank){
    VAR.result<-VAR.rank
    if(VAR.rank==-1){
        VAR.result<-"--"
    }
    else if(VAR.rank>20){
        VAR.result<-">20"
    }
    VAR.result
}

CreateChartOrTable<-function(VAR.note #unused
                             ,VAR.choice.layout
                             , VAR.which.layout
                             ,VAR.choice.data
                             ,VAR.which.data
                             ,Var.OutputPath 
                             , VAR.prefix 
                             , VAR.sectionSTR 
                             ,VAR.Layout #unused
                             ,VAR.Coloration #unused
                             ,VAR.long.DF 
                             , VAR.choice.figures #unused
                             ,VAR.which.figure #unused
                             ,VAR.row 
                             ,VAR.col 
                             , VAR.startyear=NA #unused
){
    if(!is.na(VAR.choice.figures$subset.variable[VAR.which.figure])){
        VAR.long.DF$Graph[!VAR.long.DF[,VAR.choice.figures$subset.variable[VAR.which.figure]]==VAR.choice.figures$subset.criteria[VAR.which.figure]]<-FALSE
    }
    
    
    if(VAR.sectionSTR!="Overall"){
        VAR.long.DF<-subset(VAR.long.DF
                            ,VAR.long.DF[[VAR.choice.figures$section.variable[VAR.which.figure]]]==as.character(VAR.sectionSTR)
        )
    }
    
    
    if(nrow(VAR.long.DF[VAR.long.DF$Graph==TRUE,])>0){
        if(VAR.choice.figures$form[VAR.which.figure] %in% c("Stacked_Bar"
                                                            ,"Lattice_Bar"
                                                            ,"Lattice_Line"
                                                            ,"scatterplot"
                                                            ,"histogram"
                                                            ,"density"
                                                            ,"boxplot")
        ){
            CreateChart(VAR.note
                        ,VAR.choice.layout
                        , VAR.which.layout
                        ,VAR.choice.data
                        ,VAR.which.data
                        ,Var.OutputPath
                        , VAR.prefix
                        , VAR.sectionSTR
                        ,VAR.Layout
                        ,VAR.Coloration
                        ,VAR.long.DF
                        , VAR.choice.figures
                        ,VAR.which.figure
                        ,VAR.row
                        ,VAR.col
                        , VAR.startyear
            )
        } else if (VAR.choice.figures$form[VAR.which.figure]=="table"){
            #         debug(CreateTable)
            CreateTable(VAR.note
                        ,VAR.choice.layout
                        , VAR.which.layout
                        ,Var.OutputPath 
                        , VAR.prefix 
                        , VAR.sectionSTR 
                        ,VAR.Layout 
                        ,VAR.Coloration
                        ,VAR.long.DF 
                        , VAR.choice.figures 
                        ,VAR.which.figure 
                        ,VAR.row 
                        ,VAR.col 
                        , VAR.startyear
            )
        } else {
            stop(paste("Unknown form:",VAR.choice.figures$form[VAR.which.figure]))
        }
        
    }
}

CreateCSV<-function(Var.OutputPath 
                    , VAR.prefix 
                    , VAR.sectionSTR 
                    ,VAR.topic
                    ,VAR.long.DF 
){
    if(!(is.null(VAR.topic))){
        VAR.topic<-paste("_",VAR.topic,sep="")
    }
    
    
    file.name<-paste(
        Var.OutputPath
        
        ,gsub("__+","_",
              gsub("Overall_Overall_","Overall_",
                   gsub("NA_","",                       
                        gsub("_NA$","",                       
                             gsub("[.| |<|>|$|,|]","_",
                                  paste(
                                      VAR.prefix
                                      ,VAR.sectionSTR
                                      ,VAR.topic
                                      #                            ,endtext
                                      , sep="")
                             )
                        )
                   )
              )
        )
        ,".csv"
        , sep=""
    )
    
    if("Row" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(Row))
    }
    if("Checksum" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(Checksum))
    }
    if("variable" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(variable))
    }
    if("Total" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(Total))
    }
    if("ServiceCategories" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(ServiceCategories))
    }
    if("Customer" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(Customer))
    }
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, select=-c(Graph))
    }  
    
    
    write.table(VAR.long.DF
                ,file=file.name
                , sep=","
                , row.names=FALSE
                , append=FALSE
    )
    
    sink(file.name
         ,append=TRUE
    )
    cat("Source: FPDS and CSIS analysis")
    cat("Available online at www.csis.org/NSPIR/DoD")
    sink(file =NULL)
}



CreateTopVendorList<-function(
    VAR.choice.layout
    ,VAR.which.layout
    ,VAR.long.DF
    ,name.variable
    ,value.variable
    ,VAR.row 
    ,VAR.col 
    ,VAR.top.count
    ,VAR.year=NA
    ,VAR.x.offset=0
    ,rank.variable=NA
    ,value.label=NA
    ,name.label="Vendors"
    ,total.label=NA
){
    
    if(is.na(name.label)){
        name.label<-paste(as.character(format(max(VAR.long.DF$Fiscal.Year),"%Y")),"$ Millions")
    }
    if(!is.na(VAR.year)){
        VAR.long.DF <-subset(VAR.long.DF 
                             ,Fiscal.Year==as.Date(paste(as.character(VAR.year),"/09/30/", sep = ""))
        )
        name.label<-paste(name.label,"in",VAR.year)
    }
    if(is.na(total.label)){
        total.label<-"Total Obligations"
    }
    else{
        total.label<-paste("Total",total.label,"Obligations")
    }
    
    attach(VAR.long.DF)
    
    if(is.na(rank.variable)){
        if("AnnualSubCustomerVendorRank" %in% names(VAR.long.DF))
        {
            rank.variable<-'AnnualSubCustomerVendorRank'
        }
        else if("ContractAnnualCustomerVendorRank" %in% names(VAR.long.DF))
        {
            rank.variable<-'ContractAnnualCustomerVendorRank'
        }
        else if("VendorRank" %in% names(VAR.long.DF))
        {
            rank.variable<-'VendorRank'
        }
        else stop ("Unknown VendorRank")
    }
    #     VAR.long.DF<-VAR.long.DF[order(VAR.long.DF,VAR.long.DF[,rank.variable])]
    
    #Reduce the count in case the number of entries is less than the default
    VAR.top.count<-min(VAR.top.count,length(VAR.long.DF[,name.variable]))
    
    #Optional Top Row, only used in the longer top 20 lists
    print(
        grid.text(name.label
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
                  ,hjust=0
                  ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    
    print(
        grid.text(value.label
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                  ,hjust=1
                  ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col))
    
    # Prior year ratings
    #   print(
    #     grid.text(as.character(VAR.year-1)
    #               ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+2,"lines")
    #               ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout],"lines")
    #               ,hjust=1
    #               ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
    #     , vp=vplayout(VAR.row,VAR.col))
    #   print(
    #     grid.text("Rank"
    #               ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
    #               ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout],"lines")
    #               ,hjust=1
    #               ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
    #     , vp=vplayout(VAR.row,VAR.col))
    print(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+0.5
                            ,VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+0.5)
                          ,"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout])#Used to go to prevrank
                           ,"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    
    TopTotal<-0
    for(i in 1:VAR.top.count){
        print(
            grid.text(VAR.long.DF[VAR.top.count+1-i,name.variable]
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                      ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
                      ,hjust=0
                      ,gp=gpar(fontface="plain"
                               ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            , vp=vplayout(VAR.row,VAR.col))
        TopTotal<-TopTotal+VAR.long.DF[VAR.top.count+1-i,value.variable]*1000
        print(
            if(round(VAR.long.DF[VAR.top.count+1-i,value.variable]*1000,-1)==0){
                grid.text("<1"
                          ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                          ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                          ,hjust=1,gp=gpar(fontface="plain",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
                
            }
            else{
                grid.text(
                    format(round(VAR.long.DF[VAR.top.count+1-i,value.variable]*1000,-1), trim=TRUE, big.mark=",")
                    ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                    ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                    ,hjust=1,gp=gpar(fontface="plain",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
                
            }
            , vp=vplayout(VAR.row,VAR.col))
        #Previous rank
        #         print(
        #           grid.text(cleanup.top.20.rank(Previous.Rank[[VAR.top.count+1-i]])
        #                     ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i
        #                             ,"lines")
        #                     ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout]
        #                             ,"lines")
        #                     ,hjust=1
        #                     ,gp=gpar(fontface="plain"
        #                              ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        #           , vp=vplayout(VAR.row,VAR.col)
        #         )
    }
    print(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]+0.5,VAR.choice.layout$base.y.line[VAR.which.layout]+0.5)
                          ,"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout])
                           ,"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    print(
        grid.text(paste("Top"
                        #                     ,VAR.top.count,Categories$Short[[sectionNUM]]
                        ,"Obligations")
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=0,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    
    
    
    print(
        grid.text(format(round(TopTotal,-1), trim=TRUE, big.mark=",")
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=1,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    print(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]-0.5
                            ,VAR.choice.layout$base.y.line[VAR.which.layout]-0.5),"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]),"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    print(
        grid.text(total.label,
                  y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]-1
                         ,"lines"),
                  x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                         ,"lines"),
                  just=c("left","top"),
                  gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout])),
        vp=vplayout(VAR.row,VAR.col)
    )
    print(
        grid.text(VariableNumericalFormat(round(sum(VAR.long.DF[,value.variable])*1000,-1))
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]-1
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=1,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        , vp=vplayout(VAR.row,VAR.col)
    )
    
    
    detach(VAR.long.DF) # clean up
}




CreateTopVendorListWrapper<-function(
    VAR.choice.layout
    ,VAR.which.layout
    ,value.label
    ,name.label
    ,VAR.long.DF
    ,name.variable
    ,value.variable
    ,rank.variable
    ,VAR.top.count
    ,VAR.year=NA
    ,VAR.x.offset=0
){
    
    
    
    if(!is.na(VAR.year)){
        VAR.long.DF <-subset(VAR.long.DF 
                             ,Fiscal.Year==as.Date(paste(as.character(VAR.year),"/09/30/", sep = ""))
        )
    }
    
    attach(VAR.long.DF)
    
    
    #Reduce the count in case the number of entries is less than the default
    VAR.top.count<-min(VAR.top.count,length(VAR.long.DF[,name.variable]))
    
    #Optional Top Row, only used in the longer top 20 lists
    if (VAR.choice.layout$page.layout[VAR.which.layout]=="single"){
        #     print(
        #       grid.text("Top 20"
        #                 ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+2,"lines")
        #                 ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
        #                 ,hjust=0
        #                 ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        #       
        #     )
        toptable<-(
            grid.text(paste("Vendors in", VAR.year)
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
                      ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
                      ,hjust=0
                      ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            
        )
        
        #     toptable<-toptable+(
        #       grid.text("Obligations in"
        #                 ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+2,"lines")
        #                 ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
        #                 ,hjust=1
        #                 ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        #                 
        #       )
        
    }
    else{
        toptable<-(
            grid.text(value.label
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
                      ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
                      ,hjust=0
                      ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            
        )
    }
    
    
    
    toptable<-toptable+
        grid.text(name.label
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                  ,hjust=1
                  ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
    
    #   toptable<-toptable+(
    #     grid.text(as.character(VAR.year-1)
    #               ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+2,"lines")
    #               ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout],"lines")
    #               ,hjust=1
    #               ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
    #     )
    #   toptable<-toptable+(
    #     grid.text("Rank"
    #               ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+1,"lines")
    #               ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout],"lines")
    #               ,hjust=1
    #               ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
    #     )
    toptable<-toptable+(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+0.5
                            ,VAR.choice.layout$base.y.line[VAR.which.layout]+VAR.top.count+0.5)
                          ,"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout])
                           ,"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    for(i in 1:VAR.top.count) {
        toptable<-toptable+(
            grid.text(VAR.long.DF[[VAR.top.count+1-i,name.variable]]
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                      ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout],"lines")
                      ,hjust=0
                      ,gp=gpar(fontface="plain"
                               ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        )
        toptable<-toptable+(
            if(format(round(value[[VAR.top.count+1-i]],-1), trim=TRUE, big.mark=",")==0){
                grid.text("<1"
                          ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                          ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                          ,hjust=1,gp=gpar(fontface="plain",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            }
            else{
                grid.text(
                    format(round(value[[VAR.top.count+1-i]],-1), trim=TRUE, big.mark=",")
                    ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i,"lines")
                    ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout],"lines")
                    ,hjust=1,gp=gpar(fontface="plain",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            }
        )
        #         toptable<-toptable+(
        #           grid.text(cleanup.top.20.rank(Previous.Rank[[VAR.top.count+1-i]])
        #                     ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+i
        #                             ,"lines")
        #                     ,x=unit(VAR.x.offset+VAR.choice.layout$prevrank.column[VAR.which.layout]
        #                             ,"lines")
        #                     ,hjust=1
        #                     ,gp=gpar(fontface="plain"
        #                              ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        #           
        #         )
    }
    toptable<-toptable+(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]+0.5,VAR.choice.layout$base.y.line[VAR.which.layout]+0.5)
                          ,"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout])
                           ,"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    toptable<-toptable+(
        grid.text(paste("Top"
                        #                     ,VAR.top.count,Categories$Short[[sectionNUM]]
                        ,"Obligations")
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=0,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    
    TopTotal<-sum(VAR.long.DF[VAR.long.DF[,rank.variable]!=0&
                                  VAR.long.DF[,rank.variable]<=VAR.top.count
                              ,value.variable])
    
    toptable<-toptable+(
        grid.text(format(round(sum(TopTotal)*1000,-1), trim=TRUE, big.mark=",")
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=1,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    toptable<-toptable+(
        grid.lines(y=unit(c(VAR.choice.layout$base.y.line[VAR.which.layout]-0.5
                            ,VAR.choice.layout$base.y.line[VAR.which.layout]-0.5),"lines")
                   ,x=unit(c(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                             ,VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]),"lines")
                   ,gp=gpar(fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    toptable<-toptable+(
        grid.text(paste("Total"
                        #                     ,Categories$Short[[sectionNUM]]
                        ,"Obligations")
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]-1
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$name.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=0,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    toptable<-toptable+(
        grid.text(VariableNumericalFormat(round(sum(VAR.long.DF$value)*1000,-1))
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]-1
                          ,"lines")
                  ,x=unit(VAR.x.offset+VAR.choice.layout$dollar.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=1,gp=gpar(fontface="bold"
                                   ,fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    
    
    detach(VAR.long.DF) # clean up
    toptable
}



CreateTable<-function(VAR.note #unused
                      ,VAR.choice.layout
                      , VAR.which.layout
                      ,Var.OutputPath 
                      , VAR.prefix 
                      , VAR.sectionSTR 
                      ,VAR.Layout #unused
                      ,VAR.Coloration #unused
                      ,VAR.long.DF 
                      , VAR.choice.figures #unused
                      ,VAR.which.figure #unused
                      ,VAR.row 
                      ,VAR.col 
                      , VAR.startyear=NA #unused
){
    
    
    
    if(VAR.sectionSTR!="Overall"){
        VAR.long.DF<-subset(VAR.long.DF
                            ,VAR.long.DF[[VAR.choice.figures$section.variable[VAR.which.figure]]]==as.character(VAR.sectionSTR)&
                                Graph==TRUE
                            &ContractorDisplayName!="Unranked Vendor"
                            &!is.na(Fiscal.Year)
        )
    }
    else{
        VAR.long.DF<-subset(VAR.long.DF,
                            Graph==TRUE
                            &ContractorDisplayName!="Unranked Vendor"
                            &!is.na(Fiscal.Year)
        )  
    }
    
    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in CreateTableView.",
            #       " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    
    
    
    
    if (VAR.choice.layout$page.layout[VAR.which.layout]=="single"){
        if (!(dev.cur()[[1]]==1)){
            dev.off()
        }
        note<-""
        
        #     top.count<-min(20,nrow(VAR.long.DF$ContractorDisplayName))
        top.count<-10    
        
        new_page(paste(VAR.choice.figures$section.variable[VAR.which.figure]
                       ,VAR.choice.figures$figure.number[VAR.which.figure]
                       ,"_","Top",top.count,"_",VAR.choice.figures$figure.title[VAR.which.figure],sep="")
                 ,VAR.choice.layout
                 , VAR.which.layout
                 , Var.OutputPath
                 , VAR.prefix
                 , VAR.sectionSTR            
                 , VAR.Layout)
        
        CreateCSV(
            Var.OutputPath 
            , VAR.prefix 
            , VAR.sectionSTR 
            ,paste("Top",top.count,"_",VAR.choice.figures$figure.title[VAR.which.figure],sep="")
            ,VAR.long.DF 
        ) 
    }
    if (VAR.choice.layout$page.layout[VAR.which.layout]=="grid"){
        #     top.count<-min(10,length(VAR.long.DF$ContractorDisplayName))
        top.count<-10
    }
    
    
    
    
    if (VAR.choice.layout$page.layout[which.layout]=="grid"){
        left.offset<-0
        toptable<-toptable+(
            grid.text(paste("Top",top.count,"Vendors")
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+top.count+2.5,"lines")
                      ,x=unit(VAR.choice.layout$table.title.x[VAR.which.layout],"npc")
                      ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            
        )  
    }
    else{
        left.offset<-4.25
        toptable<-toptable+(
            grid.text(paste("Top"
                            ,top.count
                            ,VAR.sectionSTR 
                            ,"Vendors,"
                            ,max(as.integer(format(VAR.long.DF$Fiscal.Year, "%Y")))-10
                            ,"and"
                            ,max(as.integer(format(VAR.long.DF$Fiscal.Year, "%Y"))))
                      ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]+top.count+3.5,"lines")
                      ,x=unit(VAR.choice.layout$table.title.x[VAR.which.layout],"npc")
                      ,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
            
        )  
    }
    #   debug(CreateTopVendorList)
    
    
    CreateTopVendorList(VAR.choice.layout
                        , VAR.which.layout
                        ,VAR.long.DF 
                        ,choice.figures$y.variable[VAR.which.figure]
                        ,VAR.row 
                        ,VAR.col 
                        ,top.count
                        ,max(max(as.integer(format(VAR.long.DF$Fiscal.Year, "%Y")))-10
                             ,min(as.integer(format(VAR.long.DF$Fiscal.Year, "%Y"))))
                        ,left.offset
    )
    
    if (VAR.choice.layout$page.layout[which.layout]=="single"){
        CreateTopVendorList(VAR.choice.layout
                            , VAR.which.layout
                            ,VAR.long.DF 
                            ,choice.figures$y.variable[VAR.which.figure]
                            ,VAR.row 
                            ,VAR.col 
                            ,top.count
                            ,max(as.integer(format(VAR.long.DF$Fiscal.Year, "%Y")))
                            ,left.offset+20
        )
    }
    
    toptable<-toptable+(
        grid.text("* Joint Venture" 
                  ,y=unit(VAR.choice.layout$base.y.line[VAR.which.layout]-3,"lines")
                  ,x=unit(VAR.choice.layout$name.column[VAR.which.layout]
                          ,"lines")
                  ,hjust=0,gp=gpar(fontface="bold",fontsize=VAR.choice.layout$table.fontsize[VAR.which.layout]))
        
    )
    
    Title(Categories
          , note,x.dimension
          ,VAR.choice.layout$page.layout[VAR.which.layout]
    )
    
    
    toptable<-toptable+(
        grid.text(paste("Source: FPDS and CSIS analysis;")
                  ,y=unit(1,"line")
                  ,x=unit(15,"line")
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain",fontsize=6))
        
    )  
    
    
    #   
    #   toptable<-toptable+(
    #     grid.text(paste("Available online at www.csis.org/NSPIR/DoD")
    #               ,y=unit(1,"line")
    #               ,x=unit(28,"line")
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain",fontsize=6))
    #     
    #   )  
    #   
    
    
    
}

CreateChart<-function(VAR.note
                      ,VAR.choice.layout
                      , VAR.which.layout
                      ,VAR.choice.data
                      ,VAR.which.data
                      ,Var.OutputPath
                      , VAR.prefix
                      , VAR.sectionSTR
                      ,VAR.Layout
                      ,VAR.Coloration
                      ,VAR.long.DF
                      , VAR.choice.figures
                      ,VAR.which.figure
                      ,VAR.row
                      ,VAR.col
                      , VAR.startyear=NA
){
    
    
    
    if (VAR.sectionSTR!="Overall"){
        VAR.long.DF<-subset(
            VAR.long.DF
            , VAR.long.DF[[VAR.choice.figures$section.variable[VAR.which.figure]]]==as.character(VAR.sectionSTR)
        )
    }
    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in CreateChart.",
            " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    if(!VAR.choice.figures$x.variable[VAR.which.figure] %in% names(VAR.long.DF)){
        stop (paste(
            VAR.choice.figures$x.variable[VAR.which.figure],
            "not found in VAR.long.DF."
        ))
    } 
    
    if (VAR.choice.figures$y.series[VAR.which.data] %in% c("SubCustomer.detail","SubCustomer.sum")){
        legend.title<-paste(VAR.choice.data$Legend.Customer[VAR.which.data]
                            ,VAR.choice.figures$legend.name[VAR.which.figure])
    } else{
        legend.title<-VAR.choice.figures$legend.name[VAR.which.figure]
    }
    
    if (!(is.na(VAR.startyear))){
        VAR.long.DF<-subset(VAR.long.DF,VAR.long.DF$Fiscal.Year>=as.Date(paste("9/30/",as.character(VAR.startyear),sep=""),"%m/%d/%Y"))
    }
    

    
    
    figure.title<-NULL
    if (!is.na(VAR.choice.figures$figure.title[VAR.which.figure])){
        if (VAR.sectionSTR=="Overall"){
            figure.title<-paste(VAR.choice.data$Title.Prefix[VAR.which.data]
                                ,VAR.choice.figures$figure.title[VAR.which.figure]
                                
            )
        } else {
            figure.title<-paste(VAR.sectionSTR
                                ,VAR.choice.figures$figure.title[VAR.which.figure]
            )
        }
        
    }
    
    #   if (VAR.choice.layout$page.layout[VAR.which.layout]=="single"){
    VAR.note<-""
    #     debug(LatticePlot)
    
    #Version with title
    if(VAR.choice.figures$form[VAR.which.figure]=="scatterplot"){
        if(nrow(subset(VAR.long.DF
                       ,!is.na(VAR.long.DF[,VAR.choice.figures$x.variable[VAR.which.figure]])
                       &!is.na(VAR.long.DF[,VAR.choice.figures$y.variable[VAR.which.figure]])))>0){
            new_page(
                paste(VAR.choice.figures$section.variable[VAR.which.figure]
                      ,VAR.choice.figures$figure.number[VAR.which.figure]
                      ,VAR.choice.figures$x.variable[VAR.which.figure]
                      ,VAR.choice.figures$y.variable[VAR.which.figure]
                      ,VAR.choice.figures$facet.primary[VAR.which.figure]
                      #           ,VAR.choice.figures$subset.variable[VAR.which.figure]
                      ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                      
                      ,"scatterplot"
                      ,sep="_"
                )
                #         VAR.choice.figures$figure.title[VAR.which.figure]
                ,VAR.choice.layout
                , VAR.which.layout
                ,Var.OutputPath
                , VAR.prefix
                , VAR.sectionSTR
                ,VAR.Layout
            )
            ScatterPlot(
                legend.title
                ,figure.title
                ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                ,VAR.Coloration
                ,VAR.row
                ,VAR.col
                ,VAR.long.DF
                ,VAR.choice.figures$x.variable[VAR.which.figure]
                ,VAR.choice.figures$y.variable[VAR.which.figure]
                ,VAR.choice.figures$facet.primary[VAR.which.figure]
                ,VAR.choice.figures$y.series[VAR.which.figure]
            )
        }
    }
    else if(VAR.choice.figures$form[VAR.which.figure] %in% c("density","histogram")){
        if(nrow(subset(VAR.long.DF
                       ,!is.na(VAR.long.DF[,VAR.choice.figures$x.variable[VAR.which.figure]])
                       #                    &!is.na(VAR.long.DF[,VAR.choice.figures$y.variable[VAR.which.figure]])
        )
        )>0){
            new_page(
                paste(VAR.choice.figures$section.variable[VAR.which.figure]
                      ,VAR.choice.figures$figure.number[VAR.which.figure]
                      ,VAR.choice.figures$x.variable[VAR.which.figure]
                      ,VAR.choice.figures$y.variable[VAR.which.figure]
                      ,VAR.choice.figures$facet.primary[VAR.which.figure]
                      #           ,VAR.choice.figures$subset.variable[VAR.which.figure]
                      ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                      
                      ,VAR.choice.figures$form[VAR.which.figure]
                      ,sep="_"
                )
                #         VAR.choice.figures$figure.title[VAR.which.figure]
                ,VAR.choice.layout
                , VAR.which.layout
                ,Var.OutputPath
                , VAR.prefix
                , VAR.sectionSTR
                ,VAR.Layout
            )
            HistogramOrDensity(
                legend.title
                ,figure.title
                ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                ,VAR.Coloration
                ,VAR.row
                ,VAR.col
                ,VAR.long.DF
                ,VAR.choice.figures$form[VAR.which.figure]
                ,VAR.choice.figures$x.variable[VAR.which.figure]
                ,VAR.choice.figures$y.series[VAR.which.figure]
                ,VAR.choice.figures$facet.primary[VAR.which.figure]
                ,VAR.choice.figures$facet.secondary[VAR.which.figure]
            )
        }
    }
    else if(VAR.choice.figures$form[VAR.which.figure]=="boxplot"){
        if(nrow(subset(VAR.long.DF
                       ,!is.na(VAR.long.DF[,VAR.choice.figures$x.variable[VAR.which.figure]])
                       &!is.na(VAR.long.DF[,VAR.choice.figures$y.variable[VAR.which.figure]])))>0){
            new_page(
                paste(VAR.choice.figures$section.variable[VAR.which.figure]
                      ,VAR.choice.figures$figure.number[VAR.which.figure]
                      ,VAR.choice.figures$x.variable[VAR.which.figure]
                      ,VAR.choice.figures$y.variable[VAR.which.figure]
                      ,VAR.choice.figures$facet.primary[VAR.which.figure]
                      #           ,VAR.choice.figures$subset.variable[VAR.which.figure]
                      ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                      
                      ,"boxplot"
                      ,sep="_"
                )
                #         VAR.choice.figures$figure.title[VAR.which.figure]
                ,VAR.choice.layout
                , VAR.which.layout
                ,Var.OutputPath
                , VAR.prefix
                , VAR.sectionSTR
                ,VAR.Layout
            )
            Boxplot(
                legend.title
                ,figure.title
                ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                ,VAR.Coloration
                ,VAR.row
                ,VAR.col
                ,VAR.long.DF
                ,VAR.choice.figures$x.variable[VAR.which.figure]
                ,VAR.choice.figures$y.variable[VAR.which.figure]
                ,VAR.choice.figures$facet.primary[VAR.which.figure]
            )
        }
    }
    #   else  if(VAR.choice.figures$form[VAR.which.figure]=="Lattice_Percent"){
    #   
    #     #Lattice percent line with title
    #     new_page(
    #       paste(VAR.choice.figures$section.variable[VAR.which.figure]
    #             ,VAR.choice.figures$figure.number[VAR.which.figure]
    #             ,VAR.choice.figures$y.series[VAR.which.figure]
    #             ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #             #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
    #             ,VAR.choice.figures$subset.criteria[VAR.which.figure]
    #             ,VAR.choice.figures$form[VAR.which.figure]
    #             ,sep="_"
    #       )
    #       #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
    #       ,VAR.choice.layout
    #       , VAR.which.layout
    #       ,Var.OutputPath
    #       , VAR.prefix
    #       , VAR.sectionSTR
    #       , VAR.Layout
    #     )
    #     #     debug(LatticePlot)
    #     LatticePercentLinePlot(
    #       legend.title
    #       ,figure.title
    #       ,VAR.choice.figures$x.axis.title[VAR.which.figure]
    #       ,VAR.choice.figures$y.axis.title[VAR.which.figure]
    #       ,VAR.Coloration
    #       ,VAR.row
    #       ,VAR.col
    #       ,VAR.long.DF
    #       ,NULL
    #       ,VAR.choice.figures$y.series[VAR.which.figure]
    #       ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #     )
    #     
    #     
    #     #Lattice percent line with no title
    #     new_page(
    #       paste(VAR.choice.figures$section.variable[VAR.which.figure]
    #             ,VAR.choice.figures$figure.number[VAR.which.figure]
    #             ,VAR.choice.figures$y.series[VAR.which.figure]
    #             ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #             #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
    #             ,VAR.choice.figures$subset.criteria[VAR.which.figure]
    #             ,"Lattice_Percent"
    #             ,sep="_"
    #       )
    #       #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
    #       ,VAR.choice.layout
    #       , VAR.which.layout
    #       ,paste(Var.OutputPath,"No Title\\",sep="")
    #       , VAR.prefix
    #       , VAR.sectionSTR
    #       , VAR.Layout
    #     )
    #     #     debug(LatticePlot)
    #     LatticePercentLinePlot(
    #       legend.title
    #       ,NULL #figure.title
    #       ,VAR.choice.figures$x.axis.title[VAR.which.figure]
    #       ,VAR.choice.figures$y.axis.title[VAR.which.figure]
    #       ,VAR.Coloration
    #       ,VAR.row
    #       ,VAR.col
    #       ,VAR.long.DF
    #       ,NULL
    #       ,VAR.choice.figures$y.variable[VAR.which.figure]
    #       ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #     )
    #     
    #   }
    else if(VAR.choice.figures$form[VAR.which.figure]=="Stacked_Bar"){
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$y.series[VAR.which.figure]
                  #           ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,VAR.choice.figures$form[VAR.which.figure]
                  ,sep="_"
            )
            #         VAR.choice.figures$figure.title[VAR.which.figure]
            ,VAR.choice.layout
            , VAR.which.layout
            ,Var.OutputPath
            , VAR.prefix
            , VAR.sectionSTR
            ,VAR.Layout
        )
        
        FullBarPlot(
            legend.title
            ,figure.title
            ,VAR.choice.figures$x.axis.title[VAR.which.figure]
            ,VAR.choice.figures$y.axis.title[VAR.which.figure]
            ,VAR.Coloration
            ,VAR.row
            ,VAR.col
            ,VAR.long.DF
            ,VAR.choice.figures$x.variable[VAR.which.figure]
            ,VAR.choice.figures$y.variable[VAR.which.figure]
            ,VAR.choice.figures$y.series[VAR.which.figure]
        )
        
        CreateCSV(
            Var.OutputPath 
            , VAR.prefix 
            , VAR.sectionSTR 
            ,paste(VAR.choice.figures$section.variable[VAR.which.figure]
                   ,VAR.choice.figures$y.series[VAR.which.figure]
                   #           ,VAR.choice.figures$subset.variable[VAR.which.figure]
                   ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                   ,sep="_"
            )    
            ,VAR.long.DF 
        ) 
        
        #Version with no title
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$y.series[VAR.which.figure]
                  #               ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,VAR.choice.figures$form[VAR.which.figure]
                  ,sep="_"
            )
            #         VAR.choice.figures$figure.title[VAR.which.figure]
            ,VAR.choice.layout
            , VAR.which.layout
            ,paste(Var.OutputPath,"No Title\\",sep="")
            , VAR.prefix
            , VAR.sectionSTR
            ,VAR.Layout
        )
        
        FullBarPlot(
            legend.title
            ,NULL #figure.title
            ,VAR.choice.figures$x.axis.title[VAR.which.figure]
            ,VAR.choice.figures$y.axis.title[VAR.which.figure]
            ,VAR.Coloration
            ,VAR.row
            ,VAR.col
            ,VAR.long.DF
            ,VAR.choice.figures$x.variable[VAR.which.figure]
            ,VAR.choice.figures$y.variable[VAR.which.figure]
            ,VAR.choice.figures$y.series[VAR.which.figure]
        )
        
        #Table output
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$y.series[VAR.which.figure]
                  #               ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,"Table"
                  ,sep="_"
            )
            #         paste(VAR.choice.figures$figure.title[VAR.which.figure],"table",sep="_")
            ,VAR.choice.layout
            ,  VAR.which.layout
            ,Var.OutputPath
            , VAR.prefix
            , VAR.sectionSTR
            ,VAR.Layout
        )
        
        TablePlot(
            legend.title
            ,figure.title
            ,VAR.choice.figures$x.axis.title[VAR.which.figure]
            ,VAR.choice.figures$y.axis.title[VAR.which.figure]
            ,VAR.Coloration
            ,VAR.row
            ,VAR.col
            ,VAR.long.DF
            ,VAR.choice.figures$x.variable[VAR.which.figure]
            ,VAR.choice.figures$y.variable[VAR.which.figure]
            ,VAR.choice.figures$y.series[VAR.which.figure]
        )
    }
    else  if(VAR.choice.figures$form[VAR.which.figure]=="Lattice_Bar"){
        
        #Detailed Lattice with title
        
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$facet.primary[VAR.which.figure]
                  ,VAR.choice.figures$facet.secondary[VAR.which.figure]
                  #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,VAR.choice.figures$form[VAR.which.figure]
                  ,sep="_"
            )
            
            #       paste(VAR.choice.figures$figure.title[VAR.which.figure]
            #                                ,"_detailed","_lattice",sep="")
            ,VAR.choice.layout
            , VAR.which.layout
            ,Var.OutputPath
            , VAR.prefix
            , VAR.sectionSTR
            , VAR.Layout
        )
        LatticePlot(legend.title
                    ,figure.title
                    ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                    ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                    ,VAR.Coloration
                    ,VAR.row
                    ,VAR.col
                    ,VAR.long.DF
                    ,NULL
                    ,VAR.choice.figures$x.variable[VAR.which.figure]
                    ,VAR.choice.figures$y.variable[VAR.which.figure]
                    ,VAR.choice.figures$y.series[VAR.which.figure]
                    ,VAR.choice.figures$facet.primary[VAR.which.figure]
                    ,VAR.choice.figures$facet.secondary[VAR.which.figure]
        )
        
        #Detailed Lattice no title
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  #           ,VAR.choice.figures$y.series[VAR.which.figure]
                  ,VAR.choice.figures$facet.primary[VAR.which.figure]
                  ,VAR.choice.figures$facet.secondary[VAR.which.figure]
                  #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,VAR.choice.figures$form[VAR.which.figure]
                  ,sep="_"
            )
            #       paste(VAR.choice.figures$figure.title[VAR.which.figure]
            #                                ,"_detailed","_lattice",sep="")
            ,VAR.choice.layout
            , VAR.which.layout
            ,paste(Var.OutputPath,"No Title\\",sep="")
            , VAR.prefix
            , VAR.sectionSTR
            , VAR.Layout
        )
        LatticePlot(legend.title
                    ,NULL #figure.title
                    ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                    ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                    ,VAR.Coloration
                    ,VAR.row
                    ,VAR.col
                    ,VAR.long.DF
                    ,NULL
                    ,VAR.choice.figures$x.variable[VAR.which.figure]
                    ,VAR.choice.figures$y.variable[VAR.which.figure]
                    ,VAR.choice.figures$y.series[VAR.which.figure]
                    ,VAR.choice.figures$facet.primary[VAR.which.figure]
                    ,VAR.choice.figures$facet.secondary[VAR.which.figure]
        )
        #   
        #   #Lattice Plot with title
        #   new_page(
        #     paste(VAR.choice.figures$section.variable[VAR.which.figure]
        #           ,VAR.choice.figures$figure.number[VAR.which.figure]
        #           ,VAR.choice.figures$x.variable[VAR.which.figure]
        #           #               ,VAR.choice.figures$subset.variable[VAR.which.figure]
        #           ,VAR.choice.figures$subset.criteria[VAR.which.figure]
        #           ,VAR.choice.figures$form[VAR.which.figure]
        #           ,sep="_"
        #     )
        #     ,VAR.choice.layout
        #     , VAR.which.layout
        #     ,Var.OutputPath
        #     , VAR.prefix
        #     , VAR.sectionSTR
        #     , VAR.Layout
        #   )
        #   #     debug(LatticePlot)
        #   LatticePlot(
        #     legend.title
        #     ,figure.title
        #     ,VAR.choice.figures$x.axis.title[VAR.which.figure]
        #     ,VAR.choice.figures$y.axis.title[VAR.which.figure]
        #     , VAR.Coloration
        #     ,VAR.row
        #     ,VAR.col
        #     ,VAR.long.DF
        #     ,NULL
        #     ,VAR.choice.figures$x.variable[VAR.which.figure]
        #   )
        #   
        #   #Lattice Plot with no title
        #   new_page(
        #     paste(VAR.choice.figures$section.variable[VAR.which.figure]
        #           ,VAR.choice.figures$figure.number[VAR.which.figure]
        #           ,VAR.choice.figures$x.variable[VAR.which.figure]
        #           ,VAR.choice.figures$form[VAR.which.figure]
        #           ,sep="_"
        #     )
        #     #         paste(VAR.choice.figures$figure.title[VAR.which.figure],"_lattice",sep="")
        #     ,VAR.choice.layout
        #     , VAR.which.layout
        #     ,paste(Var.OutputPath,"No Title\\",sep="")
        #     , VAR.prefix
        #     , VAR.sectionSTR
        #     , VAR.Layout
        #   )
        #   #     debug(LatticePlot)
        #   LatticePlot(
        #     legend.title
        #     ,NULL #figure.title
        #     ,VAR.choice.figures$x.axis.title[VAR.which.figure]
        #     ,VAR.choice.figures$y.axis.title[VAR.which.figure]
        #     , VAR.Coloration
        #     ,VAR.row
        #     ,VAR.col
        #     ,VAR.long.DF
        #     ,NULL
        #     ,VAR.choice.figures$x.variable[VAR.which.figure]
        #   )
    }
    else  if(VAR.choice.figures$form[VAR.which.figure]=="Lattice_Line"){
        #Percent with title
        #     new_page(
        #       paste(VAR.choice.figures$section.variable[VAR.which.figure]
        #             ,VAR.choice.figures$figure.number[VAR.which.figure]
        #             ,VAR.choice.figures$y.series[VAR.which.figure]
        #             ,VAR.choice.figures$facet.primary[VAR.which.figure]
        #             #               ,VAR.choice.figures$subset.variable[VAR.which.figure]
        #             ,VAR.choice.figures$subset.criteria[VAR.which.figure]
        #             ,VAR.choice.figures$form[VAR.which.figure]
        #             ,sep="_"
        #       )
        #       #         paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_lattice",sep="")
        #       ,VAR.choice.layout
        #       , VAR.which.layout
        #       ,Var.OutputPath
        #       , VAR.prefix
        #       , VAR.sectionSTR
        #       , VAR.Layout
        #     )
        #     debug(LatticePlot)
        #     LatticePercentLinePlot(
        #       legend.title
        #       ,figure.title0
        #       ,VAR.choice.figures$x.axis.title[VAR.which.figure]
        #       ,VAR.choice.figures$y.axis.title[VAR.which.figure]
        #       , VAR.Coloration
        #       ,VAR.row
        #       ,VAR.col
        #       ,VAR.long.DF
        #       ,NULL
        #       ,VAR.choice.figures$y.series[VAR.which.figure]
        #     )
        
        #Percent no title
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$y.series[VAR.which.figure]
                  #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$facet.primary[VAR.which.figure]
                  ,VAR.choice.figures$facet.secondary[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,VAR.choice.figures$form[VAR.which.figure]
                  ,sep="_"
            )
            #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
            ,VAR.choice.layout
            , VAR.which.layout
            ,Var.OutputPath
            , VAR.prefix
            , VAR.sectionSTR
            , VAR.Layout
        )
        LatticePercentLinePlot(
            legend.title
            ,figure.title
            ,VAR.choice.figures$x.axis.title[VAR.which.figure]
            ,VAR.choice.figures$y.axis.title[VAR.which.figure]
            ,VAR.Coloration
            ,VAR.row
            ,VAR.col
            ,VAR.long.DF
            ,NULL
            ,VAR.choice.figures$x.variable[VAR.which.figure]
            ,VAR.choice.figures$y.variable[VAR.which.figure]
            ,VAR.choice.figures$y.series[VAR.which.figure]
            ,VAR.choice.figures$facet.primary[VAR.which.figure]
            ,VAR.choice.figures$facet.secondary[VAR.which.figure]
        )
        
        
        #Lattice percent line with no title
        new_page(
            paste(VAR.choice.figures$section.variable[VAR.which.figure]
                  ,VAR.choice.figures$figure.number[VAR.which.figure]
                  ,VAR.choice.figures$y.series[VAR.which.figure]
                  ,VAR.choice.figures$facet.primary[VAR.which.figure]
                  ,VAR.choice.figures$facet.secondary[VAR.which.figure]
                  #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
                  ,VAR.choice.figures$subset.criteria[VAR.which.figure]
                  ,"Lattice_Percent"
                  ,sep="_"
            )
            #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
            ,VAR.choice.layout
            , VAR.which.layout
            ,paste(Var.OutputPath,"No Title\\",sep="")
            , VAR.prefix
            , VAR.sectionSTR
            , VAR.Layout
        )
        LatticePercentLinePlot(
            legend.title
            ,NULL #figure.title
            ,VAR.choice.figures$x.axis.title[VAR.which.figure]
            ,VAR.choice.figures$y.axis.title[VAR.which.figure]
            ,VAR.Coloration
            ,VAR.row
            ,VAR.col
            ,VAR.long.DF
            ,NULL
            ,VAR.choice.figures$x.variable[VAR.which.figure]
            ,VAR.choice.figures$y.variable[VAR.which.figure]
            ,VAR.choice.figures$y.series[VAR.which.figure]
            ,VAR.choice.figures$facet.primary[VAR.which.figure]
            ,VAR.choice.figures$facet.secondary[VAR.which.figure]
        )
        
    }
    
    #         #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
    #         ,VAR.choice.layout
    #         , VAR.which.layout
    #         ,Var.OutputPath
    #         , VAR.prefix
    #         , VAR.sectionSTR
    #         , VAR.Layout
    #       )
    #       #     debug(LatticePlot)
    #       LatticePercentLinePlot(
    #         legend.title
    #         ,figure.title
    #         ,VAR.choice.figures$x.axis.title[VAR.which.figure]
    #         ,VAR.choice.figures$y.axis.title[VAR.which.figure]
    #         ,VAR.Coloration
    #         ,VAR.row
    #         ,VAR.col
    #         ,VAR.long.DF
    #         ,NULL
    #         ,VAR.choice.figures$y.series[VAR.which.figure]
    #         ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #       )
    #       
    #       
    #       #Lattice percent line with no title
    #       new_page(
    #         paste(VAR.choice.figures$section.variable[VAR.which.figure]
    #               ,VAR.choice.figures$figure.number[VAR.which.figure]
    #               ,VAR.choice.figures$y.series[VAR.which.figure]
    #               ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #               #             ,VAR.choice.figures$subset.variable[VAR.which.figure]
    #               ,VAR.choice.figures$subset.criteria[VAR.which.figure]
    #               ,"Lattice_Percent"
    #               ,sep="_"
    #         )
    #         #       paste(VAR.choice.figures$figure.title[VAR.which.figure],"_percent_detailed_lattice",sep="")
    #         ,VAR.choice.layout
    #         , VAR.which.layout
    #         ,paste(Var.OutputPath,"No Title\\",sep="")
    #         , VAR.prefix
    #         , VAR.sectionSTR
    #         , VAR.Layout
    #       )
    #       #     debug(LatticePlot)
    #       LatticePercentLinePlot(
    #         legend.title
    #         ,NULL #figure.title
    #         ,VAR.choice.figures$x.axis.title[VAR.which.figure]
    #         ,VAR.choice.figures$y.axis.title[VAR.which.figure]
    #         ,VAR.Coloration
    #         ,VAR.row
    #         ,VAR.col
    #         ,VAR.long.DF
    #         ,NULL
    #         ,VAR.choice.figures$y.variable[VAR.which.figure]
    #         ,VAR.choice.figures$facet.primary[VAR.which.figure]
    #       )
    #       
    #     }
    #     
    
    #output the data to CSV (note, outputs everything)
    
    # 
    #  CreateCSV(Var.OutputPath 
    #           , VAR.prefix 
    #           , VAR.sectionSTR 
    #           , paste(VAR.choice.figures$section.variable[VAR.which.figure]
    #                   ,VAR.choice.figures$y.variable[VAR.which.figure]
    #                   ,VAR.choice.figures$x.variable[VAR.which.figure]
    #                   ,ifelse(VAR.choice.figures$x.variable[VAR.which.figure]==
    #                             VAR.choice.figures$y.variable[VAR.which.figure]
    #                           ,NA
    #                           ,VAR.choice.figures$y.variable[VAR.which.figure])
    #                   ,VAR.choice-.figures$facet.primary[VAR.which.figure]
    #                   #                      ,VAR.choice.figures$subset.variable[VAR.which.figure]
    #                   ,VAR.choice.figures$subset.criteria[VAR.which.figure]
    #                   ,sep="_"
    #           )
    #           ,VAR.long.DF 
    # )
    
    
    
    #note<-paste(note, CheckTotals(category.DF,master,"Customer"),sep="")
    #debug(MiniBarPlot)
    if (VAR.choice.layout$page.layout[VAR.which.layout]=="grid"){
        MiniBarPlot(legend.title
                    ,figure.title
                    ,VAR.choice.figures$x.axis.title[VAR.which.figure]
                    ,VAR.choice.figures$y.axis.title[VAR.which.figure]
                    ,VAR.Coloration
                    ,VAR.row,VAR.col
                    ,VAR.long.DF
                    ,VAR.choice.figures$x.variable[VAR.which.figure]
        )
    }
    while(!(dev.cur()[[1]]==1)){
        dev.off()
    }
    
    VAR.note
}



## If you want to source() a bunch of files, something like
## the following may be useful:
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}


label.offers <- function(df){
    if(is.null(df$Number_of_Offers_Received) | is.na(df$Number_of_Offers_Received) | df$Number_of_Offers_Received==0){
        offers<-"Null or Zero"
    }
    else if (df$Number_of_Offers_Received==1){
        offers<-"One"
    }
    else if (df$Number_of_Offers_Received>1){
        offers<-"Multiple"
    }
    else{
        offers<-"Error"
    }
    offers
    
    
}

subplot <- function(x, y){
    viewport(
        layout.pos.row = x,
        layout.pos.col = y
    )
}
#Functions


vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

Title<-function(VAR.Services_Categories,VAR.note,VAR.x.dimension,VAR.page.orientation){
    if(VAR.page.orientation=="landscape"){
        #    par(fig=c(0,1,max(par.yvals)+graph.height,1),mar=c(1,1,1,1),xpd=TRUE)
        #    mtext(VAR.Services_Categories$Description[[sectionNUM]],font=2,cex=title.text.size)
        
        #	par(fig=c(0,1,0,graph.bottom),
        #		mar=c(0,0,2,1),
        #		oma=c(0,0,0,0),
        #		xpd=TRUE)
        #	box(bty="o",col="White",ylim=c(0,100))
        
        #	text.height<-par("cxy")[[2]]
        text.height<-7
        #	baseY<-MaxY-(text.height*5)
        #	text(VAR.x.dimension*-0.45,baseY+text.height*5,"Source: Federal Procurement Data System; analysis by CSIS Defense-Industrial Initiatives Group.",font=1,cex=note.text.size,adj=c(0,0))
    }
    #text(VAR.x.dimension*-0.45,baseY+text.height*4,paste("Note: ","Due to differences in download date, contract vehicle totals may not match totals in other figures."),font=1,cex=note.text.size,adj=c(0,0))
    if(VAR.note!=""){
        #		text(VAR.x.dimension*-0.45,baseY+text.height*3,paste("Note:",VAR.note),font=1,cex=note.text.size,adj=c(0,0))
        toptable<-toptable+("Note")
        toptable<-toptable+(VAR.note)
    }
}

PercDiff<-function(VAR.first.value,VAR.facet.primary.value){
    if(VAR.first.value>=VAR.facet.primary.value){
        VAR.percent<-VAR.first.value/VAR.facet.primary.value-1
    }
    else if(VAR.first.value<VAR.facet.primary.value){
        VAR.percent<-VAR.facet.primary.value/VAR.first.value-1      
    }
    else {
        stop("ERROR in CompareTwoValues")
    }
    VAR.percent
}


ProcessVARCompare<-function(VAR.compare,name.first,name.second){
    VAR.compare$value.first[is.na(VAR.compare$value.first)]<-0
    VAR.compare$value.second[is.na(VAR.compare$value.second)]<-0
    VAR.compare$diff<-VAR.compare$value.first-VAR.compare$value.second
    VAR.compare$larger[VAR.compare$diff>0]<-name.first
    VAR.compare$larger[VAR.compare$diff<0]<-name.second
    VAR.compare$larger[VAR.compare$diff==0]<-"Match"
    VAR.compare$diff<-abs(VAR.compare$diff)
    VAR.compare$perc<-mapply(PercDiff,
                             VAR.first.value=VAR.compare$value.first,
                             VAR.facet.primary.value=VAR.compare$value.second
    )
    VAR.compare
}

roll_mean_3year_right_Value<-function(df){rollmean(na.fill(df$value,0),3,align="right",na.pad=TRUE)}

CompareTopline<-function(VAR.first.DF,name.first,VAR.facet.primary.DF,name.second,VAR.logfile){
    #   debug(PercDiff)
    if(!is.null(VAR.facet.primary.DF)){
        VAR.first.DF<-aggregate(VAR.first.DF$value
                                , by=list(VAR.first.DF$Fiscal.Year)
                                ,FUN = "sum"
                                ,na.rm =TRUE
        )
        names(VAR.first.DF)<-c(
            "Fiscal.Year"
            ,"value.first"
        )
        VAR.facet.primary.DF<-aggregate(
            VAR.facet.primary.DF$value
            , by=list(VAR.facet.primary.DF$Fiscal.Year)
            ,FUN = "sum"
            ,na.rm =TRUE
        )
        names(VAR.facet.primary.DF)<-c("Fiscal.Year"
                                       ,"value.second"
        )
        VAR.compare<-join(VAR.first.DF
                          , VAR.facet.primary.DF
                          , by = c("Fiscal.Year")
                          , type = "full"
                          , match = "all"
        )
        VAR.compare<-ProcessVARCompare(VAR.compare
                                       ,name.first
                                       ,name.second
        )
        VAR.compare$text<-paste("\n\t"
                                ,format(VAR.compare$Fiscal.Year,"%Y")
                                ," varied by "
                                ,"$"
                                ,round(VAR.compare$diff*500,3)
                                ,"M"
                                ," ("
                                ,VAR.compare$larger
                                ," was larger by "
                                ,round(VAR.compare$perc*100,3)
                                ,"%)"
                                ,sep=""
        )
        #   " (",round(VAR.compare$perc*100,3),"%)",
        #   CompareTwoValues(name.first,name.second),
        #Theshold set at 0.1% of the average total by year.
        threshold<-min(sum(VAR.compare$value.first)
                       ,sum(VAR.compare$value.second))/500/length(unique(VAR.compare$Fiscal.Year))
        VAR.compare<-subset(VAR.compare
                            ,VAR.compare$diff>threshold)
        if(nrow(VAR.compare)>0){
            warning_text<-paste(VAR.compare$text
                                , collapse="; ")
            warning(paste(
                "The same years a different value for ",
                name.first
                ," and "
                ,name.second
                ,"(Warning threshold: "
                ,round(threshold*500,2)
                ,"M). ",
                
                warning_text,
                sep=""
            ))
            VAR.compare<-data.frame("Total"
                                    ,VAR.compare)
            LogCompare(VAR.compare
                       ,name.first
                       ,name.second
                       ,VAR.logfile)
        }
    }
}

LogCompare<-function(VAR.output,VAR.name.first,VAR.name.second,VAR.logfile){
    names(VAR.output)<-c("Section Name","Fiscal.Year",VAR.name.first,VAR.name.second,"Difference","Larger","Percent","Text")
    write.table(VAR.output, file=VAR.logfile, sep=",", row.names=FALSE, append=TRUE)
}


CompareSections<-function(VAR.first.DF,name.first,VAR.facet.primary.DF,name.second,VAR.logfile){
    #   debug(CompareTopline)
    if(!is.null(VAR.facet.primary.DF)){
        CompareTopline(VAR.first.DF,name.first,VAR.facet.primary.DF,name.second,VAR.logfile)
        VAR.first.DF<-aggregate(VAR.first.DF$value, by=list(VAR.first.DF[[choice.figures$section.variable[VAR.which.figure]]], VAR.first.DF$Fiscal.Year),FUN = "sum")
        names(VAR.first.DF)<-c("Section","Fiscal.Year","value.first")
        VAR.facet.primary.DF<-aggregate(VAR.facet.primary.DF$value, by=list(VAR.facet.primary.DF[[choice.figures$section.variable[VAR.which.figure]]], VAR.facet.primary.DF$Fiscal.Year),FUN = "sum")
        names(VAR.facet.primary.DF)<-c(choice.figures$section.variable[VAR.which.figure],"Fiscal.Year","value.second")
        VAR.compare<-join(VAR.first.DF, VAR.facet.primary.DF, by = c(choice.figures$section.variable[VAR.which.figure],"Fiscal.Year"),
                          type = "full", match = "all")
        VAR.compare<-ProcessVARCompare(VAR.compare,name.first,name.second)
        VAR.compare<-transform(VAR.compare,text=paste("\n\t",
                                                      VAR.compare[[choice.figures$section.variable[VAR.which.figure]]]," in ",format(VAR.compare$Fiscal.Year,"%Y"),
                                                      " varied by ",
                                                      "$",round(VAR.compare$diff*500,3),"M",
                                                      " (",VAR.compare$larger," was larger by ",
                                                      round(VAR.compare$perc*100,3),"%)",
                                                      sep=""
        ))
        #Theshold set at 0.1% of the average total by year.
        threshold<-min(sum(VAR.compare$value.first),sum(VAR.compare$value.second))/500/length(unique(VAR.compare$Fiscal.Year))
        VAR.compare<-subset(VAR.compare,VAR.compare$diff>threshold)
        if(nrow(VAR.compare)>0){
            warning_text<-paste(VAR.compare$text, collapse="; ")
            warning(paste(
                "The same section had a different value for ",
                name.first," and ",name.second,"(Warning threshold: ",round(threshold*500,2),"M). ",
                
                warning_text,
                sep=""
            ))
            LogCompare(VAR.compare,name.first,name.second,VAR.logfile)
        }
    }
}

CompareVariables<-function(VAR.first.DF,name.first,VAR.facet.primary.DF,name.second,VAR.logfile){
    CompareTopline(VAR.first.DF
                   ,name.first
                   ,VAR.facet.primary.DF,name.second
                   ,VAR.logfile
    )
    VAR.first.DF<-aggregate(VAR.first.DF$value
                            , by=list(VAR.first.DF$variable
                                      , VAR.first.DF$Fiscal.Year)
                            ,FUN = "sum"
                            ,na.rm =TRUE
    )
    names(VAR.first.DF)<-c("variable"
                           ,"Fiscal.Year"
                           ,"value.first"
    )
    VAR.facet.primary.DF<-aggregate(VAR.facet.primary.DF$value
                                    , by=list(VAR.facet.primary.DF$variable
                                              , VAR.facet.primary.DF$Fiscal.Year)
                                    ,FUN = "sum"
                                    ,na.rm =TRUE
    )
    names(VAR.facet.primary.DF)<-c("variable"
                                   ,"Fiscal.Year"
                                   ,"value.second"
    )
    VAR.compare<-join(VAR.first.DF, VAR.facet.primary.DF
                      , by = c("variable"
                               ,"Fiscal.Year")
                      , type = "full"
                      , match = "all"
    )
    VAR.compare<-ProcessVARCompare(VAR.compare
                                   ,name.first,name.second
    )
    VAR.compare<-transform(VAR.compare,text=paste("\n\t'",
                                                  VAR.compare$variable,"' in "
                                                  ,format(VAR.compare$Fiscal.Year,"%Y")
                                                  ," varied by "
                                                  ,"$"
                                                  ,round(VAR.compare$diff*500,3)
                                                  ,"M"
                                                  ," ("
                                                  ,VAR.compare$larger
                                                  ," was larger by "
                                                  ,round(VAR.compare$perc*100,3)
                                                  ,"%)"
                                                  , sep=""
    ))
    #Theshold set at 0.1% of the average total by year.
    threshold<-min(sum(VAR.compare$value.first)
                   ,sum(VAR.compare$value.second)
    )/500/length(unique(VAR.compare$Fiscal.Year))
    VAR.compare<-subset(VAR.compare
                        ,VAR.compare$diff>threshold)
    if(nrow(VAR.compare)>0){
        warning_text<-paste(VAR.compare$text, collapse="; ")
        warning(paste(
            "The same variable had a different value for ",
            name.first," and ",name.second,"(Warning threshold: ",round(threshold*500,2),"M). ",
            warning_text,
            sep=""
        ))
        LogCompare(VAR.compare,name.first,name.second,VAR.logfile)
    }
    #   
    #   
    #   threshold<-min(0.0001,VAR.compare$value.first/1000,VAR.compare$value.second/1000)
    #   if(max(VAR.compare$diff)>0.0001||max(VAR.compare$perc)>0.001){
    #     warning(paste(
    #       "The same variable had a different value for ",name.first," and ",name.second,". ",
    #       "Maximum difference by value: $",round(max(VAR.compare$diff)*1000,3),"M; ",
    #       "Maximum difference by percentage: ",round(max(VAR.compare$perc)*100,3),"%; ",
    #       sep=""
    #     ))
    #   }
}



CompareAbstractVariables<-function(VAR.first.DF
                                   ,name.first
                                   ,VAR.facet.primary.DF
                                   ,name.second
                                   ,variable
                                   ,VAR.logfile
){
    #   debug(LimitScope)
    if(!is.null(VAR.facet.primary.DF)){
        CompareTopline(VAR.first.DF
                       ,name.first
                       ,VAR.facet.primary.DF
                       ,name.second
                       ,VAR.logfile
        )
        VAR.first.DF<-aggregate(VAR.first.DF$value
                                , by=list(VAR.first.DF[[variable]]
                                          , VAR.first.DF$Fiscal.Year)
                                ,FUN = "sum"
                                ,na.rm =TRUE
        )
        names(VAR.first.DF)<-c(variable
                               ,"Fiscal.Year"
                               ,"value.first"
        )
        VAR.facet.primary.DF<-aggregate(VAR.facet.primary.DF$value
                                        , by=list(VAR.facet.primary.DF[[variable]]
                                                  , VAR.facet.primary.DF$Fiscal.Year)
                                        ,FUN = "sum"
                                        ,na.rm =TRUE
        )
        names(VAR.facet.primary.DF)<-c(variable
                                       ,"Fiscal.Year"
                                       ,"value.second"
        )
        VAR.compare<-join(VAR.first.DF, VAR.facet.primary.DF
                          , by = c(variable
                                   ,"Fiscal.Year")
                          , type = "full"
                          , match = "all"
        )
        VAR.compare<-ProcessVARCompare(VAR.compare
                                       ,name.first,name.second
        )
        VAR.compare<-transform(VAR.compare,text=paste("\n\t'",
                                                      VAR.compare[variable,],"' in "
                                                      ,format(VAR.compare$Fiscal.Year,"%Y")
                                                      ," varied by "
                                                      ,"$"
                                                      ,round(VAR.compare$diff*500,3)
                                                      ,"M"
                                                      ," ("
                                                      ,VAR.compare$larger
                                                      ," was larger by "
                                                      ,round(VAR.compare$perc*100,3)
                                                      ,"%)"
                                                      , sep=""
        ))
        #Theshold set at 0.1% of the average total by year.
        threshold<-min(sum(VAR.compare$value.first)
                       ,sum(VAR.compare$value.second)
        )/500/length(unique(VAR.compare$Fiscal.Year))
        VAR.compare<-subset(VAR.compare
                            ,VAR.compare$diff>threshold)
        if(nrow(VAR.compare)>0){
            warning_text<-paste(VAR.compare$text, collapse="; ")
            warning(paste(
                "The same variable had a different value for "
                , name.first
                ," and "
                ,name.second
                ,"(Warning threshold: "
                ,round(threshold*500,2),"M). ",
                warning_text,
                sep=""
            ))
            LogCompare(VAR.compare,name.first,name.second,VAR.logfile)
        }
        #   
        #   
        #   threshold<-min(0.0001,VAR.compare$value.first/1000,VAR.compare$value.second/1000)
        #   if(max(VAR.compare$diff)>0.0001||max(VAR.compare$perc)>0.001){
        #     warning(paste(
        #       "The same variable had a different value for ",name.first," and ",name.second,". ",
        #       "Maximum difference by value: $",round(max(VAR.compare$diff)*1000,3),"M; ",
        #       "Maximum difference by percentage: ",round(max(VAR.compare$perc)*100,3),"%; ",
        #       sep=""
        #     ))
        #   }
    }
}

CheckTotals<-function(VAR.y.series.DF,VAR.matrix,VAR.color.legend.label){
    result<-""
    runningtotal<-0
    for(i in 1:nrow(VAR.y.series.DF)) {
        checksum<-Total[i]
        for(j in 1:nrow(VAR.matrix)){
            checksum<-checksum-VAR.matrix[j,i]
        }
        if(abs(checksum)>0.00001){
            result<-paste(result,Fiscal.Year[i],":",format(round(checksum,2),trim=TRUE, big.mark=","))
        }
        runningtotal<-runningtotal+abs(checksum)
    }(paste(VAR.color.legend.label," running total:",format(round(runningtotal,6), trim=TRUE, big.mark=",")))
    if(abs(runningtotal)>0.0001){
        result<-paste(VAR.color.legend.label,result)
    }
    rm(runningtotal,i,j,checksum)
    result
    
}


Label_Wrap <- function(variable, value) {
    #Taken from https://github.com/hadley/ggplot2/wiki/labeller
    lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
           paste, collapse="\n")
}  

PrepareLabelsAndColors<-function(VAR.Coloration
                                 ,VAR.long.DF
                                 ,VAR.y.series
                                 #                                  ,VAR.override.coloration=NA
)
{
    
    
    
    #Confirm that the category is even available in the data set.
    if(!VAR.y.series %in% names(VAR.long.DF)){
        stop(paste(VAR.y.series,"is not found in data frame passed to PrepareLabelsAndColors"))
    }
    
    
    #Translate the category name into the appropriate coloration.key
    #This is used because we have more category names than coloration.key
    Coloration.Key<-read.csv(
        paste(Path,"Lookups\\","lookup_coloration_key.csv",sep=""),
        header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE, 
        stringsAsFactors=FALSE
    )
    Coloration.Key<-subset(Coloration.Key, category==VAR.y.series)  
    
    if(nrow(Coloration.Key)==0){
        stop(paste(VAR.y.series,"is missing from Lookup_Coloration.Key.csv"))
    }
    
    
    #Limit the lookup table to those series that match the variable   
    labels.category.DF<-subset(VAR.Coloration, coloration.key==Coloration.Key$coloration.key[1] )
    
    #Fix oddities involving text
    labels.category.DF$variable <- gsub("\\\\n","\n",labels.category.DF$variable)
    labels.category.DF$Label <- gsub("\\\\n","\n",labels.category.DF$Label)
    
    #Check for any values in the VAR.y.series field that are not assigned a color.
    NA.labels<-subset(VAR.long.DF,!(VAR.long.DF[,VAR.y.series] %in% labels.category.DF$variable))
    
    if (nrow(NA.labels)>0){
        print(unique(NA.labels[,VAR.y.series]))
        stop(paste("Lookup_Coloration.csv is missing"
                   ,length(unique(NA.labels[,VAR.y.series]))
                   ,"label(s) for category="
                   ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
        )
    } 
    rm(NA.labels,Coloration.Key)
    
    
    
    names.DF<-subset(labels.category.DF
                     , variable %in% unique(VAR.long.DF[,VAR.y.series]))
    
    rm(labels.category.DF)
    
    #Order the names.DF and then pass on the same order to the actual data in VAR.long.DF
    names.DF<-names.DF[order(names.DF$Display.Order),]
    
    #         names.DF$colorRGB<-rgb(red=names.DF$r,green=r,names.DF$g,blue=names.DF$b)
    
    #When there is an override color, use it instead of the default.
    #   if (!is.na(VAR.override.coloration)&VAR.y.series!=VAR.override.coloration){
    #     Coloration.Key<-read.csv(
    #       paste(Path,"Lookups\\","lookup_coloration_key.csv",sep=""),
    #       header=TRUE, sep=",", na.strings="", dec=".", strip.white=TRUE, 
    #       stringsAsFactors=FALSE
    #     )
    #     
    #     
    #     Coloration.Key<-subset(Coloration.Key, category==VAR.override.coloration)
    #     
    #     if(nrow(Coloration.Key)==0){
    #       stop(paste(VAR.override.coloration,"is missing from Lookup_Coloration.Key.csv"))
    #     }
    #     
    #     #Limit the lookup table to those series that match the variable
    #     labels.category.DF<-subset(VAR.Coloration, coloration.key==Coloration.Key$coloration.key[1] )
    #     
    #     #Check for any values in the VAR.y.series field that are not assigned a color.
    #     NA.labels<-subset(VAR.long.DF,!(VAR.long.DF[,VAR.override.coloration] %in% labels.category.DF$variable))
    #     if (nrow(NA.labels)>0){
    #       print(unique(NA.labels[,VAR.override.coloration]))
    #       stop(paste("Lookup_Coloration.csv is missing"
    #                  ,length(unique(NA.labels[,VAR.override.coloration]))
    #                  ,"label(s) for override.coloration="
    #                  ,Coloration.Key$coloration.key[1], ". See above for a list of missing labels")
    #       )
    #     } 
    #     
    #     
    #     category.to.coloring.DF<-unique(
    #       data.frame(
    #         category=as.character(VAR.long.DF[,VAR.y.series]),
    #         variable=VAR.long.DF[,VAR.override.coloration],
    #         stringsAsFactors=FALSE)
    #     )
    #     names.DF<-subset(names.DF
    #                      , select=-c(Color
    #                                  , C
    #                                  ,  M
    #                                  ,  Y
    #                                  ,	K
    #                                  ,	R
    #                                  ,	G
    #                                  ,	B
    #                                  , ColorRGB
    #                      ))
    #     colors.DF<-subset(labels.category.DF
    #                       , variable %in% unique(VAR.long.DF[,VAR.override.coloration])
    #                       , select=-c(coloration.key
    #                                   ,Label
    #                                   ,Display.Order
    #                       ))
    #     colors.DF<-join(
    #       category.to.coloring.DF,
    #       colors.DF,
    #       match="first"
    #     )
    #     
    #     names(colors.DF)<-c("coloring","variable","Color",  "C",  "M",  "Y",  "K",  "R",  "G",	"B", "ColorRGB")
    #     
    #     names.DF<-join(
    #       names.DF,
    #       colors.DF,
    #       match="first"
    #     )
    #     
    #     
    #     rm(colors.DF,labels.category.DF)
    #   }
    
    
    
    names.DF
}







LatticePlot<-function(VAR.color.legend.label
                      ,VAR.main.label
                      ,VAR.X.label
                      ,VAR.Y.label
                      ,VAR.Coloration
                      ,VAR.base.row
                      ,VAR.base.col
                      ,VAR.long.DF
                      ,VAR.ncol
                      ,VAR.x.variable
                      ,VAR.y.variable
                      ,VAR.y.series
                      ,VAR.facet.primary
                      ,VAR.facet.secondary=NA
                      #                       ,VAR.override.coloration=NA
){
    #     debug(PrepareLabelsAndColors)
    
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    if(is.na(VAR.y.series)) VAR.y.series<-VAR.facet.primary
    
    #Prepare labels for the category variable
    #   if(is.na(VAR.override.coloration)){
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
                                               #                                     ,VAR.override.coloration
    )  
    #   }
    #   else{
    #     labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
    #                                       ,VAR.long.DF
    #                                       ,VAR.override.coloration
    #     )  
    #   }
    
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    
    
    
    old.theme<-theme_set(theme_grey())
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.facet.primary column.
    if(is.na(VAR.facet.secondary)){
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=list(VAR.long.DF[,VAR.x.variable]
                                         ,VAR.long.DF[,VAR.y.series]
                                         ,VAR.long.DF[,VAR.facet.primary]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","primary","y.variable")
        VAR.long.DF<-ddply(VAR.long.DF,
                           .(x.variable,primary),
                           mutate,
                           ytextposition=cumsum(y.variable)-0.5*y.variable)#.(Fiscal.Year)
        
    }
    else{
        labels.secondary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                    ,VAR.long.DF
                                                    ,VAR.facet.secondary)  
        
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=list(VAR.long.DF[,VAR.x.variable]
                                         ,VAR.long.DF[,VAR.y.series]
                                         ,VAR.long.DF[,VAR.facet.primary]
                                         ,VAR.long.DF[,VAR.facet.secondary]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","primary","secondary","y.variable")
        
        VAR.long.DF$secondary<-factor(VAR.long.DF$secondary
                                      ,levels=c(labels.secondary.DF$variable)
                                      ,labels=c(labels.secondary.DF$Label)
                                      ,ordered=TRUE)
        rm(labels.secondary.DF)
        
    }
    VAR.long.DF$primary=factor(VAR.long.DF$primary
                               ,levels=c(labels.category.DF$variable)
                               ,labels=c(labels.category.DF$Label)
                               ,ordered=TRUE
    )
    
    
    original<-qplot(
        format(x.variable,"%Y")
        , y=y.variable
        , data=VAR.long.DF
        , ylab=VAR.Y.label
        , main=VAR.main.label
        , xlab=VAR.X.label
        , geom="bar"
        , stat="identity"
        , fill=factor(category,levels=labels.category.DF$variable),
    )#+ geom_bar(stat="identity")
    
    
    tick.marks<-2
    
    print.figure<-original+scale_x_discrete(
        breaks=
            c(seq(
                as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
                as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
                by=tick.marks)),
        labels=
            paste("'",format(as.Date(as.character(
                c(seq(
                    as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
                    as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
                    by=tick.marks))
            ),"%Y"),"%y"),sep="")  
    )
    
    #   print.figure<-print.figure+geom_bar(
    #     colour="black",
    #     stat = "identity",
    #     property= "identity"
    #   )
    
    #, labels=c(labels.category.DF$Label) Section labels don't work with facets.
    #  http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
    
    print.figure<-print.figure+scale_fill_manual(
        VAR.color.legend.label
        ,  values=color.list
        , breaks=c(labels.category.DF$variable)
        
    ) 
    
    
    
    
    if(is.na(VAR.facet.secondary)){
        
        original<-original+facet_wrap(~ primary
                                              #                                           ,ncol=VAR.ncol
                                              #                                           , labeller=Label_Wrap
                                              #                                           , scales="fixed", space="free_y"
        )+scale_y_continuous(labels=comma)
        # +scale_y_continuous(expand=c(0,0.75)#)+scale_y_continuous(expand=c(0,0.75)
        #     )
        
        #Drop the labeling detail for crowded graphs.
        NumericalDetail<-1
        if(nrow(VAR.long.DF)>50){ NumericalDetail<-0 }
        print.figure<-print.figure+
            geom_text(aes(label=VariableNumericalFormat(y.variable,NumericalDetail)
                          #                     format(round(y.variable,3),  scientific=FALSE, trim=TRUE, big.mark=",")
                          #                   format(y.variable, digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
                          #apply(y.variable,VariableNumericalFormat)
                          ,y=ytextposition)
                      ,size=geom.text.size
                      ,hjust=0.5
                      ,vjust=0.5
                      #,color=color.list This doesn't work yet
            )
    }
    else{
        print.figure<-print.figure+facet_grid(primary ~ secondary
                                              , labeller=Label_Wrap
                                              , scales="free_y" #The scales actually do stay fixed
                                              , space="free_y"#But only because the space is free
        )+scale_y_continuous(expand=c(0,0.75)
                             ,labels=comma
        )+theme(strip.text.y=element_text(size=axis.text.size,family="times",face="bold",angle=0)
        )
        
    }
    
    #   
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(strip.text.x=element_text(size=strip.text.size,face="bold"))+
        theme(strip.text.y=element_text(size=strip.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.position="none")+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))
    #     theme(legend.key.width=unit(0.1,"npc"))
    #   print.figure<-facetAdjust(print.figure,"down")
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    theme_set(old.theme)
    rm(VAR.color.legend.label
       ,VAR.main.label
       ,VAR.base.row
       ,VAR.base.col
       ,original
       ,print.figure
       ,old.theme
    )
}

LatticePlotWrapper<-function(VAR.color.legend.label
                             ,VAR.main.label
                             ,VAR.X.label
                             ,VAR.Y.label
                             ,VAR.Coloration
                             ,VAR.long.DF
                             ,VAR.ncol=NA
                             ,VAR.x.variable
                             ,VAR.y.variable
                             ,VAR.y.series
                             ,VAR.facet.primary
                             ,VAR.facet.secondary=NA
                             ,MovingAverage=1
                             ,MovingSides=1
                             ,DataLabels=NA
                             #                       ,VAR.override.coloration=NA
){
    #     debug(PrepareLabelsAndColors)
    
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    if(is.na(VAR.y.series)) VAR.y.series<-VAR.facet.primary
    
    #Prepare labels for the category variable
    #   if(is.na(VAR.override.coloration)){
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
                                               #                                     ,VAR.override.coloration
    )  
    
    labels.primary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                              ,VAR.long.DF
                                              ,VAR.facet.primary
                                              #                                     ,VAR.override.coloration
    )
    #   }
    #   else{
    #     labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
    #                                       ,VAR.long.DF
    #                                       ,VAR.override.coloration
    #     )  
    #   }
    
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    
    
    old.theme<-theme_set(theme_grey())
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.facet.primary column.
    if(is.na(VAR.facet.secondary)){
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=list(VAR.long.DF[,VAR.x.variable]
                                         ,VAR.long.DF[,VAR.y.series]
                                         ,VAR.long.DF[,VAR.facet.primary]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","primary","y.variable")
        
        if(is.numeric(MovingAverage) & MovingAverage>1){
            VAR.long.DF$y.raw<-VAR.long.DF$y.variable
            VAR.long.DF<-ddply(VAR.long.DF, 
                               .(category,primary),
                               .fun=TransformFilter,
                               MovingAverage,
                               MovingSides
            )
            VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
        }
        
        VAR.long.DF<-ddply(VAR.long.DF,.(x.variable,primary),mutate,ytextposition=cumsum(y.variable)-0.5*y.variable)#.(Fiscal.Year)
        
    }
    else{
        labels.secondary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                    ,VAR.long.DF
                                                    ,VAR.facet.secondary)  
        
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=list(VAR.long.DF[,VAR.x.variable]
                                         ,VAR.long.DF[,VAR.y.series]
                                         ,VAR.long.DF[,VAR.facet.primary]
                                         ,VAR.long.DF[,VAR.facet.secondary]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","primary","secondary","y.variable")
        
        if(is.numeric(MovingAverage) & MovingAverage>1){
            VAR.long.DF$y.raw<-VAR.long.DF$y.variable
            VAR.long.DF<-ddply(VAR.long.DF, 
                               .(category,primary,secondary),
                               .fun=TransformFilter,
                               MovingAverage,
                               MovingSides
            )
            VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
        }
        
        VAR.long.DF$secondary<-factor(VAR.long.DF$secondary
                                      ,levels=c(labels.secondary.DF$variable)
                                      ,labels=c(labels.secondary.DF$Label)
                                      ,ordered=TRUE)
        rm(labels.secondary.DF)
        
    }
    
    VAR.long.DF$category<-factor(VAR.long.DF$category,levels=c(labels.category.DF$variable),
                                 labels=c(labels.category.DF$Label),
                                 ordered=TRUE)
    VAR.long.DF$primary<-factor(VAR.long.DF$primary,
                                levels=c(labels.primary.DF$variable),
                                labels=c(labels.primary.DF$Label),
                                ordered=TRUE)
    
    
    if(class(VAR.long.DF$x.variable)=="Date"){
        VAR.long.DF$x.variable<-format(VAR.long.DF$x.variable,"%Y")
    }
    
    original<-qplot(
        x=x.variable
        , y=y.variable
        , data=VAR.long.DF
        , ylab=VAR.Y.label
        , main=VAR.main.label
        , xlab=VAR.X.label
        , geom="bar"
        , stat="identity"
        , fill=factor(category,levels=labels.category.DF$variable),
    )#+ geom_bar(stat="identity")
    
    
    tick.marks<-2
    print.figure<-original
    if(class(VAR.long.DF$x.variable)=="Date"){
        print.figure<-print.figure+scale_x_discrete(
            breaks=
                c(seq(
                    as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
                    as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
                    by=tick.marks)),
            labels=
                paste("'",format(as.Date(as.character(
                    c(seq(
                        as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
                        as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
                        by=tick.marks))
                ),"%Y"),"%y"),sep="")  
        )
    }
    
    #   print.figure<-print.figure+geom_bar(
    #     colour="black",
    #     stat = "identity",
    #     property= "identity"
    #   )
    
    #, labels=c(labels.category.DF$Label) Section labels don't work with facets.
    #  http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
    
    print.figure<-print.figure+scale_fill_manual(
        VAR.color.legend.label
        ,  values=color.list
        , breaks=c(labels.category.DF$variable)
        
    ) 
    
    
    
    
    if(is.na(VAR.facet.secondary)){
        if(!is.na(VAR.ncol)){
            print.figure<-print.figure+facet_wrap(~ primary
                                                  ,ncol=VAR.ncol
                                                  #                                           , labeller=Label_Wrap
                                                  #                                           , scales="fixed", space="free_y"
            )+scale_y_continuous(labels=comma)    
        }
        else{
            print.figure<-print.figure+facet_wrap(~ primary
                                                  #                                           ,ncol=VAR.ncol
                                                  #                                           , labeller=Label_Wrap
                                                  #                                           , scales="fixed", space="free_y"
            )+scale_y_continuous(labels=comma)
        }
        # +scale_y_continuous(expand=c(0,0.75)#)+scale_y_continuous(expand=c(0,0.75)
        #     )
        
        
        #Don't add numbers at all if there's over 10 facets or 500 rows
        if(isTRUE(DataLabels) |
               (is.na(DataLabels) &
                    length(levels(VAR.long.DF$primary))<=10 & nrow(VAR.long.DF)<200
               )){
            #Drop the labeling detail for crowded graphs.
            NumericalDetail<-1
            if(nrow(VAR.long.DF)>50){ NumericalDetail<-0 }
            print.figure<-print.figure+
                geom_text(aes(label=VariableNumericalFormat(y.variable,NumericalDetail)
                              #                     format(round(y.variable,3),  scientific=FALSE, trim=TRUE, big.mark=",")
                              #                   format(y.variable, digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
                              #apply(y.variable,VariableNumericalFormat)
                              ,y=ytextposition)
                          ,size=geom.text.size
                          ,hjust=0.5
                          ,vjust=0.5
                          #,color=color.list This doesn't work yet
                )
        }
    }
    else{
        print.figure<-print.figure+facet_grid(primary ~ secondary
                                              , labeller=Label_Wrap
                                              , scales="free_y" #The scales actually do stay fixed
                                              , space="free_y"#But only because the space is free
        )+scale_y_continuous(expand=c(0,0.75)
                             ,labels=comma
        )+theme(strip.text.y=element_text(size=axis.text.size,family="times",face="bold",angle=0)
        )
        
    }
    
    #   
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(strip.text.x=element_text(size=strip.text.size,face="bold"))+
        theme(strip.text.y=element_text(size=strip.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.position="none")+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))
    #     theme(legend.key.width=unit(0.1,"npc"))
    #   print.figure<-facetAdjust(print.figure,"down")
    
    print.figure
    
    
}

PointRowWrapper<-function(VAR.main.label,
                          VAR.row.label,
                          VAR.data.label,
                          VAR.color.legend.label,
                          VAR.size.legend.label,
                          VAR.Coloration,
                          VAR.long.DF,
                          VAR.row.variable,
                          VAR.value.variable,
                          VAR.series.variable,
                          VAR.size.variable,
                          VAR.facet.primary,
                          VAR.facet.secondary=NA,
                          low=NA,
                          high=NA,
                          Percentage=NA){
    #Single Offer Competition
    
    if(!is.na(low) | !is.na(high)){
        VAR.long.DF<-SetExtremesToInf(VAR.long.DF,VAR.value.variable,low,high)
    }
    
    figure<-ggplot(VAR.long.DF,
                   aes_string(x=VAR.row.variable,
                              color=VAR.series.variable,
#                               fill=VAR.series.variable,
                              shape=VAR.series.variable,
                              y=VAR.value.variable,
                              size=VAR.size.variable,
                              alpha=VAR.size.variable

                   ),
                   main=VAR.main.label
    )+geom_point()
    figure<-figure+ggtitle(VAR.main.label)+
        xlab(VAR.row.label)+
        ylab(VAR.data.label)+
        scale_color_discrete(name=VAR.color.legend.label)+
        scale_shape_discrete(name=VAR.color.legend.label)+
        scale_size_manual(name=VAR.size.legend.label,
                            values=c("<0.01"=3,
                                     "<0.05"=2,
                                     "Not Signif.\n(>0.05)"=1,
                                     "Sample Too\nSmall to Test"=2
                                )
        )+
        theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"))+
        theme(legend.margin = unit(-0.5, "cm"))+
    theme(legend.key.size = unit(0.25, "cm"))
    figure<-figure+scale_alpha_manual(name=VAR.size.legend.label,
                    values=c("<0.01"=1,
                             "<0.05"=1,
                             "Not Signif.\n(>0.05)"=1,
                             "Sample Too\nSmall to Test"=0.5
                    )
)#     
    
    if(is.na(VAR.facet.secondary)){
        figure<-figure+facet_grid(reformulate(VAR.facet.primary)
                                  #                                               ,ncol=VAR.ncol
                                  #                                                                                         , labeller=Label_Wrap
                                  #                                               #                                           , scales="fixed", space="free_y"
        )
        
        
    }
    else{
        figure<-figure+facet_grid(reformulate(VAR.facet.secondary,VAR.facet.primary)
                                  #                                           , labeller=Label_Wrap
                                  #                                           , scales="free_y" #The scales actually do stay fixed
                                  #                                           , space="free_y"#But only because the space is free
        )   
    }
    
    #     figure<-figure+facet_grid(reformulate(VAR.facet.secondary,VAR.facet.primary)) 
    figure<-figure+coord_flip()+
        theme(legend.position="bottom",
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title= element_text(size=10),
              legend.title= element_text(size=9),
              legend.title= element_text(size=9))
    if(substr(VAR.value.variable,1,1)=="p" | (!is.na(Percentage) & Percentage==TRUE)){
        figure<-figure+scale_y_continuous( labels = percent_format())
    }
    
    figure
    
}

SetExtremesToInf<-function(VAR.long.DF, column, low=NA,high=NA){
    if(!is.na(low)){
        VAR.long.DF[VAR.long.DF[,column]<low & !is.na(VAR.long.DF[,column]),column]<- -Inf
    }
    if(!is.na(high)){
        VAR.long.DF[VAR.long.DF[,column]>high & !is.na(VAR.long.DF[,column]),column]<- Inf
        
    }
    VAR.long.DF
}

ListOutliers<-function(VAR.long.DF, column, low=NA,high=NA){
    outliers<-VAR.long.DF[0,]
    if(!is.na(low)){
        outliers<-rbind(outliers,VAR.long.DF[VAR.long.DF[,column]<low,])
    }
    if(!is.na(high)){
        outliers<-rbind(outliers,VAR.long.DF[VAR.long.DF[,column]>high,])
        
    }
    outliers
}




LatticePercentLinePlot<-function(VAR.color.legend.label
                                 ,VAR.main.label
                                 ,VAR.X.label
                                 ,VAR.Y.label
                                 ,VAR.Coloration
                                 ,VAR.base.row
                                 ,VAR.base.col
                                 ,VAR.long.DF
                                 ,VAR.ncol
                                 ,VAR.x.variable
                                 ,VAR.y.variable
                                 ,VAR.y.series
                                 ,VAR.facet.primary=NA
                                 ,VAR.facet.secondary=NA
                                 ){
    #     debug(PrepareLabelsAndColors
    
    print.figure<-LatticePercentLineWrapper(VAR.color.legend.label
                                            ,VAR.main.label
                                            ,VAR.X.label
                                            ,VAR.Y.label
                                            ,VAR.Coloration
                                            ,VAR.long.DF
                                            ,VAR.ncol
                                            ,VAR.x.variable
                                            ,VAR.y.variable
                                            ,VAR.y.series
                                            ,VAR.facet.primary
                                            ,VAR.facet.secondary
    )
    
    
    #   print.figure<-facetAdjust(print.figure,"down")
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    theme_set(old.theme)
    rm(VAR.color.legend.label
       ,VAR.main.label
       ,VAR.base.row
       ,VAR.base.col
       ,original
       ,print.figure
       ,old.theme
    )
}

TransformFilter<-function(df,MovingAverage,MovingSides=1,Source="y.variable"){
    if(Source!="y.variable"){
        colnames(df)[colnames(df)=="y.variable"]<-"TransformFilter.temp"
        colnames(df)[colnames(df)==Source]<-"y.variable"
    }
    df<-transform(df,MovingAverage = as.numeric(
        filter(y.variable, #y.variable
               rep(1/min(MovingAverage,nrow(df)),
                   min(MovingAverage,nrow(df))), 
               sides=MovingSides))
    )
    if(Source!="y.variable"){
        colnames(df)[colnames(df)=="y.variable"]<-Source
        colnames(df)[colnames(df)=="TransformFilter.temp"]<-"y.variable"
    }
    df
}


LatticePercentLineWrapper<-function(VAR.color.legend.label
                                    ,VAR.main.label
                                    ,VAR.X.label
                                    ,VAR.Y.label
                                    ,VAR.Coloration
                                    ,VAR.long.DF
                                    ,VAR.ncol=NA
                                    ,VAR.x.variable
                                    ,VAR.y.variable
                                    ,VAR.y.series
                                    ,VAR.facet.primary=NA
                                    ,VAR.facet.secondary=NA
                                    ,...
                                    ,HorseTail=FALSE
                                    ,MovingAverage=1
                                    ,MovingSides=1){
    
    if(!VAR.x.variable %in% names(VAR.long.DF)){
        stop(paste(VAR.x.variable,"passed as VAR.x.variable and is not included in VAR.long.DF"))
    }
    
    if(!VAR.y.variable %in% names(VAR.long.DF)){
        stop(paste(VAR.y.variable,"passed as VAR.y.variable and is not included in VAR.long.DF"))
    }
    #     debug(PrepareLabelsAndColors)
    
    if(is.na(VAR.y.series)){ 
        VAR.y.series<-VAR.facet.primary
    } else if(!VAR.y.series %in% names(VAR.long.DF)){
            stop(paste(VAR.y.series,"passed as VAR.y.series and is not included in VAR.long.DF"))
        }
    
    
    if(!("Graph" %in% names(VAR.long.DF))){
        VAR.long.DF$Graph<-NA
    }
    
    #Prepare labels for the category variable
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
    )  
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    old.theme<-theme_set(theme_grey())
    
    
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.y.series column.
    
    if(is.na(VAR.facet.primary) ){
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=cbind(VAR.long.DF[,VAR.x.variable]
                                          ,VAR.long.DF[,VAR.y.series]
                                          ,VAR.long.DF$Graph
                                          ,VAR.long.DF[,c(...)]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","Graph",...,"y.variable")
        
        if(is.numeric(MovingAverage) & MovingAverage>1){
            VAR.long.DF$y.raw<-VAR.long.DF$y.variable
            VAR.long.DF<-ddply(VAR.long.DF, 
                               .(category),
                               .fun=TransformFilter,
                               MovingAverage,
                               MovingSides
            )
            VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
        }
        
        #             VAR.long.DF$category<-factor(laply(strwrap(as.character(VAR.long.DF$category), width=15, simplify=FALSE), 
        #           paste, collapse="\n"))
        #     
        #     labels.category.DF$variable<-factor(laply(strwrap(as.character(labels.category.DF$variable), width=15, simplify=FALSE), 
        #                                        paste, collapse="\n"))                   
        
        VAR.long.DF<-ddply(VAR.long.DF, .(x.variable), transform, y.total=sum(y.variable))
        VAR.long.DF<-ddply(VAR.long.DF, .(x.variable), transform, p=y.variable/y.total)
        
    } else {
        labels.primary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                  ,VAR.long.DF
                                                  ,VAR.facet.primary
                                                  #                                     ,VAR.override.coloration
        )
        
        if(is.na(VAR.facet.secondary)){
            if(!VAR.facet.primary %in% names(VAR.long.DF)){
                stop(paste(VAR.facet.primary," passed as VAR.facet.primary and is not included in VAR.long.DF"))
            }
            
            VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                                   , by=cbind(VAR.long.DF[,VAR.x.variable]
                                              ,VAR.long.DF[,VAR.y.series]
                                              ,VAR.long.DF[,VAR.facet.primary]
                                              ,VAR.long.DF$Graph
                                              ,VAR.long.DF[,c(...)]
                                   )
                                   ,FUN = "sum"
                                   ,na.rm =TRUE
            )
            names(VAR.long.DF)<-c("x.variable","category","second","Graph",...,"y.variable")
            
            if(is.numeric(MovingAverage) & MovingAverage>1){
                VAR.long.DF$y.raw<-VAR.long.DF$y.variable
                VAR.long.DF<-ddply(VAR.long.DF, 
                                   .(category, second),
                                   .fun=TransformFilter,
                                   MovingAverage,
                                   MovingSides
                )
                VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
            }
            
            if(VAR.facet.primary==VAR.y.series){
                VAR.long.DF<-ddply(VAR.long.DF, .(x.variable), transform, y.total=sum(y.variable))
                VAR.long.DF<-ddply(VAR.long.DF, .(x.variable), transform, p=y.variable/y.total)
            }
            else{
                if(HorseTail==TRUE){
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, category), transform, y.total=sum(y.variable))
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, category), transform, p=y.variable/y.total)
                }
                else{
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, second), transform, y.total=sum(y.variable))
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, second), transform, p=y.variable/y.total)
                }
            }
        }
        else {
            if(!VAR.facet.secondary %in% names(VAR.long.DF)){
                stop(paste(VAR.facet.secondary,"passed as VAR.facet.secondary and is not included in VAR.long.DF"))
            }
            
            labels.secondary.DF<-PrepareLabelsAndColors(VAR.Coloration,
                                                        VAR.long.DF,
                                                        VAR.facet.secondary
            )  
            
            VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                                   , by=cbind(VAR.long.DF[,VAR.x.variable]
                                              ,VAR.long.DF[,VAR.y.series]
                                              ,VAR.long.DF[,VAR.facet.primary]
                                              ,VAR.long.DF[,VAR.facet.secondary]
                                              ,VAR.long.DF$Graph
                                              ,VAR.long.DF[,c(...)]
                                   )
                                   ,FUN = "sum"
                                   ,na.rm =TRUE
            )
            names(VAR.long.DF)<-c("x.variable","category","second","third","Graph",...,"y.variable") 
            
            if(is.numeric(MovingAverage) & MovingAverage>1){
                VAR.long.DF$y.raw<-VAR.long.DF$y.variable
                VAR.long.DF<-ddply(VAR.long.DF, 
                                   .(category, second,third),
                                   .fun=TransformFilter,
                                   MovingAverage,
                                   MovingSides
                )
                VAR.long.DF$y.variable<-VAR.long.DF$MovingAverage
                
            }
            
            if(VAR.facet.primary==VAR.y.series){
                
                VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, third), transform, y.total=sum(y.variable))
                VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, third), transform, p=y.variable/y.total)
                
            }
            else{
                if(HorseTail==TRUE){
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, category, third), transform, y.total=sum(y.variable))
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, category, third), transform, p=y.variable/y.total)
                }
                else{
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, second, third), transform, y.total=sum(y.variable))
                    VAR.long.DF<-ddply(VAR.long.DF, .(x.variable, second,  third), transform, p=y.variable/y.total)
                }
            }
            VAR.long.DF$third<-factor(VAR.long.DF$third,
                                      levels=c(labels.secondary.DF$variable),
                                      labels=c(labels.secondary.DF$Label),
                                      ordered=TRUE)
        }
        VAR.long.DF$second<-factor(VAR.long.DF$second,
                                   levels=c(labels.primary.DF$variable),
                                   labels=c(labels.primary.DF$Label),
                                   ordered=TRUE)
        #         rm(labels.secondary.DF)
    }
    
    
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    
    
    
    original<-qplot(
        , data=VAR.long.DF
        , x=x.variable#format(x.variable,"%Y")
        , y=p
        , ylab=VAR.Y.label
        , main=VAR.main.label
        , xlab=VAR.X.label
        , geom="line"
        
        ,group=category
        #          ,size=.25
        # )
        , color=factor(category,levels=labels.category.DF$variable),
    )+ scale_y_continuous(labels = percent_format())  
    # + geom_line(aes(size=1))
    
    #   main=VAR.main.label,
    tick.marks<-2
    print.figure<-original#+scale_x_continuous()
    #     print.figure<-original+scale_x_continuous(
    #     name=VAR.main.label,
    #         breaks=
    #             c(seq(
    #                 as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
    #                 as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
    #                 by=tick.marks)),
    #         labels=
    #             paste("'",format(as.Date(as.character(
    #                 c(seq(
    #                     as.numeric(format(min(VAR.long.DF$x.variable),"%Y")),
    #                     as.numeric(format(max(VAR.long.DF$x.variable),"%Y")),
    #                     by=tick.marks))
    #             ),"%Y"),"%y"),sep="")  
    #     )
    
    
    #, labels=c(labels.category.DF$Label) Section labels don't work with facets.
    #  http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
    
    #   print.figure<-print.figure+scale_shape_discrete(
    #     VAR.color.legend.label
    #     ,  values=color.list
    #     , breaks=c(labels.category.DF$variable) 
    #   )
    
    if(!is.na(VAR.facet.secondary)){
        if(!HorseTail){
            print.figure<-print.figure+facet_grid(second ~ third
                                                  , scales="free_x"
                                                  , space="free_x"
                                                  #                                               , labeller=Label_Wrap
            )+theme(strip.text.y=element_text(size=axis.text.size,angle=0)
            )#+scale_y_continuous(expand=c(0,0.75))
        }
        else{
            print.figure<-print.figure+facet_grid(third ~ second
                                                  #                                                   , scales="free_x"
                                                  #                                                   , space="free_x"
                                                  #                                               , labeller=Label_Wrap
            )+theme(strip.text.y=element_text(size=axis.text.size,angle=0)
            )#+scale_y_continuous(expand=c(0,0.75))
        }
        
    }
    else if (!is.na(VAR.facet.primary)){
        if ( is.na(VAR.ncol)){#VAR.ncol==1 |
            print.figure<-print.figure+facet_grid(. ~ second  ,
                                                  #                                           , labeller=Label_Wrap
                                                  #                                                   ncol=VAR.ncol, 
                                                  scales="free_x", 
                                                  space="free_x"
            )
            
        }
        else{
            print.figure<-print.figure+facet_wrap(~ second
                                                  #                                           , labeller=Label_Wrap
                                                  ,ncol=VAR.ncol
                                                  #                                           , scales="fixed", space="free_y"
            )
            
        }
        
    }
    
    print.figure<-print.figure+scale_color_manual(
        VAR.color.legend.label
        ,  values=color.list
        , breaks=c(labels.category.DF$variable)
        , labels=c(labels.category.DF$Label)
        , guide = guide_legend(reverse=TRUE)
        
    )
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(strip.text.x=element_text(size=strip.text.size))+
        theme(strip.text.y=element_text(size=strip.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))
    
    if(length(unique(VAR.long.DF$category))>1 
       & (is.na(VAR.facet.primary) || (VAR.facet.primary!=VAR.y.series))){
        print.figure<-print.figure+
            theme(legend.position="right")+
            theme(legend.title=element_text(size=legend.text.size,hjust=0))+
            theme(legend.text=element_text(size=legend.text.size))
    }
    else{
        print.figure<-print.figure+theme(legend.position="none")
    }
    
    #   print.figure<-facetAdjust(print.figure,"down")
    print.figure
}



LatticeLinePlot<-function(VAR.color.legend.label
                          ,VAR.main.label
                          ,VAR.X.label
                          ,VAR.Y.label
                          ,VAR.Coloration
                          ,VAR.long.DF
                          ,VAR.ncol
                          ,VAR.x.variable
                          ,VAR.y.variable
                          ,VAR.y.series
                          ,VAR.facet.primary=NA
                          ,VAR.facet.secondary=NA){
    
    print.figure<-LatticePercentLineWrapper(VAR.color.legend.label
                                            ,VAR.main.label
                                            ,VAR.X.label
                                            ,VAR.Y.label
                                            ,VAR.Coloration
                                            ,VAR.long.DF
                                            ,VAR.ncol
                                            ,VAR.x.variable
                                            ,VAR.y.variable
                                            ,VAR.y.series
                                            ,VAR.facet.primary
                                            ,VAR.facet.secondary
                                            ,...)
    
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    theme_set(old.theme)
}



LatticeTrellisPlot<-function(VAR.color.legend.label
                             ,VAR.main.label
                             ,VAR.X.label
                             ,VAR.Y.label
                             ,VAR.Coloration
                             ,VAR.base.row
                             ,VAR.base.col
                             ,VAR.long.DF
                             ,VAR.ncol
                             ,VAR.y.series
){
    #   debug(PrepareLabelsAndColors)
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
    )  
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    
    old.theme<-theme_set(theme_grey())
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.y.series column.
    VAR.long.DF<-aggregate(VAR.long.DF$value
                           , by=list(VAR.long.DF$Fiscal.Year
                                     ,VAR.long.DF[,VAR.y.series])
                           ,FUN = "sum"
                           ,na.rm =TRUE
    )
    names(VAR.long.DF)<-c("Fiscal.Year","category","value")
    
    #Break the data into groups of one calendar year
    
    
    #Turn dates into '00 style years by stripping out everything else.
    #  VAR.long.DF$Fiscal.Year<-format(VAR.long.DF$Fiscal.Year,"%y")
    #VAR.long.DF$Fiscal.Year<-paste(gsub("(-[0-9][0-9]-[0-9][0-9])", "", VAR.long.DF$Fiscal.Year),sep="")
    
    #VAR.long.DF$Fiscal.Year<-cut(VAR.long.DF$Fiscal.Year,"year")  
    
    
    #VAR.long.DF$Fiscal.Year<-VAR.long.DF$Fiscal.Year[order(original.year)]
    
    #format(Fiscal.Year,"%Y"), value,
    
    
    VAR.long.DF$category=factor(VAR.long.DF$category
                                ,levels=c(labels.category.DF$variable),
                                labels=c(labels.category.DF$Label),
                                ordered=TRUE)
    
    #   browser()
    original<-ggplot(VAR.long.DF, aes(x=format(Fiscal.Year,"%Y"),y=value))+
        geom_bar(stat="identity")
    
    
    
    
    #     ylab=VAR.Y.label,
    #     xlab=VAR.X.label,
    #     main=VAR.main.label,
    #     geom="bar",
    #     fill=factor(category,levels=labels.category.DF$variable),
    #   )
    
    tick.marks<-2
    
    print.figure<-original+scale_x_discrete(
        breaks=
            c(seq(
                as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                by=tick.marks)),
        labels=
            paste("'",format(as.Date(as.character(
                c(seq(
                    as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                    as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                    by=tick.marks))
            ),"%Y"),"%y"),sep="")  
    )
    
    #   print.figure<-print.figure+geom_bar(
    #     colour="black",
    #     stat = "identity",
    #     property= "identity"
    #   )
    
    
    
    print.figure<-print.figure+scale_fill_manual(
        VAR.color.legend.label,
        values=color.list, 
        breaks=c(labels.category.DF$variable), 
        labels=c(labels.category.DF$Label)
    )
    
    
    print.figure<-print.figure+facet_wrap(~ category
                                          #                                         ,ncol=VAR.ncol
                                          #                                         , scales="fixed", space="free_y"
                                          #   )+scale_y_continuous(expand=c(0,0.75)
    )
    
    #   
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.position="none")+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))
    #     theme(legend.key.width=unit(0.1,"npc"))
    #   print.figure<-facetAdjust(print.figure,"down")
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    theme_set(old.theme)
    rm(VAR.color.legend.label,VAR.main.label,VAR.base.row,VAR.base.col,original,print.figure,old.theme)
}

#******************LatticeGGPlot************************************
LatticeGGPlot<-function(VAR.color.legend.label
                        ,VAR.main.label
                        ,VAR.X.label
                        ,VAR.Y.label
                        ,VAR.Coloration
                        ,VAR.base.row
                        ,VAR.base.col
                        ,VAR.long.DF
                        ,VAR.ncol
                        ,VAR.y.series
){
    #   debug(PrepareLabelsAndColors)
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
    )  
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    
    old.theme<-theme_set(theme_grey())
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.y.series column.
    VAR.long.DF<-aggregate(VAR.long.DF$value
                           , by=list(VAR.long.DF$Fiscal.Year
                                     ,VAR.long.DF[,VAR.y.series])
                           ,FUN = "sum"
                           ,na.rm =TRUE
    )
    names(VAR.long.DF)<-c("Fiscal.Year","category","value")
    
    #Break the data into groups of one calendar year
    
    
    #Turn dates into '00 style years by stripping out everything else.
    #  VAR.long.DF$Fiscal.Year<-format(VAR.long.DF$Fiscal.Year,"%y")
    #VAR.long.DF$Fiscal.Year<-paste(gsub("(-[0-9][0-9]-[0-9][0-9])", "", VAR.long.DF$Fiscal.Year),sep="")
    
    #VAR.long.DF$Fiscal.Year<-cut(VAR.long.DF$Fiscal.Year,"year")  
    
    
    #VAR.long.DF$Fiscal.Year<-VAR.long.DF$Fiscal.Year[order(original.year)]
    
    #format(Fiscal.Year,"%Y"), value,
    
    
    VAR.long.DF$category=factor(VAR.long.DF$category
                                ,levels=c(labels.category.DF$variable),
                                labels=c(labels.category.DF$Label),
                                ordered=TRUE)
    
    #   browser()
    original<-ggplot(VAR.long.DF, aes(x=format(Fiscal.Year,"%Y"),y=value))+
        geom_bar(stat="identity")
    
    
    
    
    #     ylab=VAR.Y.label,
    #     xlab=VAR.X.label,
    #     main=VAR.main.label,
    #     geom="bar",
    #     fill=factor(category,levels=labels.category.DF$variable),
    #   )
    
    tick.marks<-2
    
    print.figure<-original+scale_x_discrete(
        breaks=
            c(seq(
                as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                by=tick.marks)),
        labels=
            paste("'",format(as.Date(as.character(
                c(seq(
                    as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                    as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                    by=tick.marks))
            ),"%Y"),"%y"),sep="")  
    )
    
    #   print.figure<-print.figure+geom_bar(
    #     colour="black",
    #     stat = "identity",
    #     property= "identity"
    #   )
    
    
    
    print.figure<-print.figure+scale_fill_manual(
        VAR.color.legend.label,
        values=color.list, 
        breaks=c(labels.category.DF$variable), 
        labels=c(labels.category.DF$Label)
    )
    
    
    print.figure<-print.figure+facet_wrap(~ category
                                          #                                         ,ncol=VAR.ncol
                                          #                                         , scales="fixed", space="free_y"
                                          #   )+scale_y_continuous(expand=c(0,0.75)
    )
    
    #   
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.position="none")+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))
    #     theme(legend.key.width=unit(0.1,"npc"))
    print.figure<-facetAdjust(print.figure,"down")
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    theme_set(old.theme)
    rm(VAR.color.legend.label,VAR.main.label,VAR.base.row,VAR.base.col,original,print.figure,old.theme)
}

#**************************FullBarPlot********************************
FullBarPlot<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.x.variable
    ,VAR.y.variable
    ,VAR.y.series
)
{
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)#&!is.na(Fiscal.Year)
    }  
    
    if(!(VAR.y.series %in% names(VAR.long.DF))){
        stop(paste(VAR.y.series,"not found in VAR.long.DF."))
    }  
    
    if(!(VAR.x.variable %in% names(VAR.long.DF))){
        stop(paste(VAR.x.variable,"not found in VAR.long.DF."))
    }  
    
    if(!(VAR.y.variable %in% names(VAR.long.DF))){
        stop(paste(VAR.y.variable,"not found in VAR.long.DF."))
    }  
    
    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in FullBarPlot",
            #       " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    #   labels.category.DF<-subset(VAR.Coloration, Figure==VAR.color.legend.label)
    #   labels.category.DF<-subset(labels.category.DF, variable %in% unique(VAR.long.DF[,VAR.y.series]))
    #   labels.category.DF<-labels.category.DF[order(labels.category.DF$Display.Order),]
    #   
    labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                               ,VAR.long.DF
                                               ,VAR.y.series
    )  
    
    
    
    VAR.long.DF[,VAR.y.series]<-ordered(VAR.long.DF[,VAR.y.series]
                                        ,levels=labels.category.DF$variable
                                        ,labels=labels.category.DF$Label
    )
    VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                           , by=list(VAR.long.DF[,VAR.x.variable]
                                     ,VAR.long.DF[,VAR.y.series]
                           )
                           ,FUN = "sum"
                           ,na.rm =TRUE
    )
    names(VAR.long.DF)<-c("x.variable","category","y.variable")
    
    
    
    
    old.theme<-theme_set(theme_grey())
    
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    #   #Break the data into groups of one calendar year
    #   VAR.long.DF[,VAR.x.variable]<-cut(VAR.long.DF[,VAR.x.variable],"year")  
    #   
    #   #Turn dates into '00 style years by stripping out everything else.
    #    VAR.long.DF[,VAR.x.variable]<-paste(gsub("(-[0-9][0-9]-[0-9][0-9])", "", VAR.long.DF[,VAR.x.variable]),sep="")
    #   
    #
    
    VAR.long.DF<-VAR.long.DF[order(VAR.long.DF$x.variable,VAR.long.DF$category),]
    VAR.long.DF<-ddply(VAR.long.DF,.(x.variable),mutate,ytextposition=cumsum(y.variable)-0.5*y.variable)#.(Fiscal.Year)
    
    original<-qplot(
        format(x.variable,"%Y"),
        geom="bar",
        weight=y.variable,
        #     y=y.variable,
        data=VAR.long.DF,
        fill=factor(VAR.long.DF$category
                    ,levels=labels.category.DF$variable
                    ,labels=labels.category.DF$Label
                    ,ordered=TRUE
                    #                 ,guide = guide_legend(reverse=TRUE)
        ),
        ylab=VAR.Y.label,
        xlab=VAR.X.label,
        main=VAR.main.label,
        gbinwidth=1
        ,environment = environment()
        
        #     stat=identity,
        #     geom="bar"
    )#+ scale_fill_hue()
    print.figure<-original+scale_fill_manual(
        VAR.color.legend.label,
        values=color.list, 
        breaks=c(labels.category.DF$variable), 
        labels=c(labels.category.DF$Label),
        guide = guide_legend(reverse=TRUE)
    )
    
    print.figure<-print.figure+scale_x_discrete(
        breaks=    c(as.numeric(format(min(VAR.long.DF$x.variable),"%Y")):as.numeric(format(max(VAR.long.DF$x.variable)
                                                                                            ,"%Y"))),
        labels=    paste("'",format(as.Date(as.character(c(as.numeric(format(min(VAR.long.DF$x.variable),"%Y"))):as.numeric(format(max(VAR.long.DF$x.variable),"%Y"))),"%Y")
                                    ,"%y"),sep="")
    )
    
    
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))#+
    #     theme(plot.background=element_rect(fill="grey80",colour=NA))+
    #     theme(legend.background=element_rect(fill="grey80",colour=NA))
    #   theme(plot.background=element_rect(fill="grey80",colour=NA))+
    #     theme(legend.background=element_rect(fill="grey80",colour=NA))
    #     theme(legend.key.width=unit(0.1,"npc"))
    print.figure<-print.figure+geom_bar(colour="black")#
    print.figure<-print.figure+
        geom_text(aes(label=VariableNumericalFormat(y.variable)
                      #                     format(round(y.variable,3),  scientific=FALSE, trim=TRUE, big.mark=",")
                      #                   format(y.variable, digits=1, drop0trailing=TRUE, trim=TRUE, big.mark=",")
                      #apply(y.variable,VariableNumericalFormat)
                      ,y=ytextposition)
                  ,size=geom.text.size
                  ,hjust=0.5
                  ,vjust=0.5
                  #,color=color.list This doesn't work yet
        )
    #   stop("test")
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    #   
    #   print(
    #     grid.text(paste("Source: FPDS and")
    #               ,y=unit(4,"line")
    #               ,x=unit(-10,"line")#35 -10
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain"
    #                        ,fontsize= strip.text.size)
    #     )
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    #   
    #   print(
    #     grid.text(paste("CSIS analysis")
    #               ,y=unit(3,"line")
    #               ,x=unit(-7,"line")#38
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain"
    #                        ,fontsize= strip.text.size)
    #     )
    #     , vp=vplayout(VAR.row
    #                   ,VAR.col
    #     )
    #   )  
    #   
    #   print(
    #     grid.text(paste("Available online at")
    #               ,y=unit(2,"line")
    #               ,x=unit(-10,"line")#35
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain"
    #                        ,fontsize= strip.text.size)
    #     )
    #     , vp=vplayout(VAR.row
    #                   ,VAR.col
    #     )
    #   )  
    #   
    #   
    #   print(
    #     grid.text(paste("www.csis.org/NSPIR/DoD")
    #               ,y=unit(1,"line")
    #               ,x=unit(-10,"line")#35
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain"
    #                        ,fontsize= strip.text.size)
    #     )
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    
    theme_set(old.theme)
    rm(VAR.color.legend.label
       ,VAR.main.label
       ,VAR.base.row
       ,VAR.base.col
       ,print.figure
    )
}


#**************************HistogramOrDensity********************************

HistogramOrDensityWrapper<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.long.DF
    ,VAR.histogram.or.density
    ,VAR.x.variable
    ,VAR.y.series =NA
    ,VAR.facet.primary=NA
    ,VAR.facet.secondary=NA
)
{
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    if("Fiscal.Year" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, is.na(Fiscal.Year))
    }  
    
    if(!(VAR.x.variable %in% names(VAR.long.DF)) ){
        stop(paste(VAR.x.variable,"not found in VAR.long.DF."))
    }
    
    
    #   
    #   VAR.long.DF<-aggregate(VAR.long.DFVAR.y.variable
    #                          , by=list(VAR.long.DF[,VAR.x.variable]
    #                                    ,VAR.long.DF[,VAR.x.variable])
    #                          ,FUN = "sum"
    #                          ,na.rm =TRUE
    #   )
    
    #   names(VAR.long.DF)<-c("Fiscal.Year",VAR.x.variable,VAR.y.series)

    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in FullBarPlot",
            #       " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    # 
    #   labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
    #                                     ,VAR.long.DF
    #                                     ,VAR.x.variable
    #   )  
    old.theme<-theme_set(theme_grey())
    
    #   color.list<-c(as.character(labels.category.DF$ColorRGB))
    #   names(color.list)<-c(labels.category.DF$variable)
    
    
    validcount<-nrow(subset(VAR.long.DF,!is.na(VAR.long.DF[,VAR.x.variable])))
    
    original<-ggplot(
        data=VAR.long.DF
        ,aes_string(x=VAR.x.variable
        )
    )+
        ylab(VAR.Y.label)+
        xlab(VAR.X.label)+
        ggtitle(VAR.main.label)
if(!is.na(VAR.y.series)){
    original+aes_string(color=VAR.y.series,
                        ,fill=VAR.y.series)
}
    
    #     gbinwidth=1,
    #     stat=identity,
    #     geom="bar"
    #     y=y.variable,
    #     data=VAR.long.DF,
    #     fill=factor(VAR.long.DF[,VAR.x.variable]
    #                 ,levels=labels.category.DF$variable
    #                 ,guide = guide_legend(reverse=TRUE)
    #     )
    #if percentage
    if(VAR.histogram.or.density=='histogram'){
        
        
        # if(substr(VAR.x.variable,start=1,stop=1)=='p'){
        # original<-original+ geom_histogram(
        #     binwidth=1/30
        #     , colour="black"
        #     , fill="white"
        #     )
        # }
        # else 
        
        
        if(substr(VAR.x.variable,1,1)=="p"){
            original<-original+scale_x_continuous( labels = percent_format())
        }
        if (!is.factor(VAR.long.DF[,VAR.x.variable]) & length(VAR.long.DF[,VAR.x.variable])>100){
            range.binwidth<-quantile(VAR.long.DF[,VAR.x.variable], c(0.05, 0.95),na.rm=TRUE)[2]
            range.binwidth<-range.binwidth-quantile(VAR.long.DF[,VAR.x.variable], c(0.05, 0.95),na.rm=TRUE)[1]
            if (range.binwidth<2.5 & !is.integer(VAR.long.DF[,VAR.x.variable])){
                range.binwidth<-max(round(range.binwidth/30,2),0.01)
            }
            else if (range.binwidth<25 & !is.integer(VAR.long.DF[,VAR.x.variable])){
                range.binwidth<-max(round(range.binwidth/30,1),0.1)
            }
            else if (range.binwidth<250){
                range.binwidth<-max(round(range.binwidth/30,0),1)
            }
            original<-original+ geom_histogram(
                binwidth=range.binwidth
                , colour="black"
                , fill="white"
            )
            #       original<-original+stat_bin(geom="text", binwidth=range.binwidth,aes(label=..count..), vjust=-0.25) 
        }
        else {
            original<-original+ geom_histogram(
                , colour="black"
                , fill="white"
            )
            original<-original+stat_bin(geom="text", 
                                        aes(label=..count..),
                                        vjust=-0.25,
                                        size=geom.text.size) 
            
        }
        #     
        
        
        #Range solution:
        #Source: http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
        # compute lower and upper whiskers
        # xlim1 = boxplot.stats(VAR.long.DF[,VAR.x.variable])$stats[c(1, 5)]
        if(!is.factor(VAR.long.DF[,VAR.x.variable])&length(VAR.long.DF[,VAR.x.variable])>100){
            #   stop("test")
            xlim1 = quantile(VAR.long.DF[,VAR.x.variable], c(0.05, 0.95),na.rm=TRUE)
            max.x<-max(VAR.long.DF[,VAR.x.variable],na.rm=TRUE)
            min.x<-min(VAR.long.DF[,VAR.x.variable],na.rm=TRUE)
            if((xlim1[2]-xlim1[1])*1.5<(max.x-min.x)){
                if(xlim1[1]<0)
                    xlim1[1]<-xlim1[1]*1.25
                else xlim1[1]<-xlim1[1]*.75
                if(xlim1[2]<0)
                    xlim1[2]<-xlim1[2]*.75
                else xlim1[2]<-xlim1[2]*1.25
                xlim1[1]<-max(xlim1[1],min.x)
                xlim1[2]<-min(xlim1[2],max.x)
                # scale x limits based on xlim1
                original = original + coord_cartesian(xlim = xlim1)
            }
        }
        
        
    }
    else if (VAR.histogram.or.density=='density'){
        original<-original+ geom_density(alpha=.2, fill="#FF6666")
        if(length(VAR.long.DF[,VAR.x.variable])>100){
            xlim1 = quantile(VAR.long.DF[,VAR.x.variable], c(0.05, 0.95),na.rm=TRUE)
            max.x<-max(VAR.long.DF[,VAR.x.variable],na.rm=TRUE)
            min.x<-min(VAR.long.DF[,VAR.x.variable],na.rm=TRUE)
            if((xlim1[2]-xlim1[1])*1.5<(max.x-min.x)){
                if(xlim1[1]<0)
                    xlim1[1]<-xlim1[1]*1.25
                else xlim1[1]<-xlim1[1]*.75
                if(xlim1[2]<0)
                    xlim1[2]<-xlim1[2]*.75
                else xlim1[2]<-xlim1[2]*1.25
                xlim1[1]<-max(xlim1[1],min.x)
                xlim1[2]<-min(xlim1[2],max.x)
                # scale x limits based on xlim1
                original = original + coord_cartesian(xlim = xlim1)
            }
        }
    }
    #Source: http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
    # compute lower and upper whiskers
    # ylim1 = boxplot.stats(VAR.long.DF[,VAR.y.series])$stats[c(1, 5)]
    
    original<-original+scale_y_continuous(labels=comma)
    if(is.na(VAR.facet.secondary)){
        if(!is.na(VAR.facet.primary)&validcount>0){
            original<-original+facet_wrap(
                #This is an indirect reference, it allows use of a column name variable rather than naming the column directly.
                #This isn't used in some other graph functions because in those functions the data frames are aggergated.
                #During that aggregation process, 
                as.formula(sprintf('~ %s',VAR.facet.primary))
                
                #                                           ,ncol=VAR.ncol
                #                                           , labeller=Label_Wrap
                #                                           , scales="fixed", space="free_y"
                
            )
            original<-original+scale_y_continuous( expand = c( 0.25 , 0.05 ))
            # +scale_y_continuous(expand=c(0,0.75)#)+scale_y_continuous(expand=c(0,0.75)
            
            
        }
           }
    else{
        original<-original+facet_grid(reformulate(VAR.facet.primary ,VAR.facet.secondary)
                                              , labeller=Label_Wrap
                                              , scales="free_y" #The scales actually do stay fixed
#                                               , space="free_y"#But only because the space is free
        )
#         +scale_y_continuous(expand=c(0,0.75)
#                              ,labels=comma
#         )+theme(strip.text.y=element_text(size=axis.text.size,family="times",face="bold",angle=0)
#         )
        
    }
    
    
    
    # if(VAR.x.variable=="avSize"){
    #   stop("defg")
    # }
    #   original<-original+scale_fill_manual(
    #     VAR.color.legend.label,
    #     values=color.list, 
    #     breaks=c(labels.category.DF$variable), 
    #     labels=c(labels.category.DF$Label),
    #     guide = guide_legend(reverse=TRUE)
    #   )
    
    #   print.figure<-print.figure+scale_x_discrete(
    #     breaks=    c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y")):as.numeric(format(max(VAR.long.DF[,VAR.x.variable])
    #                                                                                          ,"%Y"))),
    #     labels=    paste("'",format(as.Date(as.character(c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y"))):as.numeric(format(max(VAR.long.DF[,VAR.x.variable]),"%Y"))),"%Y")
    #                                 ,"%y"),sep="")
    #   )
    
    
    
      original<-original+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))#+
    
    # print.figure<-print.figure+geom_bar(colour="black")#
    #   print.figure<-print.figure+
    #     geom_text(aes(label=paste(round(value,0))
    #                   ,y=ytextposition)
    #               ,size=geom.text.size
    #               ,hjust=0.5
    #               ,vjust=0.5
    #               #,color=color.list This doesn't work yet
    #     )
    
    #     print(original, vp=subplot(VAR.base.row,VAR.base.col))
    
    #     print(
    #         grid.text(paste("Source: FPDS and")
    #                   ,y=unit(4,"line")
    #                   ,x=unit(-10,"line")#35
    #                   ,vjust="bottom",
    #                   ,hjust=0,
    #                   ,gp=gpar(fontface="plain"
    #                            ,fontsize= strip.text.size)
    #         )
    #         , vp=vplayout(VAR.row,VAR.col)
    #     )  
    #     
    #     print(
    #         grid.text(paste("CSIS analysis")
    #                   ,y=unit(3,"line")
    #                   ,x=unit(-7,"line")#38
    #                   ,vjust="bottom",
    #                   ,hjust=0,
    #                   ,gp=gpar(fontface="plain"
    #                            ,fontsize= strip.text.size)
    #         )
    #         , vp=vplayout(VAR.row
    #                       ,VAR.col
    #         )
    #     )  
    #     
    #     print(
    #         grid.text(paste("Available online at")
    #                   ,y=unit(2,"line")
    #                   ,x=unit(-10,"line")#35
    #                   ,vjust="bottom",
    #                   ,hjust=0,
    #                   ,gp=gpar(fontface="plain"
    #                            ,fontsize= strip.text.size)
    #         )
    #         , vp=vplayout(VAR.row
    #                       ,VAR.col
    #         )
    #     )  
    #     
    #     
    #     print(
    #         grid.text(paste("www.csis.org/NSPIR/DoD")
    #                   ,y=unit(1,"line")
    #                   ,x=unit(-10,"line")#35
    #                   ,vjust="bottom",
    #                   ,hjust=0,
    #                   ,gp=gpar(fontface="plain"
    #                            ,fontsize= strip.text.size)
    #         )
    #         , vp=vplayout(VAR.row,VAR.col)
    #     )  
    
    theme_set(old.theme)
    original
}

HistogramOrDensity<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.histogram.or.density
    ,VAR.x.variable
    ,VAR.y.series =NA
    ,VAR.facet.primary=NA
    ,VAR.facet.secondary=NA
)
{
    original<-HistogramOrDensityWrapper(
        VAR.color.legend.label
        ,VAR.main.label
        ,VAR.X.label
        ,VAR.Y.label
        ,VAR.Coloration
        ,VAR.long.DF
        ,VAR.histogram.or.density
        ,VAR.x.variable
        ,VAR.y.series
        ,VAR.facet.primary
        ,VAR.facet.secondary
    )
    print(original, vp=subplot(VAR.base.row,VAR.base.col))
    
    print(
        grid.text(paste("Source: FPDS and")
                  ,y=unit(4,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    
    print(
        grid.text(paste("CSIS analysis")
                  ,y=unit(3,"line")
                  ,x=unit(-7,"line")#38
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    print(
        grid.text(paste("Available online at")
                  ,y=unit(2,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    
    print(
        grid.text(paste("www.csis.org/NSPIR/DoD")
                  ,y=unit(1,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    

    rm(VAR.color.legend.label
       ,VAR.main.label
       ,VAR.base.row
       ,VAR.base.col
       #      ,print.figure
    )
}


#**************************Boxplot********************************
BoxplotWrapper<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.long.DF
    ,VAR.x.variable
    ,VAR.y.series
    ,VAR.facet.primary=NA
)
{
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    if("Fiscal.Year" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, is.na(Fiscal.Year))
    }  
    
    if(!(VAR.x.variable %in% names(VAR.long.DF)) ){
        stop(paste(VAR.x.variable,"not found in VAR.long.DF."))
    }  
    
    if(!(VAR.y.series %in% names(VAR.long.DF)) ){
        stop(paste(VAR.y.series,"not found in VAR.long.DF."))
    }  
    #   
    #   VAR.long.DF<-aggregate(VAR.long.DFVAR.y.variable
    #                          , by=list(VAR.long.DF[,VAR.x.variable]
    #                                    ,VAR.long.DF[,VAR.x.variable])
    #                          ,FUN = "sum"
    #                          ,na.rm =TRUE
    #   )
    
    #   names(VAR.long.DF)<-c("Fiscal.Year",VAR.x.variable,VAR.y.series)
    
    
    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in Boxplot",
            #       " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    # 
    #   labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
    #                                     ,VAR.long.DF
    #                                     ,VAR.x.variable
    #   )  
    old.theme<-theme_set(theme_grey())
    
    #   color.list<-c(as.character(labels.category.DF$ColorRGB))
    #   names(color.list)<-c(labels.category.DF$variable)
    
    
    #run the original least squares analysis
    VAR.long.DF<-subset(VAR.long.DF,!is.nan(VAR.long.DF[,VAR.x.variable])&!is.nan(VAR.long.DF[,VAR.y.series]))
    if(!is.factor(VAR.long.DF[,VAR.x.variable])){
        VAR.long.DF[,VAR.x.variable]<-factor(VAR.long.DF[,VAR.x.variable])
    }
    
    original<-ggplot(
        data=VAR.long.DF
        ,aes_string(x=VAR.x.variable,y=VAR.y.series)
        #     y=value,
        #     data=VAR.long.DF,
        #     fill=factor(VAR.long.DF[,VAR.x.variable]
        #                 ,levels=labels.category.DF$variable
        #                 ,guide = guide_legend(reverse=TRUE)
        #     ),
        #     ,ylab=VAR.Y.label
        #     ,xlab=VAR.X.label
        #     ,main=VAR.main.label
        #     gbinwidth=1,
        #     stat=identity,
        #     geom="bar"
    )+ geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+
        ylab(VAR.Y.label)+
        xlab(VAR.X.label)+
        ggtitle(VAR.main.label)
    medians<-subset(VAR.long.DF,select=c(VAR.x.variable,VAR.y.series))
    colnames(medians)[colnames(medians)==VAR.x.variable]<-"x.series"
    colnames(medians)[colnames(medians)==VAR.y.series]<-"y.series"
    
    medians <- ddply(medians,.(x.series), summarise, med = median(y.series))
    
    if(substr(VAR.y.series,1,1)=="p"){
        original<-original+scale_y_continuous( labels = percent_format())
    }
    #     original<-original+geom_text( data=medians, aes_string(x=VAR.x.variable),aes(y=med,label=med), size = 3, vjust = -1.5)
    #                                                                     as.formula(sprintf("label = %s",)), 
    
    #     geom_histogram(
    #     binwidth=.1
    #     , colour="black"
    #     , fill="white"
    #   )
    
    if(!is.na(VAR.facet.primary)){
        original<-original+facet_wrap(
            #This is an indirect reference, it allows use of a column name variable rather than naming the column directly.
            #This isn't used in some other graph functions because in those functions the data frames are aggergated.
            #During that aggregation process, 
            as.formula(sprintf('~ %s',VAR.facet.primary))
            #                                           ,ncol=VAR.ncol
            #                                           , labeller=Label_Wrap
            #                                           , scales="fixed", space="free_y"
        )
        # +scale_y_continuous(expand=c(0,0.75)#)+scale_y_continuous(expand=c(0,0.75)
        
        
    }
    #Range solution:
    #Source: http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
    # compute lower and upper whiskers
    # ylim1 = boxplot.stats(VAR.long.DF[,VAR.y.series])$stats[c(1, 5)]
    if (length(VAR.long.DF[,VAR.y.series])>100){
        ylim1 = quantile(VAR.long.DF[,VAR.y.series], c(0.05, 0.95),na.rm=TRUE)
        max.y<-max(VAR.long.DF[,VAR.y.series],na.rm=TRUE)
        min.y<-min(VAR.long.DF[,VAR.y.series],na.rm=TRUE)
        if((ylim1[2]-ylim1[1])*1.5<(max.y-min.y)){
            if(ylim1[1]<0)
                ylim1[1]<-ylim1[1]*1.25
            else ylim1[1]<-ylim1[1]*.75
            if(ylim1[2]<0)
                ylim1[2]<-ylim1[2]*.75
            else ylim1[2]<-ylim1[2]*1.25
            ylim1[1]<-max(ylim1[1],min.y)
            ylim1[2]<-min(ylim1[2],max.y)
            # scale y limits based on ylim1
            original = original + coord_cartesian(ylim = ylim1)
        }
    }
    
    # if(VAR.x.variable=="avSize"){
    #   stop("defg")
    # }
    #   print.figure<-original+scale_fill_manual(
    #     VAR.color.legend.label,
    #     values=color.list, 
    #     breaks=c(labels.category.DF$variable), 
    #     labels=c(labels.category.DF$Label),
    #     guide = guide_legend(reverse=TRUE)
    #   )
    
    #   print.figure<-print.figure+scale_x_discrete(
    #     breaks=    c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y")):as.numeric(format(max(VAR.long.DF[,VAR.x.variable])
    #                                                                                          ,"%Y"))),
    #     labels=    paste("'",format(as.Date(as.character(c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y"))):as.numeric(format(max(VAR.long.DF[,VAR.x.variable]),"%Y"))),"%Y")
    #                                 ,"%y"),sep="")
    #   )
    
    
    
    #   print.figure<-print.figure+
    #     theme(axis.text.x=element_text(size=axis.text.size))+
    #     theme(axis.text.y=element_text(size=axis.text.size))+
    #     theme(axis.title.x=element_text(size=axis.text.size))+
    #     theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
    #     theme(plot.title=element_text(size=title.text.size))+
    #     theme(legend.title=element_text(size=legend.text.size,hjust=0))+
    #     theme(legend.text=element_text(size=legend.text.size))#+
    
    # print.figure<-print.figure+geom_bar(colour="black")#
    #   print.figure<-print.figure+
    #     geom_text(aes(label=paste(round(value,0))
    #                   ,y=ytextposition)
    #               ,size=geom.text.size
    #               ,hjust=0.5
    #               ,vjust=0.5
    #               #,color=color.list This doesn't work yet
    #     )
    
    original
    
    
}


Boxplot<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.x.variable
    ,VAR.y.series
    ,VAR.facet.primary=NA
)
{
    original<-BoxplotWrapper(
        VAR.color.legend.label
        ,VAR.main.label
        ,VAR.X.label
        ,VAR.Y.label
        ,VAR.Coloration
        ,VAR.long.DF
        ,VAR.x.variable
        ,VAR.y.series
        ,VAR.facet.primary
    )
    print(original, vp=subplot(VAR.base.row,VAR.base.col))
    
    print(
        grid.text(paste("Source: FPDS and")
                  ,y=unit(4,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    
    print(
        grid.text(paste("CSIS analysis")
                  ,y=unit(3,"line")
                  ,x=unit(-7,"line")#38
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    print(
        grid.text(paste("Available online at")
                  ,y=unit(2,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    
    print(
        grid.text(paste("www.csis.org/NSPIR/DoD")
                  ,y=unit(1,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    
    theme_set(old.theme)
    
}


#**************************ScatterPlot********************************
ScatterPlot<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.x.variable
    ,VAR.y.variable
    ,VAR.facet.primary=NA
    ,VAR.y.series=NA
)
{
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    if("Fiscal.Year" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, is.na(Fiscal.Year))
    }  
    
    if(!(VAR.x.variable %in% names(VAR.long.DF)) ){
        stop(paste(VAR.x.variable,"not found in VAR.long.DF."))
    }  
    
    if(!(VAR.y.variable %in% names(VAR.long.DF)) ){
        stop(paste(VAR.y.variable,"not found in VAR.long.DF."))
    }  
    #   
    #   VAR.long.DF<-aggregate(VAR.long.DFVAR.y.variable
    #                          , by=list(VAR.long.DF[,VAR.x.variable]
    #                                    ,VAR.long.DF[,VAR.x.variable])
    #                          ,FUN = "sum"
    #                          ,na.rm =TRUE
    #   )
    
    #   names(VAR.long.DF)<-c("Fiscal.Year",VAR.x.variable,VAR.y.variable)
    
    
    
    #Error checking for function calls with no data (possibly a result of the subset)
    if(nrow(VAR.long.DF)==0){
        stop (paste(
            "Empty VAR.long.DF in FullBarPlot",
            #       " legend.title=",legend.title,
            "; VAR.sectionSTR=",VAR.sectionSTR,
            ".",
            sep=""
        ))
    } 
    
    # 
    #   labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
    #                                     ,VAR.long.DF
    #                                     ,VAR.x.variable
    #   )  
    old.theme<-theme_set(theme_grey())
    
    #   color.list<-c(as.character(labels.category.DF$ColorRGB))
    #   names(color.list)<-c(labels.category.DF$variable)
    
    
    #run the original least squares analysis
    #   noNAs.DF<-subset(VAR.long.DF,!is.nan(VAR.long.DF[,VAR.x.variable])&!is.nan(VAR.long.DF[,VAR.y.variable]))
    validcount<-nrow(subset(VAR.long.DF,!is.na(VAR.long.DF[,VAR.x.variable])&!is.na(VAR.long.DF[,VAR.y.variable])))
    if(validcount>0 & !is.factor(VAR.long.DF[,VAR.x.variable]) & !is.factor(VAR.long.DF[,VAR.y.variable])){
        ols<-lm(VAR.long.DF[,VAR.x.variable]~VAR.long.DF[,VAR.y.variable])
        summary(ols)
    }
    original<-ggplot(
        data=VAR.long.DF
        ,aes_string(x=VAR.x.variable,
                    y=VAR.y.variable)
        #     y=value,
        #     data=VAR.long.DF,
        #     fill=factor(VAR.long.DF[,VAR.x.variable]
        #                 ,levels=labels.category.DF$variable
        #                 ,guide = guide_legend(reverse=TRUE)
        #     ),
        #     gbinwidth=1,
        #     stat=identity,
        #     geom="bar"
    )
    if(is.na(VAR.y.series)){
        original<-original+geom_point()
    }
    else{
        original<-original+geom_point(aes_string(color=VAR.y.series,shape=VAR.y.series))
    }
    
    
    original<-original+
        geom_smooth(method = 'loess')+
        ylab(VAR.Y.label)+
        xlab(VAR.X.label)+
        ggtitle(VAR.main.label)#+ scale_fill_hue()
    #   if(validcount>0 & !is.factor(VAR.long.DF[,VAR.x.variable]) & !is.factor(VAR.long.DF[,VAR.y.variable])){
    #     original<-original+geom_abline(intercept=mean(VAR.long.DF[,VAR.y.variable])#ols$coefficients[1]#
    #                                    ,slope=ols$coefficients[2])
    #     
    #   }
    
    if(substr(VAR.x.variable,1,1)=="p"){
        original<-original+scale_x_continuous( labels = percent_format())
    }
    if(substr(VAR.y.variable,1,1)=="p"){
        original<-original+scale_y_continuous( labels = percent_format())
    }
    
    if(!is.na(VAR.facet.primary)&validcount>0){
        original<-original+facet_wrap(
            #This is an indirect reference, it allows use of a column name variable rather than naming the column directly.
            #This isn't used in some other graph functions because in those functions the data frames are aggergated.
            #During that aggregation process, 
            as.formula(sprintf('~ %s',VAR.facet.primary))
            #                                           ,ncol=VAR.ncol
            #                                           , labeller=Label_Wrap
            #                                           , scales="fixed", space="free_y"
        )
        # +scale_y_continuous(expand=c(0,0.75)#)+scale_y_continuous(expand=c(0,0.75)
        
        
    }
    
    # if(VAR.x.variable=="avSize"){
    #   stop("defg")
    # }
    #   print.figure<-original+scale_fill_manual(
    #     VAR.color.legend.label,
    #     values=color.list, 
    #     breaks=c(labels.category.DF$variable), 
    #     labels=c(labels.category.DF$Label),
    #     guide = guide_legend(reverse=TRUE)
    #   )
    
    #   print.figure<-print.figure+scale_x_discrete(
    #     breaks=    c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y")):as.numeric(format(max(VAR.long.DF[,VAR.x.variable])
    #                                                                                          ,"%Y"))),
    #     labels=    paste("'",format(as.Date(as.character(c(as.numeric(format(min(VAR.long.DF[,VAR.x.variable]),"%Y"))):as.numeric(format(max(VAR.long.DF[,VAR.x.variable]),"%Y"))),"%Y")
    #                                 ,"%y"),sep="")
    #   )
    
    
    
    original<-original+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))#+
    
    # print.figure<-print.figure+geom_bar(colour="black")#
    #   print.figure<-print.figure+
    #     geom_text(aes(label=paste(round(value,0))
    #                   ,y=ytextposition)
    #               ,size=geom.text.size
    #               ,hjust=0.5
    #               ,vjust=0.5
    #               #,color=color.list This doesn't work yet
    #     )
    
    print(original, vp=subplot(VAR.base.row,VAR.base.col))
    
    print(
        grid.text(paste("Source: FPDS and")
                  ,y=unit(4,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    
    print(
        grid.text(paste("CSIS analysis")
                  ,y=unit(3,"line")
                  ,x=unit(-7,"line")#38
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    print(
        grid.text(paste("Available online at")
                  ,y=unit(2,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row
                      ,VAR.col
        )
    )  
    
    
    print(
        grid.text(paste("www.csis.org/NSPIR/DoD")
                  ,y=unit(1,"line")
                  ,x=unit(-10,"line")#35
                  ,vjust="bottom",
                  ,hjust=0,
                  ,gp=gpar(fontface="plain"
                           ,fontsize= strip.text.size)
        )
        , vp=vplayout(VAR.row,VAR.col)
    )  
    
    theme_set(old.theme)
    rm(VAR.color.legend.label
       ,VAR.main.label
       ,VAR.base.row
       ,VAR.base.col
       #      ,print.figure
    )
}


TablePlot<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.x.variable
    ,VAR.y.variable
    ,VAR.y.series
    ,VAR.facet.primary=NA
    ,VAR.facet.secondary=NA
)
{
    #   if("Graph" %in% names(VAR.long.DF)){
    #     VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE&!is.na(Fiscal.Year))
    #   }  
    
    #   labels.category.DF<-subset(VAR.Coloration, Figure==VAR.color.legend.label)
    #   labels.category.DF<-subset(labels.category.DF, variable %in% unique(VAR.long.DF[,VAR.y.series]))
    #   labels.category.DF<-labels.category.DF[order(labels.category.DF$Display.Order),]
    #   
    
    #   VAR.long.DF<-aggregate(VAR.long.DFVAR.y.variable
    #                          , by=list(VAR.long.DF[,VAR.x.variable]
    #                                    ,VAR.long.DF[,VAR.y.series]
    #                          )
    #                          ,FUN = "sum"
    #                          ,na.rm =TRUE
    #   )
    #   names(VAR.long.DF)<-c("Fiscal.Year",VAR.y.series,"value")
    
    
    
    
    #Reduce the number of rows by aggregating to one row per unique entry in the VAR.y.series column.
    
    if(is.na(VAR.facet.primary)){
        labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                   ,VAR.long.DF
                                                   ,VAR.y.series
        )  
        
        
        VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                               , by=list(VAR.long.DF[,VAR.x.variable]
                                         ,VAR.long.DF[,VAR.y.series]
                               )
                               ,FUN = "sum"
                               ,na.rm =TRUE
        )
        names(VAR.long.DF)<-c("x.variable","category","y.variable")
        
    } else{
        
        if(is.na(VAR.facet.secondary)){ 
            #Reduce the number of rows by aggregating to one row per unique entry in the VAR.facet.primary column.
            
            VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                                   , by=list(VAR.long.DF[,VAR.x.variable]
                                             ,VAR.long.DF[,VAR.y.series]
                                             ,VAR.long.DF[,VAR.facet.primary]
                                   )
                                   ,FUN = "sum"
                                   ,na.rm =TRUE
            )
            names(VAR.long.DF)<-c("x.variable","category","primary","y.variable")
            
            
        }
        else{
            labels.secondary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                        ,VAR.long.DF
                                                        ,VAR.facet.secondary)  
            
            VAR.long.DF<-aggregate(VAR.long.DF[,VAR.y.variable]
                                   , by=list(VAR.long.DF[,VAR.x.variable]
                                             ,VAR.long.DF[,VAR.y.series]
                                             ,VAR.long.DF[,VAR.facet.primary]
                                             ,VAR.long.DF[,VAR.facet.secondary]
                                   )
                                   ,FUN = "sum"
                                   ,na.rm =TRUE
            )
            names(VAR.long.DF)<-c("x.variable","category","primary","secondary","y.variable")
            
            VAR.long.DF$secondary<-factor(VAR.long.DF$secondary,
                                          levels=c(labels.secondary.DF$variable),
                                          labels=c(labels.secondary.DF$variable),
                                          ordered=TRUE)
            rm(labels.secondary.DF)
            
        }
        VAR.long.DF$primary=factor(VAR.long.DF$primary,
                                   levels=c(labels.category.DF$variable),
                                   labels=c(labels.category.DF$variable),
                                   ordered=TRUE)
        
        
        labels.category.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                   ,VAR.long.DF
                                                   ,VAR.facet.primary
        )  
        
        
        labels.secondary.DF<-PrepareLabelsAndColors(VAR.Coloration
                                                    ,VAR.long.DF
                                                    ,VAR.y.series
        )  
    }
    
    
    
    old.theme<-theme_set(theme_grey())
    
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    #   #Break the data into groups of one calendar year
    #   VAR.long.DF$Fiscal.Year<-cut(VAR.long.DF$Fiscal.Year,"year")  
    #   
    #   #Turn dates into '00 style years by stripping out everything else.
    #    VAR.long.DF$Fiscal.Year<-paste(gsub("(-[0-9][0-9]-[0-9][0-9])", "", VAR.long.DF$Fiscal.Year),sep="")
    #   
    #
    
    #   original<-qplot(
    #     x=format(Fiscal.Year,"%Y"),
    #     #geom="bar",
    #     #     weight=value,
    #     y=factor(VAR.long.DF[,VAR.y.series]
    #              ,levels=labels.category.DF$variable
    #              #                 #                 ,guide = guide_legend(reverse=TRUE)
    #     ),
    #     label = format(value, nsmall = 1, digits=1),
    #     data=VAR.long.DF,
    #     #     fill=factor(VAR.long.DF[,VAR.y.series]
    #     #                 ,levels=labels.category.DF$variable
    #     #                 #                 ,guide = guide_legend(reverse=TRUE)
    #     #     ),
    #     ylab=VAR.Y.label,
    #     xlab=VAR.X.label,
    #     main=VAR.main.label,
    #     #     gbinwidth=1,
    #     #     stat=identity,
    #         geom="text"
    #   )#+ scale_fill_hue()
    #   
    
    
    VAR.long.DF$category<-factor(VAR.long.DF$category,
                                 levels=labels.category.DF$variable,
                                 labels=labels.category.DF$Label,
                                 ordered=TRUE)
    
    original<-ggplot(data=VAR.long.DF
                     ,aes(  x=format(x.variable,"%Y")
                            ,y=category
                            ,label = format(round(y.variable,1)
                                            , nsmall = 1
                                            , digits=1
                                            ,scientific=FALSE)
                     )  
                     ,stat=bin #identity
    )
    
    
    
    # ,ylab=
    # ,xlab=
    # ,
    
    #   y=factor(category#.[,VAR.y.series]
    #            ,levels=labels.category.DF$variable
    #   ),
    
    #     fill=factor(VAR.long.DF[,VAR.y.series]
    #                 ,levels=labels.category.DF$variable
    #                 #                 ,guide = guide_legend(reverse=TRUE)
    #     ),
    # )()
    
    
    print.figure<-original+geom_text(size=geom.text.size,hjust=1)+
        theme_bw()+
        theme(panel.grid.major = element_blank()
              , panel.grid.minor =element_line()
              , legend.position ="none"
        ) +
        theme(panel.border = element_rect()
              ,axis.ticks.y = element_blank()
        ) + 
        theme(plot.margin = unit(c(0,1, 0, 0.5)
                                 , "lines")
        )+ 
        xlab(VAR.X.label)   +
        ylab(VAR.Y.label)+
        #     main(VAR.main.label)+
        theme(strip.text.y = element_text(size = strip.text.size
                                          , angle = 360)
        )+
        theme(axis.title = element_text(size = axis.text.size 
                                        ,lineheight=.8
                                        , face="bold")
        )+ 
        theme(axis.text = element_text(size = axis.text.size # *0.5
                                       ,lineheight=.8
                                       , face="bold")
        )
    if(!is.na(VAR.facet.primary)){
        print.figure<-print.figure+facet_grid(primary~., drop=TRUE
                                              #                                           , margins=c("second")
        )
    }
    
    #     theme(panel.grid.major = element_line(size = 0))+
    #     theme(panel.grid.minor = element_line(size = 0))+
    #     theme(axis.text.x=element_text(size=axis.text.size))+
    #     theme(axis.text.y=element_text(size=axis.text.size))+
    # +scale_fill_manual(
    #     VAR.color.legend.label,
    #     values=color.list, 
    #     breaks=c(labels.category.DF$variable), 
    #     labels=c(labels.category.DF$Label),
    #     guide = guide_legend(reverse=TRUE)
    #   )
    #   
    #   print.figure<-print.figure+scale_x_discrete(
    #     breaks=    c(as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")):as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y"))),
    #     labels=    paste("'",format(as.Date(as.character(c(as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y"))):as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y"))),"%Y"),"%y"),sep="")
    #   )
    #   
    #   
    # #   
    #   print.figure<-print.figure+
    #     theme(axis.text.x=element_text(size=axis.text.size))+
    #     theme(axis.text.y=element_text(size=axis.text.size))+
    #     theme(axis.title.x=element_text(size=axis.text.size))+
    #     theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
    #     theme(plot.title=element_text(size=title.text.size))+
    #     theme(legend.title=element_text(size=legend.text.size,hjust=0))+
    #     theme(legend.text=element_text(size=legend.text.size))#+
    #       theme(plot.background=element_rect(fill="grey80",colour=NA))+
    #       theme(legend.background=element_rect(fill="grey80",colour=NA))
    #       theme(legend.key.width=unit(0.1,"npc"))
    #   print.figure<-print.figure+geom_bar(colour="black")#
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    #   
    #   print(
    #     grid.text(paste("Source: FPDS and")
    #               ,y=unit(4,"line")
    #               ,x=unit(35,"line")
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain",fontsize=6))
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    #   
    #   print(
    #     grid.text(paste("CSIS analysis")
    #               ,y=unit(3,"line")
    #               ,x=unit(38,"line")
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain",fontsize=6))
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    #   
    #   print(
    #     grid.text(paste("Available online at")
    #               ,y=unit(2,"line")
    #               ,x=unit(35,"line")
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain",fontsize=6))
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    #   
    #   
    #   print(
    #     grid.text(paste("www.csis.org/NSPIR/DoD")
    #               ,y=unit(1,"line")
    #               ,x=unit(35,"line")
    #               ,vjust="bottom",
    #               ,hjust=0,
    #               ,gp=gpar(fontface="plain",fontsize=6))
    #     , vp=vplayout(VAR.row,VAR.col)
    #   )  
    
    theme_set(old.theme)
    rm(VAR.color.legend.label,VAR.main.label,VAR.base.row,VAR.base.col,print.figure)
}



MiniBarPlot<-function(
    VAR.color.legend.label
    ,VAR.main.label
    ,VAR.X.label
    ,VAR.Y.label
    ,VAR.Coloration
    ,VAR.base.row
    ,VAR.base.col
    ,VAR.long.DF
    ,VAR.y.series
){
    if("Graph" %in% names(VAR.long.DF)){
        VAR.long.DF<-subset(VAR.long.DF, Graph==TRUE)
    }  
    
    labels.category.DF<-subset(VAR.Coloration, Figure==VAR.color.legend.label)
    
    labels.category.DF<-subset(labels.category.DF, variable %in% unique(VAR.long.DF[[VAR.y.series]]))
    
    if(nrow(labels.category.DF)==0){
        stop(paste("Error: Missing coloration data for Figure=="
                   ,VAR.color.legend.label
                   ,sep="")
        )
    }
    
    labels.category.DF<-labels.category.DF[order(labels.category.DF$Display.Order),]
    
    
    
    old.theme<-theme_set(theme_grey())
    
    
    color.list<-c(as.character(labels.category.DF$ColorRGB))
    names(color.list)<-c(labels.category.DF$variable)
    
    #   year.range<-c(as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")):as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")))
    
    #Break the data into groups of one calendar year
    #   VAR.long.DF$Fiscal.Year<-cut(VAR.long.DF$Fiscal.Year,"year")  
    
    #Turn dates into '00 style years by stripping out everything else.
    #   VAR.long.DF$Fiscal.Year<-paste("'",gsub("(^20|^19|-[0-9][0-9]-[0-9][0-9])", "", VAR.long.DF$Fiscal.Year),sep="")
    #   labels.year.DF<-paste("'",gsub("(^20|^19|-[0-9][0-9]-[0-9][0-9])", "", year.range),sep="")
    #   labels.year.DF<-labels.year.DF[order(year.range),]
    
    original<-qplot(
        format(Fiscal.Year,"%Y"),
        data=VAR.long.DF,
        y=value,
        geom="bar",
        stat="identify",
        weight=value,
        fill=factor(VAR.long.DF[,VAR.y.series],
                    levels=c(labels.category.DF$variable),
                    lables=c(labels.category.DF$Label),
                    ordered=TRUE),
        ylab=VAR.Y.label,
        xlab=VAR.X.label,
        main=VAR.main.label,
        gbinwidth=1
    )
    print.figure<-original+scale_x_discrete(
        breaks=
            c(seq(
                as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                by=2)),
        labels=
            paste("'",format(as.Date(as.character(
                c(seq(
                    as.numeric(format(min(VAR.long.DF$Fiscal.Year),"%Y")),
                    as.numeric(format(max(VAR.long.DF$Fiscal.Year),"%Y")),
                    by=2))
            ),"%Y"),"%y"),sep="")
    )
    
    print.figure<-print.figure+scale_fill_manual(VAR.color.legend.label,values=color.list, breaks=c(labels.category.DF$variable), labels=c(labels.category.DF$Label))
    print.figure<-print.figure+geom_bar(colour="black",stat="identity")
    
    
    
    print.figure<-print.figure+
        theme(axis.text.x=element_text(size=axis.text.size))+
        theme(axis.text.y=element_text(size=axis.text.size))+
        theme(axis.title.x=element_text(size=axis.text.size))+
        theme(axis.title.y=element_text(size=axis.text.size, angle=90))+
        theme(plot.title=element_text(size=title.text.size))+
        theme(legend.title=element_text(size=legend.text.size,hjust=0))+
        theme(legend.text=element_text(size=legend.text.size))
    #     theme(legend.key.width=unit(0.1,"npc"))
    print(print.figure, vp=subplot(VAR.base.row,VAR.base.col))
    
    theme_set(old.theme)
    rm(VAR.color.legend.label,VAR.main.label,VAR.base.row,VAR.base.col,print.figure)
}



library(grid)
#http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot
#Author: Julius http://stackoverflow.com/users/1320535/julius
# pos - where to add new labels
# newpage, vp - see ?print.ggplot
facetAdjust <- function(x, pos = c("up", "down"), 
                        newpage = is.null(vp), vp = NULL)
{
    # part of print.ggplot
    ggplot2:::set_last_plot(x)
    if(newpage)
        grid.newpage()
    pos <- match.arg(pos)
    p <- ggplot_build(x)
    gtable <- ggplot_gtable(p)
    # finding dimensions
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    # number of panels in the plot
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    # missing panels
    n <- space - panels
    # checking whether modifications are needed
    if(panels != space){
        # indices of panels to fix
        idx <- (space - ncol - n + 1):(space - ncol)
        # copying x-axis of the last existing panel to the chosen panels 
        # in the row above
        gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
        if(pos == "down"){
            # if pos == down then shifting labels down to the same level as 
            # the x-axis of last panel
            rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                         gtable$layout$name)
            lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
            gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
        }
    }
    # again part of print.ggplot, plotting adjusted version
    if(is.null(vp)){
        grid.draw(gtable)
    }
    else{
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(gtable)
        upViewport()
    }
    invisible(p)
}


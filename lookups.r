require(stringr)
require(plyr)

ClassifyDuration<-function(VAR.Week.Count){
    
    if (is.na(VAR.Week.Count) )
    { NA
    } else if (VAR.Week.Count < 0){ NA
    } else if (VAR.Week.Count == 0){ "1 day"
    } else if (VAR.Week.Count <=4){ "<=4 weeks"
    } else if (VAR.Week.Count<=13){ "<=13 weeks"
    } else if (VAR.Week.Count<=26.125){ "<=1/2 year"
    } else if (VAR.Week.Count<=52.25){ "<=1 year"
    } else if (VAR.Week.Count<=104.5){ "<=2 years"
    } else if (VAR.Week.Count<=261.25){ "<=5 years"
    } else if (VAR.Week.Count>261.25){  ">5 years"
    }
    else NULL
    
}

CreateDuration<-function(VAR.Week.Count){
    VAR.Week.Count<-sapply(VAR.Week.Count,ClassifyDuration)
    factor(VAR.Week.Count
           ,exclude=NULL
           
           ,levels=c(
               "1 day"
               ,"<=4 weeks"
               ,"<=13 weeks"
               ,"<=1/2 year"
               ,"<=1 year"
               ,"<=2 years"
               , "<=5 years"
               ,">5 years"
               ,NA
           )
           ,labels=c(
               
               "1\nday"
               ,"2-28\ndays"
               ,">4-13\nweeks"
               ,">1/4-1/2\nyear"
               ,">1/2-1\nyear"
               ,">1-2\nyears"
               , ">2-5\nyears"
               ,">5\n years"
               ,"Unlabeled"
           )
    )
}


ClassifySize<-function(VAR.Dollar.Value){
    
    if (is.na(VAR.Dollar.Value))
    { NA
    } else if (VAR.Dollar.Value == 0){ "Zero value"
    } else if (VAR.Dollar.Value < 0){ "Deobligation"
    } else if (VAR.Dollar.Value<250000){ "Less than $250 thousand"
    } else if (VAR.Dollar.Value<1000000){ "$250 thousand - <$1 million"
    } else if (VAR.Dollar.Value<5000000){ "$1 - <$5 million"
    } else if (VAR.Dollar.Value<25000000){ "$5 - <$25 million"
    } else if (VAR.Dollar.Value<100000000){ "$25 - <$100 million"
    } else if (VAR.Dollar.Value<500000000){ "$100 - <$500 million"
    } else if (VAR.Dollar.Value>=500000000){  "$500 million or greater"
    }
    else NULL
    
}

CreateSize<-function(VAR.Dollar.Value){
    VAR.Dollar.Value<-sapply(VAR.Dollar.Value,ClassifySize)
    factor(VAR.Dollar.Value
           ,levels=c("Deobligation"
                     , "Zero value"
                     , "Less than $250 thousand"
                     ,"$250 thousand - <$1 million"
                     ,"$1 - <$5 million"
                     ,"$5 - <$25 million"
                     , "$25 - <$100 million"
                     ,"$100 - <$500 million"
                     ,  "$500 million or greater"
           )
           ,labels=c("Deobligation"
                     ,"Zero value"
                     ,"Less than $0.25M"
                     ,"$0.25M - <$1M"
                     ,"$1 - <$5M"
                     ,"$5M - <$25M"
                     ,"$25M - <$100M"
                     ,"$100M - <$500M"
                     ,"$500M or greater"
           )
    )
}


FactorToNumber<-function(VAR.factor){
    if ((is.factor(VAR.factor))||(is.character(VAR.factor))){
        VAR.factor<-gsub('\\$','',as.character( VAR.factor))
        VAR.factor<-as.double(gsub('\\,','',as.character( VAR.factor)))
    }
    VAR.factor
}

load.FPDS.gov.customers.df<-function(
    VAR.Path
    ,VAR.Choice.Data
    ,VAR.Which.Data
){
    
    
    customers.files<-paste(Path
                           ,"Footing Data\\"
                           ,list.files(path=paste(Path,"Footing Data\\",sep="")
                                       ,pattern=paste("Footing_"
                                                      ,as.character(VAR.Choice.Data$ProdServCode.Prefix[VAR.Which.Data])
                                                      ,"Customers.*[.]csv"
                                                      ,sep=""
                                       )
                           )
                           ,sep=""
    )
    if (file.exists(customers.files)){
        FPDS.gov.customers.df<-read.tables(customers.files,
                                           header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, 
                                           stringsAsFactors=TRUE
        )
        
        rm(customers.files)
        
        FPDS.gov.customers.df<-LimitData(FPDS.gov.customers.df
                                         ,VAR.Choice.Data$Customer[VAR.Which.Data]
                                         ,VAR.Choice.Data$big.ProdServCode[VAR.Which.Data]
        )
        
        FPDS.gov.customers.df
    }
}

load.FPDS.gov.buckets.df<-function(VAR.Path,VAR.Choice.Data,VAR.Which.Data){
    buckets.files<-paste(VAR.Path,"Footing Data\\",list.files(path=paste(VAR.Path,"Footing Data\\",sep=""),pattern="Footing_Buckets.*[.]csv"),sep="")
    
    FPDS.gov.buckets.df<-read.tables(buckets.files,
                                     header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, 
                                     stringsAsFactors=FALSE
    )
    
    FPDS.gov.buckets.df$Contracting.Department.ID<-factor(FPDS.gov.buckets.df$Contracting.Department.ID)
    FPDS.gov.buckets.df$Contracting.Department.Name<-factor(FPDS.gov.buckets.df$Contracting.Department.Name)
    FPDS.gov.buckets.df$Product.or.Service.Code<-factor(FPDS.gov.buckets.df$Product.or.Service.Code)
    FPDS.gov.buckets.df$Product.or.Service.Description<-factor(FPDS.gov.buckets.df$Product.or.Service.Description)
    
    rm(buckets.files)
    
    
    
    FPDS.gov.buckets.df<-LimitData(FPDS.gov.buckets.df
                                   ,VAR.Choice.Data$Customer[VAR.Which.Data]
                                   ,VAR.Choice.Data$big.ProdServCode[VAR.Which.Data]
    )
    FPDS.gov.buckets.df
}


#Source: http://stackoverflow.com/questions/2104483/how-to-read-table-multiple-files-into-a-single-table-in-r
read.tables <- function(file.names, ...) {
    require(plyr)
    ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)))
}



LimitData <- function(
    var.FPDS.gov
    ,VAR.customer=NULL
    ,VAR.big.ProdServ=NULL) 
{
    var.FPDS.gov<-subset(var.FPDS.gov,select=-c(Filename))
    var.FPDS.gov<-append_contract_fixes(Path,var.FPDS.gov)
    var.FPDS.gov<-apply_lookups(Path,var.FPDS.gov)
    if(!(is.null(VAR.customer)) && (VAR.customer=="") &&!(VAR.customer=="") && !(VAR.customer=="D3")) {
        var.FPDS.gov<-subset(var.FPDS.gov,Customer==VAR.customer)
    } else if (!(is.null(VAR.customer)) && (VAR.customer=="D3")){
        var.FPDS.gov<-subset(var.FPDS.gov,Customer  %in% c("Defense","State and IAP"))
    }
    
    if(!(is.null(VAR.big.ProdServ)) && VAR.big.ProdServ=="Services" && ("ProductOrServiceArea" %in% names(var.FPDS.gov))){
        var.FPDS.gov<-subset(var.FPDS.gov,ProductOrServiceArea %in% c("PAMS","MED","FRS&C","ICT","ERS","R&D"))
    }
    else if(!(is.null(VAR.big.ProdServ))&& VAR.big.ProdServ!=""&& ("ProductOrServiceArea" %in% names(var.FPDS.gov))){
        #     stop(paste("In LimitData, VAR.big.Prodserv = ",VAR.big.ProdServ,"which is not handled."))
        warning(paste("In LimitData, VAR.big.Prodserv = ",VAR.big.ProdServ,"which is not handled."))
    }
    var.FPDS.gov
}

LimitScope <- function(var.FPDS.gov, var.main.DF) {
    var.FPDS.gov<-subset(var.FPDS.gov,Fiscal.Year>=min(subset(var.main.DF$Fiscal.Year
                                                              ,!is.na(var.main.DF$Fiscal.Year
                                                              ))))
    var.FPDS.gov<-subset(var.FPDS.gov,Fiscal.Year<=max(subset(var.main.DF$Fiscal.Year
                                                              ,!is.na(var.main.DF$Fiscal.Year)
    )))
    var.FPDS.gov
}

import_SQLserver_file <- function(VAR.Path
                                  , VAR.prefix
                                  , VAR.file.name) {
    import.data.file <-read.csv(
        paste(
            #       VAR.Path,
            "Data\\",VAR.prefix,VAR.file.name,sep=""),
        header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE, 
        stringsAsFactors=TRUE
    )
    
    
    #   import.data.file<-subset(import.data.file,select =-c(Row))
    
    #   import.data.file<-melt(import.data.file, id.var=c("Fiscal.Year","Section","Total","Checksum"))
    #   
    #   import.data.file<- join(
    #     import.data.file, 
    #     VAR.lookup,
    #     match="first"
    #   )
    
    #   import.zero.check<-aggregate(abs(import.data.file$value), by=list(import.data.file$variable),FUN = "max")
    #   names(import.zero.check)<-c("variable","value")
    #   import.zero.check<-subset(import.zero.check,import.zero.check$value>0)
    
    #   import.data.file<-subset(import.data.file,import.data.file$variable %in% import.zero.check$variable)
    
    
    #   NA.check.df<-subset(import.data.file, is.na(variable.sum), select=c("variable","variable.detail","variable.sum"))
    #   if(nrow(NA.check.df)>0){
    #     print(unique(NA.check.df))
    #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
    #   }
    
    #   NA.check.df<-subset(import.data.file, is.na(variable.detail), select=c("variable","variable.detail","variable.sum"))
    #   if(nrow(NA.check.df)>0){
    #     print(unique(NA.check.df))
    #     stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
    #   }
    
    
    import.data.file
}



import_figures_and_tables <- function(VAR.Path
                                      , VAR.choice.data
                                      , VAR.which.data
                                      , VAR.choice.figures
                                      ,VAR.which.figure
                                      ,VAR.logfile
                                      #                                       ,VAR.Categories
) 
{
    
    #   if(VAR.choice.figures$form[VAR.which.figure]=="chart"){
    if (!is.na(VAR.choice.figures$file.type[VAR.which.figure])&&VAR.choice.figures$file.type[VAR.which.figure]=="SQL output"){
        imported.data<-import_SQLserver_file(VAR.Path
                                             ,paste(if(VAR.choice.figures$use.all.agencies[VAR.which.figure]==TRUE){
                                                 "All_Agencies_"
                                             }
                                             else{
                                                 as.character(VAR.choice.data$Prefix[VAR.which.data])
                                             }
                                             
                                             ,as.character(VAR.choice.data$ProdServCode.Prefix[VAR.which.data])
                                             ,sep=""
                                             )
                                             ,VAR.choice.figures$data.file[VAR.which.figure]
        )
        #       debug(apply_lookups)
        #       imported.data<-apply_lookups(Path,imported.data)
    }
    else{
        #       
        #       if(!is.na(VAR.choice.figures$lookup.file[VAR.which.figure])){
        # #         Data.Lookup<- read.csv(
        # #           paste(VAR.Path
        # #                 ,"Lookups\\"
        # #                 ,"LOOKUP_"
        # #                 ,VAR.choice.figures$lookup.file[VAR.which.figure]
        # #                 ,sep=""
        # #           )  
        # #           , header=TRUE
        # #           , sep=","
        # #           , na.strings="NA"
        # #           , dec="."
        # #           , strip.white=TRUE
        # #           , stringsAsFactors=TRUE
        # #         )
        # #         
        # #       }
        #     debug(import_figures_and_tables_file)
        #       #     stop("hammer")
        imported.data<-import_figures_and_tables_file(VAR.Path
                                                      , VAR.choice.data
                                                      , VAR.which.data
                                                      , VAR.choice.figures
                                                      , VAR.which.figure
                                                      ,paste(as.character(VAR.choice.data$Prefix[VAR.which.data])
                                                             ,as.character(VAR.choice.data$ProdServCode.Prefix[VAR.which.data]),sep="")
                                                      ,VAR.choice.figures$data.file[VAR.which.figure]
                                                      #                                                    ,Data.Lookup
        )
        
    }
    #   }
    #   else if (VAR.choice.figures$form[VAR.which.figure]=="table"){
    #     imported.data <-read.csv(
    #       paste(Path
    #             ,"Data\\"
    #             ,as.character(choice.data$Prefix[which.data])
    #             ,as.character(choice.data$ProdServCode.Prefix[which.data])
    #             ,VAR.choice.figures$data.file[VAR.which.figure],sep="")
    #       
    #       ,header=TRUE
    #       , sep=","
    #       , na.strings=""
    #       , dec="."
    #       , strip.white=TRUE
    #       , stringsAsFactors=TRUE
    #     )
    #     
    #   }
    #   if(VAR.choice.figures$file.type[VAR.which.figure]=="single"){
    
    #   }  
    #   else if(VAR.choice.figures$file.type[VAR.which.figure]=="double"){
    #     if(VAR.choice.figures$form[VAR.which.figure]=="table"){
    #       colnames(imported.data)[colnames(imported.data)=="Section"]<-VAR.choice.figures$double.section[VAR.which.figure]
    #       
    #     }
    #     imported.data<-apply_lookups(Path,imported.data)
    #     
    #     imported.data<-subset(imported.data  
    #                                ,imported.data[[choice.figures$section.variable[VAR.which.figure]]] %in% VAR.Categories$Category)
    #     
    #     
    #     
    #     if(VAR.choice.figures$form[VAR.which.figure]=="chart"){
    #       Data.by.Alternate<-subset(Data.by.Both
    #                                 ,!(Data.by.Both[,choice.figures$section.variable[VAR.which.figure]] %in% VAR.Categories$Category))
    #       
    #       
    #       
    #       CompareAbstractVariables(Data.by.Alternate
    #                                ,paste(VAR.choice.figures$data.file[VAR.which.figure],".by.alternate.type",sep="")
    #                                ,imported.data
    #                                ,paste(VAR.choice.figures$data.file[VAR.which.figure],".by.which.type",sep="")
    #                                ,choice.figures$lookup.series.variable[[figureNUM]]
    #                                ,VAR.logfile
    #       )
    #       
    #     }
    #     
    #   }
    
    if("Customer" %in% names(imported.data ) &
           !is.na(VAR.choice.data$Customer[VAR.which.data]) & 
           VAR.choice.data$Customer[VAR.which.data]!=""){
        imported.data<-subset(imported.data,Customer==VAR.choice.data$Customer[VAR.which.data])
    }
    
    
    
    imported.data<-apply_lookups(Path,imported.data)
    
    # imported.data$Fiscal.Year <-as.Date(paste("9/30/",as.character(imported.data$Fiscal.Year),sep=""),"%m/%d/%Y")
    imported.data  
}


import_country_file <- function(VAR.Path
                                , VAR.choice.data
                                , VAR.which.data
                                , VAR.choice.figures
                                , VAR.which.figure
                                , VAR.prefix
                                , VAR.file.name
) {
    import.data.file <-read.csv(
        paste(VAR.Path,"Data\\",VAR.prefix,VAR.file.name,sep=""),
        header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, 
        stringsAsFactors=TRUE
    )
    id.var.list<-names(import.data.file)
    id.var.list<-subset(id.var.list
                        , !(id.var.list>="x1900"
                            & id.var.list<="x2200")
    )
    import.data.file<-melt(import.data.file
                           , id.var=id.var.list
    )
    
    import.data.file$value<-FactorToNumber(import.data.file$value)
    
    
    colnames(import.data.file)[colnames(import.data.file)=="variable"]<-"Fiscal.Year"
    
    
    import.data.file$Fiscal.Year<-str_sub(import.data.file$Fiscal.Year,2)
    
    import.data.file<-apply_lookups(Path,import.data.file)
    
    #   colnames(import.data.file)[colnames(import.data.file)=="value"]<-"Unmodified.Value"
    if('Destination.Country' %in% names(import.data.file)){
        import.data.file<-ddply(import.data.file
                                , .(Source.Country
                                    ,Destination.Country
                                    ,Indicator)
                                , transform
                                , rollingmean = rollmean(na.fill(value,0)
                                                         ,3
                                                         ,align="right"
                                                         ,na.pad=TRUE
                                )
        )
    }
    else if ('Arms.Type' %in% names(import.data.file)){
        import.data.file<-ddply(import.data.file
                                , .(Source.Country
                                    ,Arms.Type
                                    ,Indicator)
                                , transform
                                , rollingmean = rollmean(na.fill(value,0)
                                                         ,3
                                                         ,align="right"
                                                         ,na.pad=TRUE
                                )
        )
    }
    #   colnames(import.data.file)[colnames(import.data.file)=="Section"]<-VAR.choice.figures$section.variable[VAR.which.figure]
    #   
    #   if(max(abs(import.data.file$Checksum))>0.0000001){
    #     stop (paste(
    #       "Non-zero check sum value (",
    #       max(abs(import.data.file$Checksum)),
    #       ") in file ",
    #       VAR.file.name
    #     ))
    #   } 
    #   
    #   import.data.file<-subset(import.data.file,select =-c(Row))
    #   
    #   import.data.file<-melt(import.data.file
    #                          , id.var=c("Fiscal.Year"
    #                                     ,VAR.choice.figures$section.variable[VAR.which.figure]
    #                                     ,"Total"
    #                                     ,"Checksum"))
    #   
    #   import.data.file<-read_and_join(
    #     VAR.Path
    #     ,"Lookup_name_fix.csv"
    #     ,import.data.file
    #   )
    #   
    #   
    #   NA.check.df<-subset(import.data.file
    #                       , is.na(name.fix)
    #                       , select=c(variable)
    #   )
    #   
    #   if(nrow(NA.check.df)>0){
    #     print(unique(NA.check.df))
    #     stop(paste(nrow(NA.check.df),"rows of NAs generated in name.fix from Lookup_name_fix.csv"))
    #   }
    #   
    #   
    #   import.data.file$name.fix<-factor(import.data.file$name.fix)
    #   
    #   colnames(import.data.file)[colnames(import.data.file)=="name.fix"]<-VAR.choice.figures$lookup.series.variable[VAR.which.figure]
    #   
    #   
    #   
    #   import.zero.check<-aggregate(abs(import.data.file$value)
    #                                , by=list(import.data.file[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]])
    #                                ,FUN = "max")
    #   names(import.zero.check)<-c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
    #                               ,"value")
    #   import.zero.check<-subset(import.zero.check,import.zero.check$value>0)
    #   
    #   import.data.file<-subset(import.data.file
    #                            ,import.data.file[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]]
    #                            %in% import.zero.check[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]])
    #   
    #   if(any(is.na(VAR.choice.figures$y.variable[VAR.which.figure]))){
    #     NA.check.df<-subset(import.data.file
    #                         , is.na(import.data.file[[VAR.choice.figures$x.variable[VAR.which.figure]]])
    #                         , select=c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
    #                                    ,VAR.choice.figures$x.variable[VAR.which.figure])
    #     )
    #     
    #     if(nrow(NA.check.df)>0){
    #       print(unique(NA.check.df))
    #       stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
    #     }
    #     
    #   }
    #   
    #   if(!is.na(VAR.choice.figures$y.variable[VAR.which.figure])){
    #     NA.check.df<-subset(import.data.file
    #                         , is.na(import.data.file[[VAR.choice.figures$y.variable[VAR.which.figure]]])
    #                         , select=c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
    #                                    ,VAR.choice.figures$x.variable[VAR.which.figure]
    #                                    ,VAR.choice.figures$y.variable[VAR.which.figure])
    #     )
    #     if(nrow(NA.check.df)>0){
    #       print(unique(NA.check.df))
    #       stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
    #     }
    #   }
    #   
    #   
    import.data.file
}





import_figures_and_tables_file <- function(VAR.Path
                                           , VAR.choice.data
                                           , VAR.which.data
                                           , VAR.choice.figures
                                           , VAR.which.figure
                                           , VAR.prefix
                                           , VAR.file.name
                                           ,VAR.lookup
) {
    import.data.file <-read.csv(
        paste(VAR.Path,"Data\\",VAR.prefix,VAR.file.name,sep=""),
        header=TRUE, sep=",", na.strings=c("NA","NULL",""), dec=".", strip.white=TRUE, 
        stringsAsFactors=TRUE
    )
    
    #   colnames(import.data.file)[colnames(import.data.file)=="Section"]<-VAR.choice.figures$double.section[VAR.which.figure]
    
    
    if("checksum" %in% names(import.data.file)){
        if(max(abs(import.data.file$Checksum))>0.0000001){
            stop (paste(
                "Non-zero check sum value (",
                max(abs(import.data.file$Checksum)),
                ") in file ",
                VAR.file.name
            ))
        } 
    }
    
    if("Row" %in% names(import.data.file)){
        import.data.file<-subset(import.data.file,select =-c(Row))
    }
    
    import.data.file<-melt(import.data.file
                           , id.var=c("Fiscal.Year"
                                      ,VAR.choice.figures$wide.section.variable[VAR.which.figure]
                                      ,"Total"
                                      ,"Checksum"))
    
    import.data.file<-read_and_join(
        VAR.Path
        ,"Lookup_name_fix.csv"
        ,import.data.file
    )
    
    
    NA.check.df<-subset(import.data.file
                        , is.na(name.fix)
                        , select=c(variable)
    )
    
    if(nrow(NA.check.df)>0){
        print(unique(NA.check.df))
        stop(paste(nrow(NA.check.df),"rows of NAs generated in name.fix from Lookup_name_fix.csv"))
    }
    
    
    import.data.file$name.fix<-factor(import.data.file$name.fix)
    
    colnames(import.data.file)[colnames(import.data.file)=="name.fix"]<-VAR.choice.figures$lookup.series.variable[VAR.which.figure]
    
    import.data.file<- join(
        import.data.file, 
        VAR.lookup,
        match="first"
    )
    
    import.zero.check<-aggregate(abs(import.data.file[VAR.choice.figures[,y.variable[VAR.which.figure]]])
                                 , by=list(import.data.file[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]])
                                 ,FUN = "max")
    names(import.zero.check)<-c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
                                ,VAR.choice.figures$y.variable[VAR.which.figure])
    import.zero.check<-subset(import.zero.check,import.zero.check[,VAR.choice.figures$y.variable[VAR.which.figure]]>0)
    
    import.data.file<-subset(import.data.file
                             ,import.data.file[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]]
                             %in% import.zero.check[[VAR.choice.figures$lookup.series.variable[VAR.which.figure]]])
    
    if(any(is.na(VAR.choice.figures$y.variable[VAR.which.figure]))){
        NA.check.df<-subset(import.data.file
                            , is.na(import.data.file[[VAR.choice.figures$x.variable[VAR.which.figure]]])
                            , select=c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
                                       ,VAR.choice.figures$x.variable[VAR.which.figure])
        )
        
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
        }
        
    }
    
    if(!is.na(VAR.choice.figures$y.variable[VAR.which.figure])){
        NA.check.df<-subset(import.data.file
                            , is.na(import.data.file[[VAR.choice.figures$y.variable[VAR.which.figure]]])
                            , select=c(VAR.choice.figures$lookup.series.variable[VAR.which.figure]
                                       ,VAR.choice.figures$x.variable[VAR.which.figure]
                                       ,VAR.choice.figures$y.variable[VAR.which.figure])
        )
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in variable.sum from ",VAR.file.name))
        }
    }
    
    import.data.file
}


NA.check<-function(VAR.df
                   , VAR.input
                   , VAR.output
                   , VAR.file
){
    
    NA.check.df<-subset(VAR.df
                        , is.na(VAR.df[[VAR.output]])
                        , select=c(VAR.input)
    )
    
    if(nrow(NA.check.df)>0){
        print(unique(NA.check.df))
        stop(paste(nrow(NA.check.df)
                   ,"rows of NAs generated in "
                   ,VAR.output
                   ,"from "
                   ,VAR.file)
        )
    }
}

read_and_join<-function(VAR.path,
                        VAR.file,
                        VAR.existing.df,
                        directory="Lookups\\",
                        by=NULL){
    lookup.file<-read.csv(
        paste(VAR.path,directory,VAR.file,sep=""),
        header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE,
        stringsAsFactors=TRUE
    )
    
    
    
    #   #Fixes for Excel's penchent to drop leading 0s.
    if("Contracting.Agency.ID" %in% names(lookup.file)){
        #     append.fixed.tasks$Fair.Opportunity.Limited.Sources[is.na(append.fixed.tasks$Fair.Opportunity.Limited.Sources)]<-""
        #     lookup.file$Contracting.Agency.ID<-sprintf("%04d",lookup.file$Contracting.Agency.ID)
        #     debug(LeadingZeros)
        #       lookup.file$Contracting.Agency.ID<-"0000"&lookup.file$Contracting.Agency.ID
        lookup.file$Contracting.Agency.ID<-factor(str_pad(lookup.file$Contracting.Agency.ID,4,side="left",pad="0"))
        VAR.existing.df$Contracting.Agency.ID<-as.character(VAR.existing.df$Contracting.Agency.ID)
        #         VAR.existing.df$Contracting.Agency.ID[VAR.existing.df$Contracting.Agency.ID==""]<-"0000"
        VAR.existing.df$Contracting.Agency.ID[is.na(VAR.existing.df$Contracting.Agency.ID=="")]<-"0000"
        #         VAR.existing.df$Contracting.Agency.ID<-factor(VAR.existing.df$Contracting.Agency.ID)
        VAR.existing.df$Contracting.Agency.ID<-factor(str_pad(VAR.existing.df$Contracting.Agency.ID,4,side="left",pad="0"))
        #       lookup.file$Contracting.Agency.ID<-factor(lookup.file$Contracting.Agency.ID,str_pad(x,30,side=left,pad="0"))
        #       VAR.existing.df$Contracting.Agency.ID<-factor(lookup.file$Contracting.Agency.ID,str_pad(x,30,side=left,pad="0"))
        #       VAR.existing.df$Contracting.Agency.ID<-ldply(VAR.existing.df$Contracting.Agency.ID,as.character)
    }
    #   }
    #   if("Contracting.Department.ID" %in% names(lookup.file)){
    #     lookup.file$Contracting.Department.ID<-sprintf("%04d",lookup.file$Contracting.Department.ID)
    #     
    #   }
    
    
    if("ï..CSIScontractID" %in% colnames(lookup.file)){
        colnames(lookup.file)[colnames(lookup.file)=="ï..CSIScontractID"]<-"CSIScontractID"
    }
    
    if("CSIScontractID" %in% colnames(lookup.file)){
        if(!is.numeric(lookup.file$CSIScontractID)){
            lookup.file$CSIScontractID<-as.numeric(levels(lookup.file$CSIScontractID))
        }
    }
    
    if(is.null(by)){
        VAR.existing.df<- plyr::join(
            VAR.existing.df,
            lookup.file,
            match="first"
        )
    }
    else{
        VAR.existing.df<- plyr::join(
            VAR.existing.df,
            lookup.file,
            match="first",
            by=by
            
        )
        
    }
    
    #   print(head(VAR.file))
    #   print(head(VAR.existing.df))
    VAR.existing.df
}



label.offers2 <- function(x){
    if(is.null(x) | is.na(x) | x==0){
        offers<-"Null or Zero"
    }
    else if (x==1){
        offers<-"One"
    }
    else if (x>1){
        offers<-"Multiple"
    }
    else{
        offers<-"Error"
    }
    offers
    
    
}

append_contract_fixes<- function(VAR.path,VAR.df){
    #   print(nrow(VAR.df))
    
    
    #Step 1, apply our known fixes to the data
    if("X" %in% names(VAR.df))  {
        VAR.df<-subset(VAR.df,select =-c(X))
    }
    
    append.fixed.tasks<-read.csv(
        paste(VAR.path,"Lookups\\","APPEND_Fixed_Tasks_webtool.csv",sep=""),
        header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE,
        stringsAsFactors=TRUE,
    )                            
    
    append.fixed.tasks$Fair.Opportunity.Limited.Sources[is.na(append.fixed.tasks$Fair.Opportunity.Limited.Sources)]<-""
    append.fixed.tasks$IDV.Part.8.Or.Part.13[is.na(append.fixed.tasks$IDV.Part.8.Or.Part.13)]<-""
    append.fixed.tasks$IDV.Multiple.Or.Single.Award.IDV[is.na(append.fixed.tasks$IDV.Multiple.Or.Single.Award.IDV)]<-""
    append.fixed.tasks$IDV.Type[is.na(append.fixed.tasks$IDV.Type)]<-""
    
    
    
    
    append.fixed.tasks<-subset(
        append.fixed.tasks, 
        select=c(
            names(VAR.df)
        ))
    
    
    #   append.fixed.tasks<-subset(
    #     append.fixed.tasks, 
    #     select=c(
    #       Contracting.Agency.Name, 
    #       Contracting.Agency.ID, 
    #       Contracting.Department.ID, 
    #       Award.or.IDV.Type, 
    #       IDV.Part.8.Or.Part.13, 
    #       IDV.Multiple.Or.Single.Award.IDV, 
    #       IDV.Type, 
    #       Extent.Competed, 
    #       Fair.Opportunity.Limited.Sources, 
    #       Number.of.Offers.Received, 
    #       Fiscal.Year, 
    #       Action.Obligation, 
    #       Actions, 
    #       Download.Date
    #     ))
    #
    
    
    #   if("IDV.Part.8.Or.Part.13" %in% names(VAR.df)){
    # #     print(paste("typeof","VAR.df",typeof(VAR.df$IDV.Part.8.Or.Part.13)))
    # #     print(paste("typeof","append.fixed.tasks",typeof(append.fixed.tasks$IDV.Part.8.Or.Part.13)))
    # #     append.fixed.tasks$IDV.Part.8.Or.Part.13<-as.integer(append.fixed.tasks$IDV.Part.8.Or.Part.13)
    #     
    # #     print(paste("typeof","VAR.df[1]",typeof(VAR.df$IDV.Part.8.Or.Part.13[1])))
    # #     print(paste("typeof","append.fixed.tasks[1]",typeof(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
    #     
    #     #    append.fixed.tasks$IDV.Part.8.Or.Part.13<-factor(append.fixed.tasks$IDV.Part.8.Or.Part.13)
    #     
    # #     print(paste("is.character","VAR.df",is.character(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
    # #     print(paste("is.vector","append.fixed.tasks",is.vector(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
    #     
    #     append.fixed.tasks$IDV.Part.8.Or.Part.13<-factor(append.fixed.tasks$IDV.Part.8.Or.Part.13)
    #     
    #     print(paste("is.vector","append.fixed.tasks",is.vector(append.fixed.tasks$IDV.Part.8.Or.Part.13[1])))
    #   }
    
    #   print(sum(append.fixed.tasks$value))
    #  append.fixed.tasks<-subset(append.fixed.tasks, select=c(Contracting.Agency.Name, Contracting.Agency.ID, Contracting.Department.ID, Award.or.IDV.Type, IDV.Part.8.Or.Part.13, IDV.Multiple.Or.Single.Award.IDV, IDV.Type, Extent.Competed, Fair.Opportunity.Limited.Sources, Number.of.Offers.Received, Fiscal.Year, variable, value, Download.Date))
    #   print(sum(append.fixed.tasks$value))
    #   print(sum(VAR.df$value))
    
    
    VAR.df$Action.Obligation<-FactorToNumber(VAR.df$Action.Obligation)
    
    VAR.df$Actions<-FactorToNumber(VAR.df$Actions)  
    
    
    VAR.df<-rbind(VAR.df, append.fixed.tasks)
    #   print(sum(VAR.df$value))
    print("LOOKUP_Fixed_Tasks_webtool.csv")
    print(head(VAR.df))
    print(tail(VAR.df))
    
    print(head(append.fixed.tasks))
    
    VAR.df
}
#***********************Standardize Variable Names
standardize_variable_names<- function(VAR.Path,VAR.df){
    #***Standardize variable names
    NameList<-read.csv(
        paste(
                  VAR.Path,
            "Lookups\\","Lookup_StandardizeVariableNames.csv",sep=""),
        header=TRUE, sep=",", na.strings=c("NA","NULL"), dec=".", strip.white=TRUE, 
        stringsAsFactors=FALSE
    )
    
    for(x in 1:nrow(NameList)){
        colnames(VAR.df)[toupper(colnames(VAR.df))==toupper(NameList$Original[[x]])]<-
            NameList$Replacement[[x]]
    }
    
    VAR.df
}


#***********************Apply Lookups***********************
apply_lookups<- function(VAR.path,VAR.df){
    VAR.df<-standardize_variable_names(VAR.path,VAR.df)
    
    
    #***Pivot the data frame when necessary.
    #   if (!is.na(choice.figures$wide.section.variable[figureNUM])){
    #     #Get a list of variable names that aren't years
    #     id.var.list<-names(VAR.df)
    #     id.var.list<-subset(id.var.list
    #                         , !(id.var.list>="x1900"
    #                             && id.var.list<="x2200")
    #     )
    #     VAR.df<-melt(VAR.df
    #                  , id.var=id.var.list
    #     )
    #     VAR.df$value<-FactorToNumber(VAR.df$value)
    #     
    #     colnames(VAR.df)[colnames(VAR.df)=="value"]<-choice.figures$wide.section.variable[figureNUM]
    #     
    #     colnames(VAR.df)[colnames(VAR.df)=="variable"]<-"Fiscal.Year"
    #       
    #       VAR.df$Fiscal.Year<-str_sub(VAR.df$Fiscal.Year,2)
    #     #Rearranging the raw data so that it is long rather than wide. A fairly minor step because we only have two numerical variables.
    #     #   VAR.df<-melt(VAR.df, measure.vars=c(
    #     #     "Action.Obligation",
    #     #     "Actions"  
    #     #   ))
    #     #                                      
    #     #   print(paste("Post melt"))
    #     #   print(head(VAR.df))
    #     
    #     #       Contracting.Agency.Name, 
    #     #       Contracting.Agency.ID, 
    #     #       Contracting.Department.ID, 
    #     #       Award.or.IDV.Type, 
    #     #       IDV.Part.8.Or.Part.13, 
    #     #       IDV.Multiple.Or.Single.Award.IDV, 
    #     #       IDV.Type, 
    #     #       Extent.Competed, 
    #     #       Fair.Opportunity.Limited.Sources, 
    #     #       Number.of.Offers.Received, 
    #     #       Fiscal.Year, 
    #     #       Action.Obligation, 
    #     #       Actions, 
    #     #       Download.Date
    #     #   print("Start lookup")
    #     
    #     
    #   }
    #   
    
    
    
    #***Join relevant variables to lookup tables
    
    if("Contracting.Agency.ID" %in% names(VAR.df))
    {
        
        if("Contracting.Department.ID" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Contracting.Department.ID))
        }
        if("Contracting.Agency.Name"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Contracting.Agency.Name))
        }
        if("Customer"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Customer))
        }
        if("SubCustomer"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(SubCustomer))
        }
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Contracting_Agencies.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(Contracting.Agency.Name), select=c("Contracting.Agency.ID"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contracting.Agency.Name"))
        }
        
    }
    else if("Contracting.Department.ID" %in% names(VAR.df)){
        
        if("Contracting.Agency.Name"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Contracting.Agency.Name))
        }
        if("Customer"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Customer))
        }
        if("SubCustomer"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(SubCustomer))
        }
        names(VAR.df)[which(names(VAR.df)=="Contracting.Department.ID")]<-"Contracting.Agency.ID"
        #     stop("safety")
        
        #     debug(read_and_join)
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Contracting_Agencies.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(Contracting.Agency.Name), select=c("Fiscal.Year","Contracting.Agency.ID"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contracting.Agency.Name"))
        }
    }
    
    
    if("Customer" %in% names(VAR.df) && "SubCustomer" %in% names(VAR.df)){
        if("SubCustomer.sum"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(SubCustomer.sum))
        }
        
        #     stop("hammer time")
        #Handle NA values if present
        if(any(is.na(VAR.df$SubCustomer))){
            #Make sure unlabeled is within the list of levels
            if (!("Uncategorized" %in% levels(VAR.df$SubCustomer))){
                VAR.df$SubCustomer<-addNA(VAR.df$SubCustomer,ifany=TRUE)
                levels(VAR.df$SubCustomer)[is.na(levels(VAR.df$SubCustomer))] <- "Uncategorized"
            }
        }
        if(any(is.na(VAR.df$Customer))){
            #Make sure unlabeled is within the list of levels
            if (!("Uncategorized" %in% levels(VAR.df$Customer))){
                VAR.df$Customer<-addNA(VAR.df$Customer,ifany=TRUE)
                levels(VAR.df$Customer)[is.na(levels(VAR.df$Customer))] <- "Uncategorized"
            }
        }
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"Lookup_SubCustomer.csv",VAR.df)
        NA.check.df<-subset(VAR.df,is.na(SubCustomer.sum), select=c("Customer","SubCustomer"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.detail"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.component"))
        }
        
    }
    else if ("SubCustomer" %in% names(VAR.df)){
        stop("Customer is missing from the table, SubCustomer does not stand alone.")
    }
    else if("Customer" %in% names(VAR.df)){
        
        #     stop("hammer time")
        #Handle NA values if present
        if(any(is.na(VAR.df$Customer))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$Customer))){
                VAR.df$Customer<-addNA(VAR.df$Customer,ifany=TRUE)
                levels(VAR.df$Customer)[is.na(levels(VAR.df$Customer))] <- "Unlabeled"
            }
        }
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"Lookup_Customer.csv",VAR.df)
        NA.check.df<-subset(VAR.df,is.na(Customer.sum), select=c("Customer","Customer.sum"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Customer.sum"))
        }
        
    }
    
    
    if("Funder" %in% names(VAR.df) && "SubFunder" %in% names(VAR.df)){
        
        if("SubFunder.Sum"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(SubFunder.Sum))
        }
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_SubFunder.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(SubFunder.Sum), select=c("Fiscal.Year","Funder","SubFunder"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in SubFunder.Sum"))
        }
    }
    
    if("Product.or.Service.Code" %in% names(VAR.df))
    {
        if(is.integer(VAR.df$Product.or.Service.Code)){
            VAR.df$Product.or.Service.Code<-factor(VAR.df$Product.or.Service.Code)
        }
        
        if("Product.or.Service.Description" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Product.or.Service.Description))
        }
        
        #           debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"Lookup_ProdServ_Codes.csv",VAR.df)
        #         NA.check.df<-subset(VAR.df, is.na(ProductOrServiceArea), select=c("Product.or.Service.Code"))
        #         if(nrow(NA.check.df)>0){
        #             print(unique(NA.check.df))
        #             stop(paste(nrow(NA.check.df),"rows of NAs generated in ProductOrServiceArea"))
        #         }
    }
    
    
    if(("OMBagencyCode" %in% names(VAR.df))
       #     &"OMBagencyName" %in% names(VAR.df)
       &"OMBbureauCode" %in% names(VAR.df)
       #     &"OMBbureauname" %in% names(VAR.df)
    )
    {
        #   
        if("OMBagencyName" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(OMBagencyName))
        }
        if("OMBbureauname"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(OMBbureauname))
        }
        #   if("Customer"%in% names(VAR.df)){
        #     VAR.df<-subset(VAR.df, select=-c(Customer))
        #   }
        #   if("SubCustomer"%in% names(VAR.df)){
        #     VAR.df<-subset(VAR.df, select=-c(SubCustomer))
        #   }
        #       debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_OMBagencyBureau.csv",VAR.df)
        NA.check.df<-subset(VAR.df
                            , is.na(CSISbureau) #& !is.na(OMBbureauCode)
                            , select=c("OMBagencyCode"
                                       ,"OMBagencyName"
                                       ,"OMBbureauCode"
                                       ,"OMBbureauname"
                                       ,"CSISbureau"
                            )
        )
        
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in CSISbureau"))
        }
        
    }
    
    else if("ServicesCategory.detail" %in% names(VAR.df))
    {
        
        if("ServicesCategory.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(ServicesCategory.sum))
        }
        
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Buckets.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(ServicesCategory.sum), select=c("Fiscal.Year","ServicesCategory.detail"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in ServicesCategory.sum"))
        }
        
    }
    else if("ProductOrServiceArea" %in% names(VAR.df))
    {
        #Handle NA values if present
        if(any(is.na(VAR.df$ProductOrServiceArea))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$ProductOrServiceArea))){
                VAR.df$ProductOrServiceArea<-addNA(VAR.df$ProductOrServiceArea,ifany=TRUE)
                levels(VAR.df$ProductOrServiceArea)[is.na(levels(VAR.df$ProductOrServiceArea))] <- "Unlabeled"
            }
        }
        
        if("ServicesCategory.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(ServicesCategory.sum))
        }
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Buckets.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(ServicesCategory.sum), select=c("ProductOrServiceArea"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in ServicesCategory.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(ServicesCategory.detail), select=c("ProductOrServiceArea"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in ServicesCategory.detail"))
        }
        
    }
    
    
    if("MajorCommandID" %in%  names(VAR.df) && "ContractingOfficeID" %in%  names(VAR.df)){
        
        if("MajorCommandCode"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(MajorCommandCode))
        }
        
        if("MajorCommandName"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(MajorCommandName))
        }
        
        
        #     stop("hammer time")
        
        #Handle NA values if present
        if(any(is.na(VAR.df$MajorCommandID))){
            #Make sure unlabeled is within the list of levels
            if (!("Uncategorized" %in% levels(VAR.df$MajorCommandID))){
                VAR.df$MajorCommandID<-addNA(VAR.df$MajorCommandID,ifany=TRUE)
                levels(VAR.df$MajorCommandID)[is.na(levels(VAR.df$MajorCommandID))] <- "Uncategorized"
            }
        }
        
        VAR.df<-read_and_join(VAR.path,"Lookup_MajorCommandID.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(MajorCommandCode), select=c("MajorCommandID","MajorCommandCode","MajorCommandName"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in MajorCommandCode"))
        }
        #     
        #     NA.check.df<-subset(VAR.df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        #     if(nrow(NA.check.df)>0){
        #       print(unique(NA.check.df))
        #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.detail"))
        #     }
        #     
        #     NA.check.df<-subset(VAR.df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        #     if(nrow(NA.check.df)>0){
        #       print(unique(NA.check.df))
        #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.component"))
        #     }
        #     
    }
    else if("MajorCommandID" %in%  names(VAR.df)){
        
        if("MajorCommandCode"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(MajorCommandCode))
        }
        
        if("MajorCommandName"%in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(MajorCommandName))
        }
        
        #Handle NA values if present
        if(any(is.na(VAR.df$MajorCommandID))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$MajorCommandID))){
                VAR.df$MajorCommandID<-addNA(VAR.df$MajorCommandID,ifany=TRUE)
                levels(VAR.df$MajorCommandID)[is.na(levels(VAR.df$MajorCommandID))] <- "Uncategorized"
            }
        }
        
        VAR.df<-read_and_join(VAR.path,"Lookup_MajorCommandID.csv",VAR.df)
        NA.check.df<-subset(VAR.df,is.na(MajorCommandCode), select=c("MajorCommandID"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in MajorCommandCode"))
        }
        #     
        #     NA.check.df<-subset(VAR.df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        #     if(nrow(NA.check.df)>0){
        #       print(unique(NA.check.df))
        #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.detail"))
        #     }
        #     
        #     NA.check.df<-subset(VAR.df,is.na(SubCustomer.detail), select=c("Customer","SubCustomer"))
        #     if(nrow(NA.check.df)>0){
        #       print(unique(NA.check.df))
        #       stop(paste(nrow(NA.check.df),"rows of NAs generated in SubCustomer.component"))
        #     }
        #     
    }
    
    
    if("PlatformPortfolio" %in% names(VAR.df))
    {
        
        if("PlatformPortfolio.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(PlatformPortfolio.sum))
        }
        
        #Handle NA values if present
        if(any(is.na(VAR.df$PlatformPortfolio))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$PlatformPortfolio))){
                VAR.df$PlatformPortfolio<-addNA(VAR.df$PlatformPortfolio,ifany=TRUE)
                levels(VAR.df$PlatformPortfolio)[is.na(levels(VAR.df$PlatformPortfolio))] <- "Unlabeled"
            }
        }
        
        #           debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"Lookup_PlatformPortfolio.csv",VAR.df)
        NA.check.df<-subset(VAR.df, is.na(PlatformPortfolio.sum), select=c("PlatformPortfolio"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in PlatformPortfolio.sum"))
        }
    }
    
    
    
    if("Fair.Opportunity.Limited.Sources" %in% names(VAR.df)){
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Fair_Opportunity.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Fair.Competed) & !is.na(Fair.Opportunity.Limited.Sources), select=c("Fair.Opportunity.Limited.Sources","Fair.Competed"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Fair.Competed"))
        }
    }
    
    if("Arms.Type" %in% names(VAR.df)){
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_ArmsType.csv",VAR.df)
        
        
        NA.check.df<-subset(VAR.df, is.na(Arms.Summary), select=c("Arms.Type","Arms.Summary")
        )
        
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Arms.Type"))
        }
        
    }
    
    if("Country" %in% names(VAR.df)){
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Country.csv",VAR.df)
        
        
        NA.check.df<-subset(VAR.df, is.na(Country.Proper), select=c("Country","Country.Proper")
        )
        
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Countries"))
        }
        
    }
    else if("Destination.Country" %in% names(VAR.df)){
        #     debug(read_and_join)
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Country.csv",VAR.df)
        
        
        NA.check.df<-subset(VAR.df, is.na(Country.Proper), select=c("Destination.Country","Country.Proper")
        )
        
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Countries"))
        }
    }
#     browser()
    if("Pricing.Mechanism" %in% names(VAR.df)){ 
        VAR.df$Pricing.Mechanism[VAR.df$Pricing.Mechanism==""]<-NA
        
        #Handle NA values if present
        if(any(is.na(VAR.df$Pricing.Mechanism))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$Pricing.Mechanism))){
                VAR.df$Pricing.Mechanism<-addNA(VAR.df$Pricing.Mechanism,ifany=TRUE)
                levels(VAR.df$Pricing.Mechanism)[is.na(levels(VAR.df$Pricing.Mechanism))] <- "Unlabeled"
            }
        }
        
        
        
        if("Pricing.Mechanism.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.sum))
        }
        
        if("Pricing.Mechanism.detail" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.detail))
        }
        
        if("Pricing.Mechanism.Correlation" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.Correlation))
        }
        
        if("Pricing.Mechanism.Graph" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.Graph))
        }
        
        #     stop("hammertiime")
        
        
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Pricing_Mechanism.csv",VAR.df)
        
        
        NA.check.df<-subset(VAR.df, is.na(Pricing.Mechanism.sum), select=c("Pricing.Mechanism"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Pricing.Mechanism.detail), select=c("Pricing.Mechanism"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.detail"))
        }
    }  
    #   else if ("Pricing.Mechanism.Code" %in% names(VAR.df)){ 
    #     #Replace blank strings with Unlabeled
    # #     VAR.df$Pricing.Mechanism<-mapvalues(VAR.df$Pricing.Mechanism,from=c(""),to=c("Unlabeled"))
    # #     
    # #     #Handle NA values if present
    # #     if(any(is.na(VAR.df$Pricing.Mechanism))){
    # #       #Make sure unlabeled is within the list of levels
    # #       if (!("Unlabeled" %in% levels(VAR.df$Pricing.Mechanism))){
    # #         VAR.df$Pricing.Mechanism=factor(VAR.df$Pricing.Mechanism,levels=c(unique(VAR.df$Pricing.Mechanism),"Unlabeled"))
    # #       }
    # #     } 
    # #     
    # #     #Replace NAs with Uncategorized
    # #     VAR.df$Pricing.Mechanism[is.na(VAR.df$Pricing.Mechanism)]<-"Unlabeled"
    # #     
    # 
    #     
    #     if("IsCostBased" %in% names(VAR.df) && (!any(!is.na(VAR.df$IsCostBased)))){
    #       VAR.df<-subset(VAR.df, select=-c(IsCostBased))
    #     }
    #     
    #     if("IsFixedPrice" %in% names(VAR.df) && all(is.na(VAR.df$IsFixedPrice))){
    #       VAR.df<-subset(VAR.df, select=-c(IsFixedPrice))
    #     }
    #     
    #     
    #     if("IsIncentive" %in% names(VAR.df) && (!any(!is.na(VAR.df$IsIncentive)))){
    #       VAR.df<-subset(VAR.df, select=-c(IsIncentive))
    #     }
    #     
    #     if("Pricing.Mechanism.sum" %in% names(VAR.df)){
    #       VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.sum))
    #     }
    #     
    #     if("Pricing.Mechanism.detail" %in% names(VAR.df)){
    #       VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.detail))
    #     }
    #     
    #     if("Pricing.Mechanism.Correlation" %in% names(VAR.df)){
    #       VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.Correlation))
    #     }
    #     
    #     if("Pricing.Mechanism.Graph" %in% names(VAR.df)){
    #       VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.Graph))
    #     }
    #     
    #     #     stop("hammertiime")
    #     
    #     
    #     
    #     VAR.df<-read_and_join(VAR.path,"LOOKUP_Pricing_Mechanism.csv",VAR.df)
    #     
    #     
    #     NA.check.df<-subset(VAR.df, !is.na(Pricing.Mechanism.Code) & is.na(Pricing.Mechanism.sum), select=c("Pricing.Mechanism"))
    #     if(nrow(NA.check.df)>0){
    #       print(unique(NA.check.df))
    #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.sum"))
    #     }
    #     
    #     NA.check.df<-subset(VAR.df, is.na(Pricing.Mechanism.detail), select=c("Pricing.Mechanism"))
    #     if(nrow(NA.check.df)>0){
    #       print(unique(NA.check.df))
    #       stop(paste(nrow(NA.check.df),"rows of NAs generated in Pricing.Mechanism.detail"))
    #     }
    #   }
    
    
    if("Contractor.Size" %in% names(VAR.df)){
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Contractor_Size_named.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Contractor.Size.detail), select=c("Contractor.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.detail"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Contractor.Size.sum), select=c("Contractor.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Contractor.Size.correlation), select=c("Contractor.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contractor.Size.correlation"))
        }
        
    } 
    
    
    if("Vendor.Size" %in% names(VAR.df)){
        
        #Handle NAs    
        if(any(is.na(VAR.df$Vendor.Size))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$Vendor.Size))){
                VAR.df$Vendor.Size<-addNA(VAR.df$Vendor.Size,ifany=TRUE)
                levels(VAR.df$Vendor.Size)[is.na(levels(VAR.df$Vendor.Size))] <- "Unlabeled"
            }
        }
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Contractor_Size.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Vendor.Size.detail), select=c("Vendor.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Vendor.Size.detail"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Vendor.Size.sum), select=c("Vendor.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Vendor.Size.sum"))
        }
        
    } 
    
    
    if("Contract.Size" %in% names(VAR.df)){
        #Handle NA values if present
        if(any(is.na(VAR.df$Contract.Size))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$Contract.Size))){
                VAR.df$Contract.Size<-addNA(VAR.df$Contract.Size,ifany=TRUE)
                levels(VAR.df$Contract.Size)[is.na(levels(VAR.df$Contract.Size))] <- "Unlabeled"
            }
        }
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Contract_Size.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Contract.Size.detail), select=c("Contract.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contract.Size.detail"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Contract.Size.sum), select=c("Contract.Size"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Contract.Size.sum"))
        }
        
    } 
    
    
    if("Extent.Competed" %in% names(VAR.df)){
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Extent_Competed.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Extent.Competed.Sum), select=c("Extent.Competed.Sum"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Extent.Competed.Sum"))
        }
    }
    
    if("systemequipmentcode" %in% names(VAR.df)){
        if(any(is.na(VAR.df$systemequipmentcode))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$systemequipmentcode))){
                VAR.df$systemequipmentcode<-addNA(VAR.df$systemequipmentcode,ifany=TRUE)
                levels(VAR.df$systemequipmentcode)[is.na(levels(VAR.df$systemequipmentcode))] <- "Unlabeled"
            }
        }
        
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_systemequipmentcode.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(systemequipmentcode)|is.na(systemequipmentshorttext), select=c("systemequipmentcode","systemequipmentcodeText","systemequipmentshorttext"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in systemequipmentcodeText or systemequipmentshorttext"))
        }
    }
    
    
    
    if("Number.of.Offers.Received" %in% names(VAR.df)){
        offers.lookup<-data.frame(cbind(unique(VAR.df$Number.of.Offers.Received)))
        #     offers.lookup<-cbind(offers.lookup,label.offers2(offers.lookup$Number.of.Offers.Received))
        names(offers.lookup)<-c("Number.of.Offers.Received")
        offers.lookup<-ddply(offers.lookup, .(Number.of.Offers.Received), transform, Offers=label.offers2(min(Number.of.Offers.Received)))
        VAR.df<- join(
            VAR.df,
            offers.lookup,
            match="first"
        )
        
        #offers<-ddply(VAR.df, label.offers)
        #offers.lookup<-ddply(VAR.df, .(Number.of.Offers.Received), label.offers)
        
        NA.check.df<-subset(VAR.df, is.na(Offers), select=c("Offers"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Offers"))
        }
        
    }
    
    if(("Award.or.IDV.Type") %in% names(VAR.df)
       &&  ("IDV.Part.8.Or.Part.13" %in% names(VAR.df))
       && ("IDV.Multiple.Or.Single.Award.IDV"%in% names(VAR.df))
       && ("IDV.Type" %in% names(VAR.df)))
    {
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Vehicle_Classification.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Competed.Criteria), select=c("Award.or.IDV.Type","IDV.Part.8.Or.Part.13","IDV.Multiple.Or.Single.Award.IDV","IDV.Type","Competed.Criteria"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Competed.Criteria"))
        }
    }
    else if("Vehicle" %in% names(VAR.df))
    {
        
        
        #Handle NA values if present
        if(any(is.na(VAR.df$Vehicle))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$Vehicle))){
                VAR.df$Vehicle<-addNA(VAR.df$Vehicle,ifany=TRUE)
                levels(VAR.df$Vehicle)[is.na(levels(VAR.df$Vehicle))] <- "Unlabeled"
            }
        }
        
        VAR.df$Vehicle<-factor(toupper(as.character(VAR.df$Vehicle)))
        
        if("Vehicle.detail" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Vehicle.detail))
        }
        
        if("Vehicle.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Vehicle.sum))
        }
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Vehicle.csv",VAR.df)
        
        
        NA.check.df<-subset(VAR.df, is.na(Vehicle.sum) , select=c("Vehicle"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Vehicle.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Vehicle.detail), select=c("Vehicle"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Vehicle.detail"))
        }
    }
    
    if(("Competed.Criteria"%in% names(VAR.df))
       &&  ("Fair.Competed" %in% names(VAR.df))
       && ("Extent.Competed.Sum" %in% names(VAR.df))
       && ("Offers" %in% names(VAR.df)))
    {
        if("Competition.detail" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Competition.detail))
        }
        
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Competition_Classification.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Updated.Competition), select=c("Competed.Criteria","Fair.Competed","Extent.Competed.Sum","Offers","Updated.Competition"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Updated.Competition"))
        }
        NA.check.df<-subset(VAR.df, is.na(Original.Competition), select=c("Competed.Criteria","Fair.Competed","Extent.Competed.Sum","Offers","Original.Competition"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Original.Competition"))
        }
    }
    else if("CompetitionClassification" %in% names(VAR.df)
            && "ClassifyNumberOfOffers" %in% names(VAR.df)){
        
        if("Competition.sum" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Competition.sum))
        }
        
        if("Competition.detail" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Competition.detail))
        }
        
        if("Competion.Graph" %in% names(VAR.df)){
            VAR.df<-subset(VAR.df, select=-c(Competion.Graph))
        }
        
        #Handle NA values if present
        if(any(is.na(VAR.df$ClassifyNumberOfOffers))){
            #Make sure unlabeled is within the list of levels
            if (!("Unlabeled" %in% levels(VAR.df$ClassifyNumberOfOffers))){
                VAR.df$ClassifyNumberOfOffers<-addNA(VAR.df$ClassifyNumberOfOffers,ifany=TRUE)
                levels(VAR.df$ClassifyNumberOfOffers)[is.na(levels(VAR.df$ClassifyNumberOfOffers))] <- "Unlabeled"
            }
        }
        
        VAR.df<-read_and_join(VAR.path,"Lookup_SQL_CompetitionClassification.csv",VAR.df)
        
        
        
        NA.check.df<-subset(VAR.df, is.na(Competition.sum), select=c("CompetitionClassification","ClassifyNumberOfOffers"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Competition.sum"))
        }
        
        NA.check.df<-subset(VAR.df, is.na(Competition.detail), select=c("CompetitionClassification","ClassifyNumberOfOffers"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Competition.detail"))
        }
    }
    
    else if(("Extent.Competed.Sum" %in% names(VAR.df))
            && ("Offers" %in% names(VAR.df)))
    {
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Competition_Classification_woFairOpportunity.csv",VAR.df)
        
        NA.check.df<-subset(VAR.df, is.na(Original.Competition), select=c("Extent.Competed.Sum","Offers"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in Original.Competition"))
        }
    }
    
    VAR.df$Action.Obligation<-FactorToNumber(VAR.df$Action.Obligation)
    VAR.df$Actions<-FactorToNumber(VAR.df$Actions)
    
    if("Fiscal.Year"%in% names(VAR.df)){
        VAR.df<-read_and_join(VAR.path,"LOOKUP_Deflators.csv",VAR.df)  
        NA.check.df<-subset(VAR.df,  is.na(Deflator.2014) & is.na(Deflator.2013) & !is.na(Fiscal.Year), select=c("Fiscal.Year","Deflator.2013","Deflator.2014"))
        if(nrow(NA.check.df)>0){
            print(unique(NA.check.df))
            stop(paste(nrow(NA.check.df),"rows of NAs generated in value"))
        }
        
        
        if("Action.Obligation"%in% names(VAR.df)){
            VAR.df$Action.Obligation<-FactorToNumber(VAR.df$Action.Obligation)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$Obligation.2013<-VAR.df$Action.Obligation/VAR.df$Deflator.2013/1000000000
            }
            if("Deflator.2014"%in% names(VAR.df)){
                VAR.df$Obligation.2014<-VAR.df$Action.Obligation/VAR.df$Deflator.2014/1000000000
            }
        }
        
        
        
        if("GBKdisbursements"%in% names(VAR.df)){
            VAR.df$GBKdisbursements<-FactorToNumber(VAR.df$GBKdisbursements)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$GBKdisbursements.ConstantB<-VAR.df$GBKdisbursements/VAR.df$Deflator.2013/1000000000
            }
        }
        
        if("GBKobligations"%in% names(VAR.df)){
            VAR.df$SumOfObligations<-FactorToNumber(VAR.df$GBKobligations)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$GBKobligations.2013<-VAR.df$GBKobligations/VAR.df$Deflator.2013/1000000000
            }
        }
        
        if("Outlay"%in% names(VAR.df)){
            VAR.df$Outlay<-FactorToNumber(VAR.df$Outlay)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$Outlay.2013<-VAR.df$Outlay/VAR.df$Deflator.2013/1000000000
            }
        }
        
        
        if("OutlayNoOffsetAccount"%in% names(VAR.df)){
            VAR.df$OutlayNoOffsetAccount<-FactorToNumber(VAR.df$OutlayNoOffsetAccount)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$OutlayNoOffsetAccount.2013<-VAR.df$OutlayNoOffsetAccount/VAR.df$Deflator.2013/1000000000
            }
        }
        
        if("OutlayOffsetAccount"%in% names(VAR.df)){
            VAR.df$OutlayOffsetAccount<-FactorToNumber(VAR.df$OutlayOffsetAccount)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$OutlayOffsetAccount.2013<-VAR.df$OutlayOffsetAccount/VAR.df$Deflator.2013/1000000000
            }
        }
        
        
        
        if("Fed_Grant_Funding_Amount"%in% names(VAR.df)){
            VAR.df$Fed_Grant_Funding_Amount	   <-FactorToNumber(VAR.df$Fed_Grant_Funding_Amount	   )
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$Fed_Grant_Funding_Amount.2013<-VAR.df$Fed_Grant_Funding_Amount	   /VAR.df$Deflator.2013/1000000000
            }
        }
        
        
        if("ContractObligatedAmount"%in% names(VAR.df)){
            VAR.df$ContractObligatedAmount<-FactorToNumber(VAR.df$ContractObligatedAmount)
            if("Deflator.2013"%in% names(VAR.df)){
                VAR.df$ContractObligatedAmount.2013<-VAR.df$ContractObligatedAmount/VAR.df$Deflator.2013/1000000000
            }
        }
    }
    if("OutlayNoOffsetAccount.2013" %in% names(VAR.df)
       & "OutlayOffsetAccount.2013" %in% names(VAR.df)
       & "ContractObligatedAmount.2013" %in% names(VAR.df)
       #      & "Fed_Grant_Funding_Amount.2013" %in% names(VAR.df)
       & "Outlay.2013" %in% names(VAR.df)
    ){
        
        
        VAR.df$ResidualOutlay.2013<-ifelse(is.na(VAR.df$OutlayNoOffsetAccount.2013)
                                           ,0
                                           ,VAR.df$OutlayNoOffsetAccount.2013
        )
        
        VAR.df$ResidualOutlay.2013<-VAR.df$ResidualOutlay.2013-ifelse(is.na(VAR.df$ContractObligatedAmount.2013)
                                                                      ,0
                                                                      ,VAR.df$ContractObligatedAmount.2013
        )
        
        
        Measurement.Vars.List=c("OutlayNoOffsetAccount.2013"
                                ,"OutlayOffsetAccount.2013"
                                ,"ContractObligatedAmount.2013"
                                #                             ,"Fed_Grant_Funding_Amount.2013"
                                ,"Outlay.2013"
                                ,"ResidualOutlay.2013"
        )
        
        if("Fed_Grant_Funding_Amount.2013" %in% names(VAR.df))
        {
            
            VAR.df$ResidualOutlay.2013<-VAR.df$ResidualOutlay.2013-ifelse(is.na(VAR.df$Fed_Grant_Funding_Amount.2013)
                                                                          ,0
                                                                          ,VAR.df$Fed_Grant_Funding_Amount.2013
            )
            
            Measurement.Vars.List<-rbind(Measurement.Vars.List,"Fed_Grant_Funding_Amount.2013")
            
            
        }
        
        
        
        
        VAR.df<-melt(VAR.df, 
                     #                        id=c("Fiscal.Year"
                     #                                                  ,"SubFunder.Detail")
                     ,measure.vars=Measurement.Vars.List
                     ,variable.name="comparison.dollar.type")
        
        
        
        VAR.df<-read_and_join(
            Path
            ,"LOOKUP_comparison_dollar_type.csv"
            ,VAR.df
        )
        
    }
    
    if("SimpleArea" %in% names(VAR.df))
    {
        VAR.df$SimpleArea[VAR.df$SimpleArea==""]<-NA
        VAR.df$SimpleArea<-factor(VAR.df$SimpleArea
                                  ,exclude=NULL 
                                  ,levels = c("Products"
                                              ,"Services"
                                              ,"R&D"
                                              ,NA)
                                  ,labels = c("Products"
                                              ,"Services"
                                              ,"R&D"
                                              ,"Mixed or Unlabeled"),
        )
    }
    
    if("IsTerminated" %in% names(VAR.df))
    {
        
        VAR.df$IsTerminated<-factor(VAR.df$IsTerminated, 
                                    levels = c(0,1),
                                    labels = c("Unterminated", "Terminated")
        )
        
        
        
        
        #     addNA(VAR.df$IsTerminated, ifany = TRUE)
    }
    
    if("IsClosed" %in% names(VAR.df))
    {
        
        VAR.df$IsClosed<-factor(VAR.df$IsClosed, 
                                levels = c(0,1),
                                labels = c("Unspecified", "Closed")
        )
        #     addNA(VAR.df$IsClosed, ifany = TRUE)
    }
    if("numberofoffersreceived" %in% names(VAR.df)){
        VAR.df$numberofoffersreceived[VAR.df$numberofoffersreceived==0]<-NA
    }
    
    if("UnmodifiedNumberOfOffersReceived" %in% names(VAR.df))
    {
        VAR.df$UnmodifiedNumberOfOffersReceived<-FactorToNumber(VAR.df$UnmodifiedNumberOfOffersReceived)
        VAR.df$UnmodifiedNumberOfOffersReceived[VAR.df$UnmodifiedNumberOfOffersReceived==0]<-NA
        if("numberofoffersreceived" %in% names(VAR.df)){
            VAR.df$UnmodifiedNumberOfOffersReceived[is.na(VAR.df$UnmodifiedNumberOfOffersReceived)]<-VAR.df$numberofoffersreceived[is.na(VAR.df$UnmodifiedNumberOfOffersReceived)]
        }
        
        VAR.df$UnmodifiedNumberOfOffersSummary[is.na(VAR.df$UnmodifiedNumberOfOffersReceived)]<-NA
        VAR.df$UnmodifiedNumberOfOffersSummary[VAR.df$UnmodifiedNumberOfOffersReceived==1]<-1
        VAR.df$UnmodifiedNumberOfOffersSummary[VAR.df$UnmodifiedNumberOfOffersReceived==2]<-2
        VAR.df$UnmodifiedNumberOfOffersSummary[VAR.df$UnmodifiedNumberOfOffersReceived==3 | VAR.df$UnmodifiedNumberOfOffersReceived==4]<-3.5
        VAR.df$UnmodifiedNumberOfOffersSummary[VAR.df$UnmodifiedNumberOfOffersReceived>=5]<-5
        
        
        VAR.df$UnmodifiedNumberOfOffersSummary<-factor(VAR.df$UnmodifiedNumberOfOffersSummary, 
                                                       levels = c(NA,1,2,3.5,5),
                                                       labels = c("Unlabeled", "1\nOffer","2\noffers","3-4\noffers","5+\noffers"),
                                                       exclude=NULL
        )
        
        
    }
    
    
    
    if("MaxOfisChangeOrder" %in% names(VAR.df))
    {
        
        VAR.df$MaxOfisChangeOrder<-factor(VAR.df$MaxOfisChangeOrder, 
                                          levels = c(0,1),
                                          labels = c("No Change Order", "Change Order(s)")
        )
        addNA(VAR.df$MaxOfisChangeOrder, ifany = TRUE)
    }
    
    if("IsFixedPrice" %in% names(VAR.df))
    {
        
        VAR.df$IsFixedPrice<-factor(VAR.df$IsFixedPrice, 
                                    exclude=NULL,
                                    levels = c(1,0,NA),
                                    labels = c("Fixed Price", "Other","Combination \nor Unlabeled")
        )
        if (!("Combination \nor Unlabeled" %in% levels(VAR.df$IsFixedPrice))){
            VAR.df$IsFixedPrice<-addNA(VAR.df$IsFixedPrice,ifany=TRUE)
            levels(VAR.df$IsFixedPrice)[is.na(levels(VAR.df$IsFixedPrice))] <- "Combination \nor Unlabeled"
        }
    }
    
    
    if("Action.Obligation" %in% names(VAR.df)){
        VAR.df$LogOfAction.Obligation<-log10(VAR.df$Action.Obligation)
        VAR.df$LogOfAction.Obligation[is.infinite(VAR.df$LogOfAction.Obligation)]<-0
        
        if("NewWorkObligatedAmount" %in% names(VAR.df)){
            VAR.df$pNewWorkVsContractObligatedAmount<-VAR.df$NewWorkObligatedAmount/VAR.df$Action.Obligation
            VAR.df$pNewWorkVsContractObligatedAmount[is.infinite(VAR.df$pNewWorkVsContractObligatedAmount)]<-NA
            VAR.df$pNewWorkVsContractObligatedAmount[abs(VAR.df$pNewWorkVsContractObligatedAmount)>100]<-NA
        }
        if("ChangeOrderObligatedAmount" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsContractObligatedAmount<-VAR.df$ChangeOrderObligatedAmount/VAR.df$Action.Obligation
            VAR.df$pChangeOrderVsContractObligatedAmount[is.infinite(VAR.df$pChangeOrderVsContractObligatedAmount)]<-NA
            VAR.df$pChangeOrderVsContractObligatedAmount[abs(VAR.df$pChangeOrderVsContractObligatedAmount)>100]<-NA
        }
    }
    if("ContractBaseAndAllOptionsValue" %in% names(VAR.df)){
        VAR.df$ContractBaseAndAllOptionsValue<-FactorToNumber(VAR.df$ContractBaseAndAllOptionsValue)
        VAR.df$LogOfContractBaseAndAllOptionsValue<-log10(VAR.df$ContractBaseAndAllOptionsValue)
        VAR.df$LogOfContractBaseAndAllOptionsValue[is.infinite(VAR.df$LogOfContractBaseAndAllOptionsValue)]<-0
        
        if("NewWorkBaseAndAllOptionsValue" %in% names(VAR.df)){
            VAR.df$pNewWorkVsContractBaseAndAllOptionsValue<-VAR.df$NewWorkBaseAndAllOptionsValue/VAR.df$ContractBaseAndAllOptionsValue
            VAR.df$pNewWorkVsContractBaseAndAllOptionsValue[is.infinite(VAR.df$pNewWorkVsContractBaseAndAllOptionsValue)]<-NA
            VAR.df$pNewWorkVsContractBaseAndAllOptionsValue[abs(VAR.df$pNewWorkVsContractBaseAndAllOptionsValue)>100]<-NA
        }
        if("ChangeOrderBaseAndAllOptionsValue" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsContractBaseAndAllOptionsValue<-VAR.df$ChangeOrderBaseAndAllOptionsValue/VAR.df$ContractBaseAndAllOptionsValue
            VAR.df$pChangeOrderVsContractBaseAndAllOptionsValue[is.infinite(VAR.df$pChangeOrderVsContractBaseAndAllOptionsValue)]<-NA
            VAR.df$pChangeOrderVsContractBaseAndAllOptionsValue[abs(VAR.df$pChangeOrderVsContractBaseAndAllOptionsValue)>100]<-NA
        }
    }
    if("ContractBaseAndExercisedOptionsValue" %in% names(VAR.df)){
        VAR.df$ContractBaseAndExercisedOptionsValue<-FactorToNumber(VAR.df$ContractBaseAndExercisedOptionsValue)
        VAR.df$LogOfContractBaseAndExercisedOptionsValue<-log10(VAR.df$ContractBaseAndExercisedOptionsValue)
        VAR.df$LogOfContractBaseAndExercisedOptionsValue[is.infinite(VAR.df$LogOfContractBaseAndExercisedOptionsValue)]<-0
        
        if("NewWorkBaseAndExercisedOptionsValue" %in% names(VAR.df)){
            VAR.df$pNewWorkVsContractBaseAndExercised<-VAR.df$NewWorkBaseAndExercisedOptionsValue/VAR.df$ContractBaseAndExercisedOptionsValue
            VAR.df$pNewWorkVsContractBaseAndExercised[is.infinite(VAR.df$pNewWorkVsContractBaseAndExercised)]<-NA
            VAR.df$pNewWorkVsContractBaseAndExercised[abs(VAR.df$pNewWorkVsContractBaseAndExercised)>100]<-NA
        }
        if("ChangeOrderBaseAndExercisedOptionsValue" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsContractBaseAndExercised<-VAR.df$ChangeOrderBaseAndExercisedOptionsValue/VAR.df$ContractBaseAndExercisedOptionsValue
            VAR.df$pChangeOrderVsContractBaseAndExercised[is.infinite(VAR.df$pChangeOrderVsContractBaseAndExercised)]<-NA
            VAR.df$pChangeOrderVsContractBaseAndExercised[abs(VAR.df$pChangeOrderVsContractBaseAndExercised)>100]<-NA
        }
    }
    if("UnmodifiedContractObligatedAmount" %in% names(VAR.df)){
        VAR.df$UnmodifiedContractObligatedAmount<-FactorToNumber(VAR.df$UnmodifiedContractObligatedAmount)
        VAR.df$LogOfUnmodifiedContractObligatedAmount<-log10(VAR.df$UnmodifiedContractObligatedAmount)
        VAR.df$LogOfUnmodifiedContractObligatedAmount[is.infinite(VAR.df$LogOfUnmodifiedContractObligatedAmount)]<-0
        if("Action.Obligation" %in% names(VAR.df)){
            VAR.df$pUnmodifiedContractObligated<-VAR.df$UnmodifiedContractObligatedAmount/VAR.df$Action.Obligation
        }    
        if("NewWorkObligatedAmount" %in% names(VAR.df)){
            VAR.df$pNewWorkVsUnmodifiedObligatedAmount<-VAR.df$NewWorkObligatedAmount/VAR.df$UnmodifiedContractObligatedAmount
            VAR.df$pNewWorkVsUnmodifiedObligatedAmount[is.infinite(VAR.df$pNewWorkVsUnmodifiedObligatedAmount)]<-NA
            VAR.df$pNewWorkVsUnmodifiedObligatedAmount[abs(VAR.df$pNewWorkVsUnmodifiedObligatedAmount)>100]<-NA
        }
        if("ChangeOrderObligatedAmount" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsUnmodifiedObligatedAmount<-VAR.df$ChangeOrderObligatedAmount/VAR.df$UnmodifiedContractObligatedAmount
            VAR.df$pChangeOrderVsUnmodifiedObligatedAmount[is.infinite(VAR.df$pChangeOrderVsUnmodifiedObligatedAmount)]<-NA
            VAR.df$pChangeOrderVsUnmodifiedObligatedAmount[abs(VAR.df$pChangeOrderVsUnmodifiedObligatedAmount)>100]<-NA
        }
    }
    if("UnmodifiedContractBaseAndAllOptionsValue" %in% names(VAR.df)){
        VAR.df$UnmodifiedContractBaseAndAllOptionsValue<-FactorToNumber(VAR.df$UnmodifiedContractBaseAndAllOptionsValue)
        VAR.df$LogOfUnmodifiedContractBaseAndAllOptionsValue<-log10(VAR.df$UnmodifiedContractBaseAndAllOptionsValue)
        VAR.df$LogOfUnmodifiedContractBaseAndAllOptionsValue[is.infinite(VAR.df$LogOfUnmodifiedContractBaseAndAllOptionsValue)]<-0
        
        VAR.df$SizeOfUnmodifiedContractBaseAndAll<-CreateSize(VAR.df$UnmodifiedContractBaseAndAllOptionsValue)
        
        if("ContractBaseAndAllOptionsValue" %in% names(VAR.df)){
            VAR.df$pUnmodifiedContractBaseAndAll<-VAR.df$UnmodifiedContractBaseAndAllOptionsValue/VAR.df$ContractBaseAndAllOptionsValue
        }
        if("NewWorkBaseAndAllOptionsValue" %in% names(VAR.df)){
            VAR.df$pNewWorkVsUnmodifiedBaseAndAll<-VAR.df$NewWorkBaseAndAllOptionsValue/VAR.df$UnmodifiedContractBaseAndAllOptionsValue
            VAR.df$pNewWorkVsUnmodifiedBaseAndAll[is.infinite(VAR.df$pNewWorkVsUnmodifiedBaseAndAll)]<-NA
            VAR.df$pNewWorkVsUnmodifiedBaseAndAll[abs(VAR.df$pNewWorkVsUnmodifiedBaseAndAll)>100]<-NA
        }
        if("ChangeOrderBaseAndAllOptionsValue" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsUnmodifiedBaseAndAll<-VAR.df$ChangeOrderBaseAndAllOptionsValue/VAR.df$UnmodifiedContractBaseAndAllOptionsValue
            VAR.df$pChangeOrderVsUnmodifiedBaseAndAll[is.infinite(VAR.df$pChangeOrderVsUnmodifiedBaseAndAll)]<-NA
            VAR.df$pChangeOrderVsUnmodifiedBaseAndAll[abs(VAR.df$pChangeOrderVsUnmodifiedBaseAndAll)>100]<-NA
        }
    }
    if("UnmodifiedContractBaseAndExercisedOptionsValue" %in% names(VAR.df)){
        VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue<-FactorToNumber(VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue)
        VAR.df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue<-log10(VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue)
        VAR.df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue[is.infinite(VAR.df$LogOfUnmodifiedContractBaseAndExercisedOptionsValue)]<-0
        if("ContractBaseAndExercisedOptionsValue" %in% names(VAR.df)){
            VAR.df$pUnmodifiedContractBaseAndExercised<-VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue/VAR.df$ContractBaseAndExercisedOptionsValue
        }
        if("NewWorkBaseAndExercisedOptionsValue" %in% names(VAR.df)){
            VAR.df$pNewWorkVsUnmodifiedBaseAndExercised<-VAR.df$NewWorkBaseAndExercisedOptionsValue/VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue
            VAR.df$pNewWorkVsUnmodifiedBaseAndExercised[is.infinite(VAR.df$pNewWorkVsUnmodifiedBaseAndExercised)]<-NA
            VAR.df$pNewWorkVsUnmodifiedBaseAndExercised[abs(VAR.df$pNewWorkVsUnmodifiedBaseAndExercised)>100]<-NA
        }
        if("ChangeOrderBaseAndExercisedOptionsValue" %in% names(VAR.df)){
            VAR.df$pChangeOrderVsUnmodifiedBaseAndExercised<-VAR.df$ChangeOrderBaseAndExercisedOptionsValue/VAR.df$UnmodifiedContractBaseAndExercisedOptionsValue
            VAR.df$pChangeOrderVsUnmodifiedBaseAndExercised[is.infinite(VAR.df$pChangeOrderVsUnmodifiedBaseAndExercised)]<-NA
            VAR.df$pChangeOrderVsUnmodifiedBaseAndExercised[abs(VAR.df$pChangeOrderVsUnmodifiedBaseAndExercised)>100]<-NA
        }
    }
    
    #   
    # ChangeOrderObligatedAmount
    # ChangeOrderBaseAndExercisedOptionsValue	
    # ChangeOrderBaseAndAllOptionsValue
    # NewWorkObligatedAmount
    # NewWorkBaseAndExercisedOptionsValue
    # NewWorkBaseAndAllOptionsValue
    
    
    
    if("Fiscal.Year"%in% names(VAR.df)){
        VAR.df$Fiscal.Year <-as.Date(paste("9/30/",as.character(VAR.df$Fiscal.Year),sep=""),"%m/%d/%Y")
    }
    
    if("Date.Signed"%in% names(VAR.df)){
        
            if(max(nchar(as.character(VAR.df$Date.Signed)))==10){
#         if((max(substring(as.character(VAR.df$Date.Signed),7,8))=="99" | 
#                max(substring(as.character(VAR.df$Date.Signed),7,8))<"20") &
#                !max(substring(as.character(VAR.df$Date.Signed),1,2))>"12"){
            VAR.df$Date.Signed <-as.Date(as.character(VAR.df$Date.Signed),"%m/%d/%Y")
        }
        else{
            VAR.df$Date.Signed <-as.Date(as.character(VAR.df$Date.Signed),"%y/%m/%d")
        }
}

if("SignedMonth"%in% names(VAR.df)){
    VAR.df$SignedMonth <-as.Date(as.character(VAR.df$SignedMonth),"%Y-%m-%d")
}


if("YEAR"%in% names(VAR.df)){
    VAR.df$YEAR <-as.Date(paste("12/31/",as.character(VAR.df$YEAR),sep=""),"%m/%d/%Y")
}

if(!("Graph" %in% names(VAR.df))){
    VAR.df$Graph<-TRUE
}
if("ProductOrServicesCategory.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$ProductOrServicesCategory.Graph
    VAR.df<-subset(VAR.df, select=-c(ProductOrServicesCategory.Graph)) 
}
if("Contract.Size.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$Contract.Size.Graph
    VAR.df<-subset(VAR.df, select=-c(Contract.Size.Graph)) 
}

if("Competition.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$Competition.Graph
    VAR.df<-subset(VAR.df, select=-c(Competition.Graph)) 
}
if("Vehicle.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$Vehicle.Graph
    VAR.df<-subset(VAR.df, select=-c(Vehicle.Graph)) 
}
if("Pricing.Mechanism.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$Pricing.Mechanism.Graph
    VAR.df<-subset(VAR.df, select=-c(Pricing.Mechanism.Graph)) 
}
if("Customer.Graph"%in% names(VAR.df)){
    VAR.df$Graph<-VAR.df$Graph&VAR.df$Customer.Graph
    VAR.df<-subset(VAR.df, select=-c(Customer.Graph)) 
}        
if("LastCurrentCompletionDate"%in% names(VAR.df)&"MinOfEffectiveDate"%in% names(VAR.df)){
    
    VAR.df$CurrentMonths<-as.numeric(difftime(strptime(VAR.df$LastCurrentCompletionDate,"%Y-%m-%d")
                                              , strptime(VAR.df$MinOfEffectiveDate,"%Y-%m-%d")
                                              , unit="weeks"
    ))
    VAR.df$CategoryOfCurrentMonths<-CreateDuration(VAR.df$CurrentMonths)
    VAR.df$CurrentMonths<-ceiling(VAR.df$CurrentMonths/4)
}


if("UnmodifiedCurrentCompletionDate"%in% names(VAR.df)&"MinOfEffectiveDate"%in% names(VAR.df)){
    
    VAR.df$UnmodifiedMonths<-as.numeric(difftime(strptime(VAR.df$UnmodifiedCurrentCompletionDate,"%Y-%m-%d")
                                                 , strptime(VAR.df$MinOfEffectiveDate,"%Y-%m-%d")
                                                 , unit="weeks"
    ))
    VAR.df$CategoryOfUnmodifiedMonths<-CreateDuration(VAR.df$UnmodifiedMonths)
    VAR.df$UnmodifiedMonths<-ceiling(VAR.df$UnmodifiedMonths/4)
    
}

if("UnmodifiedIsSomeCompetition" %in% names(VAR.df))
{
    VAR.df$UnmodifiedIsSomeCompetition<-factor(VAR.df$UnmodifiedIsSomeCompetition, 
                                               exclude=NULL,
                                               levels = c(1,0,NA),
                                               labels = c("Comp.", "No Comp.","Unlabeled")
    )
}

if("IsSomeCompetition" %in% names(VAR.df))
{
    
    
    
    if ("IsFullAndOpen" %in% names(VAR.df)&
            "IsOnlyOneSource" %in% names(VAR.df)){
        VAR.df$UnmodifiedCompetition[VAR.df$IsFullAndOpen==1]<-1
        VAR.df$UnmodifiedCompetition[VAR.df$IsSomeCompetition==1
                                     &is.na(VAR.df$UnmodifiedCompetition)]<-2
        VAR.df$UnmodifiedCompetition[VAR.df$IsOnlyOneSource==1
                                     &is.na(VAR.df$UnmodifiedCompetition)]<-3
        VAR.df$UnmodifiedCompetition[VAR.df$IsOnlyOneSource==0
                                     &is.na(VAR.df$UnmodifiedCompetition)]<-4
        VAR.df$UnmodifiedCompetition<-factor(VAR.df$UnmodifiedCompetition
                                             ,exclude=NULL
                                             ,levels=c(1,2,3,4,NA)
                                             ,labels=c("Full and Open"
                                                       ,"Some Comp."
                                                       ,"No Comp.\n1 Source"
                                                       ,"No Comp.\nOther"
                                                       ,"Unlabeled"
                                             )
        )
        
    }
    VAR.df$IsSomeCompetition<-factor(VAR.df$IsSomeCompetition, 
                                     exclude=NULL,
                                     levels = c(1,0,NA),
                                     labels = c("Comp.", "No Comp.","Mixed or \nUnlabeled")
    )
    
    if("IsFullAndOpen" %in% names(VAR.df))
    {
        VAR.df$IsFullAndOpen<-factor(VAR.df$IsFullAndOpen, 
                                     exclude=NULL,
                                     levels = c(1,0,NA),
                                     labels = c("Full & Open", "Not Full \n& Open","Mixed or \nUnlabeled")
        )
    }
    
    
    
    if("UnmodifiedIsFullAndOpen" %in% names(VAR.df))
    {
        VAR.df$UnmodifiedIsFullAndOpen<-factor(VAR.df$UnmodifiedIsFullAndOpen, 
                                               exclude=NULL,
                                               levels = c(1,0,NA),
                                               labels = c("Full & Open", "Not Full \n& Open","Unlabeled")
        )
    }
    if("IsOnlyOneSource" %in% names(VAR.df))
    {
        VAR.df$IsOnlyOneSource<-factor(VAR.df$IsOnlyOneSource, 
                                       exclude=NULL,
                                       levels = c(1,0,NA),
                                       labels = c("Only One Source", "Not Only Once Source","Unlabeled")
        )
    }
    
    
    
    
    if ("IsIDV" %in% names(VAR.df)&
            "multipleorsingleawardidc" %in% names(VAR.df)&
            "AwardOrIDVcontractactiontype" %in% names(VAR.df)
    ){
        VAR.df$UnmodifiedVehicle[is.na(VAR.df$IsIDV)]<-NA
        VAR.df$UnmodifiedVehicle[VAR.df$AwardOrIDVcontractactiontype %in% c("Definitive Contract")
                                 &is.na(VAR.df$UnmodifiedVehicle)]<-1
        VAR.df$UnmodifiedVehicle[VAR.df$AwardOrIDVcontractactiontype %in% c("Purchase Order")
                                 &is.na(VAR.df$UnmodifiedVehicle)]<-2
        VAR.df$UnmodifiedVehicle[VAR.df$AwardOrIDVcontractactiontype %in% c("Blanket Purchase Agreement"
                                                                            ,"Federal Supply Schedule"
                                                                            ,"Government Wide Acquisition Contract"
                                                                            ,"Basic Ordering Agreement")
                                 &is.na(VAR.df$UnmodifiedVehicle)]<-5
        
        VAR.df$UnmodifiedVehicle[VAR.df$multipleorsingleawardidc=="MULTIPLE AWARD"
                                 &is.na(VAR.df$UnmodifiedVehicle)]<-4
        VAR.df$UnmodifiedVehicle[VAR.df$multipleorsingleawardidc=="SINGLE AWARD"
                                 &is.na(VAR.df$UnmodifiedVehicle)]<-3
        VAR.df$UnmodifiedVehicle[is.na(VAR.df$UnmodifiedVehicle)]<-6
        VAR.df$UnmodifiedVehicle<-factor(VAR.df$UnmodifiedVehicle
                                         ,exclude=NULL
                                         ,levels=c(1
                                                   ,2
                                                   ,3
                                                   ,4
                                                   ,5
                                                   ,6
                                                   ,NA)
                                         ,labels=c("Definitive"
                                                   ,"Purchase\nOrder"
                                                   ,"Single-Award\nIDC"
                                                   ,"Multi-Award\nIDC"
                                                   ,"Other IDC"
                                                   ,"Unlabeled\nIDC"
                                                   ,"Unlabeled"
                                         )
        )
        
    }
    
    
}



VAR.df
}
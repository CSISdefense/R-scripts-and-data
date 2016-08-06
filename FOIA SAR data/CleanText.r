
#*************************************Required Libraries******************************************
# require(plyr)
# require(grid)
# require(reshape2)
# require(stringr)
# require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
# require(gdata)
require(XLConnect)


#*************************************Functions******************************************

CleanExtractAndWrite<-function(df,
                               HeaderList,
                               sDirectory,
                               FilePrefix,
                               Increment){
    #Check if there is one name row. If there's multiple names or only a single name, this does not work
    name.df<-df[!is.na(df$Type) & df$Type=="Name",]
    if(any(!is.na(name.df$Header))&
       (min(as.character(name.df$Header),na.rm=TRUE)==max(as.character(name.df$Header),na.rm=TRUE))){
        df$Section<-min(as.character(name.df$Header),na.rm=TRUE)
        df<-df[!df$Type %in% c("Name"),]
    }
    sUnit<-NA
    unit.df<-df[!is.na(df$Type) & df$Type=="Unit",]
    if(any(!is.na(unit.df$Header))&
       (min(as.character(unit.df$Header),na.rm=TRUE)==max(as.character(unit.df$Header),na.rm=TRUE))){
        sUnit<-min(as.character(unit.df$Header),na.rm=TRUE)
        df$Unit<-sUnit
        df<-df[!df$Type %in% c("Unit"),]
    }
    
    sSection<-NA
    if ("Section" %in% colnames(df)){
        sSection<-min(df$Section)
    }
    
    #Determine the source, checking if the file has an override
    sSource<-NA
    OverrideSource.df<-df[!is.na(df$Type) & df$Type=="OverrideSource",]
    if(any(!is.na(OverrideSource.df$Header))&
       (min(as.character(OverrideSource.df$Header),na.rm=TRUE)==max(as.character(OverrideSource.df$Header),na.rm=TRUE))){
        sSource<-min(as.character(OverrideSource.df$Header),na.rm=TRUE)
        df$Source<-sSource
        df<-df[!df$Type %in% c("OverrideSource"),]
    }
    else if(any(!is.na(df$Source))&
       (min(as.character(df$Source),na.rm=TRUE)==max(as.character(df$Source),na.rm=TRUE))){
        sSource<-min(df$Source)

    }

    sPlatform<-NA
    if(any(!is.na(df$Platform))&
       (min(as.character(df$Platform),na.rm=TRUE)==max(as.character(df$Platform),na.rm=TRUE))){
        sPlatform<-min(df$Platform)
    }

    sHeader<-NA
    # Label and then remove columns that are completely blank
    # in this extraction. E.g. some extractions are 8 columns, some are 4.
    df<-df[!df$Type %in% c("Type"),]
    if(any(!is.na(df$Header))&
       (min(as.character(df$Header),na.rm=TRUE)==max(as.character(df$Header),na.rm=TRUE))){
        sHeader<-max(as.character(df$Header),na.rm=TRUE)
        HeaderList<-subset(HeaderList,Header==sHeader,select=-c(Header,Type))
        colnames(df)[1:ncol(HeaderList)]<-HeaderList[1,]
        df<-df[!df$Type %in% c("Header"),]
        
    }
    BlankCols<-colSums(!is.na(df)) == 0
    df<-df[,!BlankCols]
    
    ifelse(!file.exists(file.path(sDirectory, "Processing")), 
           dir.create(file.path(sDirectory, "Processing")), FALSE)
    
    #Project and Source are guaranteed to be in 
    if(ncol(df)>2){
        write.csv(df,file.path(sDirectory,"Processing",
                           gsub(" ","_",paste( ifelse(!is.na(sPlatform),paste(sPlatform,"_",sep=""),""),
                                               ifelse(!is.na(FilePrefix),paste(FilePrefix,"_",sep=""),""),
                                               ifelse(!is.na(sSource),paste(sSource,"_",sep=""),""),
                                               Increment,
                                               ifelse(!is.na(sHeader),paste("_",sHeader,sep=""),""),
                                               ifelse(!is.na(sUnit),paste("_",sUnit,sep=""),""),
                                               ifelse(!is.na(sSection),paste("_",sSection,sep=""),""),
                                               ".csv",sep=""))),
                  row.names=FALSE)
        df
    }
}



SplitAtBlankRow<-function(df,
                          FilePrefix="",
                          Increment=1,
                          sDirectory="",
                          HeaderName="",
                          HeaderList,
                          Section=NA){
    
    Header<-NA
    if(nrow(df)>1){
        df[df==""|df==" "]<-NA
        BlankRows<-rowSums(!is.na(df[,!colnames(df) %in% c("Source","Platform","Section")])) == 0
        
        #First, get rid of blank rows at the start
        if(any(BlankRows)){
            FirstFilledLine<-min(which(BlankRows == FALSE))
            if(FirstFilledLine>1){
                df<-df[FirstFilledLine:nrow(df),]
                BlankRows<-rowSums(!is.na(df[,!colnames(df) %in% c("Source","Platform","Section")])) == 0
            }
        }
        
        if(!is.na(Section)){
            df$Section<-Section
        }
        
        #Second, split up if there are remaining blank rows
        if(any(BlankRows)){
            #Remove any blank lines at the start of the extract
            
            NextBlankLine<-min(which(BlankRows == TRUE))
            FirstExtract<-df[1:(NextBlankLine-1),]
            FirstExtract<-CleanExtractAndWrite(FirstExtract,
                                               HeaderList,
                                               sDirectory,
                                               FilePrefix,
                                               Increment
            )
            
            #Recursing
            Section<-NA
            if ("Section" %in% colnames(FirstExtract)){
                Section<-min(FirstExtract$Section)
            }
            
            SplitAtBlankRow(df[(NextBlankLine+1):length(BlankRows),],
                            FilePrefix=FilePrefix,
                            Increment=Increment+1,
                            sDirectory=sDirectory,
                            HeaderList=HeaderList,
                            Section=Section)
        }
        else{
            # Label and then remove columns that are completely blank
            # in this extraction. E.g. some extractions are 8 columns, some are 4.
            df<-CleanExtractAndWrite(df,
                                     HeaderList,
                                     sDirectory,
                                     FilePrefix,
                                     Increment)
        }
    }
    else if(length(df)>0){
        # Label and then remove columns that are completely blank
        # in this extraction. E.g. some extractions are 8 columns, some are 4.
        df<-CleanExtractAndWrite(df,
                                 HeaderList,
                                 sDirectory,
                                 FilePrefix,
                                 Increment)
    }
}

ReadAndSplit<-function(sFileName,Platform=NA,Source=NA,sSheetName="",sDirectory=""){
    lookup.RawHeaderList<-read.csv("RawHeaderList.csv",
                                   na.strings=c("NA",""),
                                   stringsAsFactors = FALSE
    )
    lookup.RawHeaderCleaned<-read.csv("CleanedHeaderList.csv",
                                      na.strings=c("NA",""),
                                      stringsAsFactors = FALSE)
    wb<-loadWorkbook(file.path(sDirectory,sFileName), create = TRUE)
    df<-readWorksheet(wb, 
                      sheet = sSheetName,
                      header = FALSE)
    #If there's more columns in the header lookup, limit the lookup
    #to only those cases where there is no material in the unused columns
    if(ncol(df)+1==(ncol(lookup.RawHeaderList)-2)){#One Column
        BlankRows<-is.na(lookup.RawHeaderList[,ncol(df)+1])
        lookup.RawHeaderList<-lookup.RawHeaderList[BlankRows,]
    }
    else if(ncol(df)<ncol(lookup.RawHeaderList)-2){#MultipleColumns
        MaterialInUnusuedColumns<-subset(lookup.RawHeaderList,select=-c(Type,Header))
        MaterialInUnusuedColumns<-MaterialInUnusuedColumns[,
                                                           (ncol(df)+1):ncol(MaterialInUnusuedColumns)]
        BlankRows<-rowSums(!is.na(MaterialInUnusuedColumns)) == 0
        lookup.RawHeaderList<-lookup.RawHeaderList[BlankRows,]
    }
    
    
    df<- join(
        df, 
        lookup.RawHeaderList,
        match="first"
    )
    if(!is.na(Platform)){
        df$Platform<-Platform
    }
    if(!is.na(Platform)){
        df$Source<-Source
    }
    #Remove type names, as we figure that out based on the headers.
    df<-subset(df,Type!="Type" | is.na(Type))
    SplitAtBlankRow(df,
                    NA,
                    sDirectory=sDirectory,
                    HeaderList=lookup.RawHeaderCleaned
    )
}

#Source: http://stackoverflow.com/questions/2104483/how-to-read-table-multiple-files-into-a-single-table-in-r
read.tables <- function(file.names, ...) {
    require(plyr)
    ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)))
}



BulkImport<-function(Path,Header){
    WD<-getwd()
    setwd(file.path(WD,Path))
    data.files<-file.path(list.files(pattern=paste(".*"
                                                 ,Header
                                                 ,".*[.]csv"
                                                 ,sep=""
                                  )
                      )
    )
    
    results<-read.tables(data.files,
                header=TRUE, sep=",", na.strings=c("NA",
                                                   "--",
                                                   "-- ",
                                                   "N/A"), dec=".", strip.white=TRUE, 
                stringsAsFactors=FALSE
    )
    setwd(WD)
    results
}

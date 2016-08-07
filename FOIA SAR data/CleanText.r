
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
                               FilePrefix=NA,
                               Increment=NA){
  #Check if there is only one name present in the section column, 
  #If yes, name the section, which can also influence future files in the sheet.
  #If missing or there are contradicting names, do nothing and move on
  if(any(!is.na(df$OverrideSection))&
     (min(as.character(df$OverrideSection),na.rm=TRUE)==max(as.character(df$OverrideSection),na.rm=TRUE))){
    df$Section<-min(as.character(df$OverrideSection),na.rm=TRUE)
  }
  
  sSection<-NA
  if ("Section" %in% colnames(df)){
    sSection<-min(df$Section)
  }
  
  #Check if it lists a unit type (as in then-year vs. base year, currency, etc.)
  sUnit<-NA
  if(any(!is.na(df$Unit))&
     (min(as.character(df$Unit),na.rm=TRUE)==
      max(as.character(df$Unit),na.rm=TRUE))){
    sUnit<-min(as.character(df$Unit),na.rm=TRUE)
    df$Unit<-sUnit
  }
  
  
  #Determine the source, checking if the file has an override
  sSource<-NA
  if(any(!is.na(df$OverrideSource))&
     (min(as.character(df$OverrideSource),na.rm=TRUE)==
      max(as.character(df$OverrideSource),na.rm=TRUE))){
    sSource<-min(as.character(df$OverrideSource),na.rm=TRUE)
    df$Source<-sSource
  }
  else if(any(!is.na(df$Source))&
          (min(as.character(df$Source),na.rm=TRUE)==
           max(as.character(df$Source),na.rm=TRUE))){
    sSource<-min(df$Source)
    
  }
  
  sPlatform<-NA
  if(any(!is.na(df$Platform))&
     (min(as.character(df$Platform),na.rm=TRUE)==
      max(as.character(df$Platform),na.rm=TRUE))){
    sPlatform<-min(df$Platform)
  }
  
  
  sHeader<-NA
  # Label and then remove columns that are completely blank
  # in this extraction. E.g. some extractions are 8 columns, some are 4.
  if(any(!is.na(df$Header))&
     (min(as.character(df$Header),na.rm=TRUE)==max(as.character(df$Header),na.rm=TRUE))){
    sHeader<-max(as.character(df$Header),na.rm=TRUE)
    HeaderList<-subset(HeaderList,Header==sHeader,select=-c(Header))
    colnames(df)[1:ncol(HeaderList)]<-HeaderList[1,]
    df<-subset(df,is.na(Remove)|df$Remove==FALSE,select=-c(Header,Remove))
  }
  BlankCols<-colSums(!is.na(df)) == 0
  df<-df[,!BlankCols]
  
  
  ifelse(!file.exists(file.path(sDirectory, "Processing")), 
         dir.create(file.path(sDirectory, "Processing")), FALSE)
  
  #Project and Source are guaranteed to be in 
  if(ncol(df)>2){
    write.csv(df,file.path(sDirectory,"Processing",
                           gsub("_[.]csv",".csv",
                                gsub(" ","_",paste( ifelse(!is.na(sPlatform),paste(sPlatform,"_",sep=""),""),
                                                    ifelse(!is.na(FilePrefix),paste(FilePrefix,"_",sep=""),""),
                                                    ifelse(!is.na(sSource),paste(sSource,"_",sep=""),""),
                                                    ifelse(!is.na(Increment),paste(Increment,"_",sep=""),""),
                                                    ifelse(!is.na(sHeader),paste(sHeader,"_",sep=""),""),
                                                    ifelse(!is.na(sUnit),paste(sUnit,"_",sep=""),""),
                                                    ifelse(!is.na(sSection),paste(sSection,"_",sep=""),""),
                                                    ".csv",sep="")))),
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
                          Section=NA,
                          Source=NA){
    
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
        
        
        if(!is.na(Source)){
          df$Source<-Source
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
            
            #Check if there's a filled in section to pass on to the next block
            Section<-NA
            if ("Section" %in% colnames(FirstExtract)){
                Section<-min(FirstExtract$Section)
            }
            #Check if there's a filled in source to pass on to the next block
            if ("Source" %in% colnames(FirstExtract)){
              Source<-min(FirstExtract$Source)
            }
            
            SplitAtBlankRow(df[(NextBlankLine+1):length(BlankRows),],
                            FilePrefix=FilePrefix,
                            Increment=Increment+1,
                            sDirectory=sDirectory,
                            HeaderList=HeaderList,
                            Section=Section,
                            Source=Source)
        }
        else{
            # Label and then remove columns that are completely blank
            # in this extraction. E.g. some extractions are 8 columns, some are 4.
            df<-CleanExtractAndWrite(df,
                                     HeaderList,
                                     sDirectory,
                                     FilePrefix,
                                     ifelse(Increment==1,NA,Increment))#If only one, we don't need to number it.
        }
    }
    else if(length(df)>0){
        # Label and then remove columns that are completely blank
        # in this extraction. E.g. some extractions are 8 columns, some are 4.
        df<-CleanExtractAndWrite(df,
                                 HeaderList,
                                 sDirectory,
                                 FilePrefix,
                                 ifelse(Increment==1,NA,Increment))#If only one, we don't need to number it.
    }
}

ReadAndSplit<-function(sFileName,
                       Platform=NA,
                       Source=NA,
                       sSheetName=NA,
                       sDirectory=""){
    lookup.RawHeaderList<-read.csv("RawHeaderList.csv",
                                   na.strings=c("NA",""),
                                   stringsAsFactors = FALSE
    )
    lookup.CleanHeaderList<-read.csv("CleanedHeaderList.csv",
                                      na.strings=c("NA",""),
                                      stringsAsFactors = FALSE)
    wb<-loadWorkbook(file.path(sDirectory,sFileName), create = TRUE)
    df<-readWorksheet(wb, 
                      sheet = sSheetName,
                      header = FALSE,
                      dateTimeFormat = "%Y-%m-%d")
    
   getOption("XLConnect.dateTimeFormat")
    
    #If there's more columns in the header lookup, limit the lookup
    #to only those cases where there is no material in the unused columns
    if(ncol(df)+1==(ncol(lookup.RawHeaderList)-6)){#One Column
        BlankRows<-is.na(lookup.RawHeaderList[,ncol(df)+1])
        lookup.RawHeaderList<-lookup.RawHeaderList[BlankRows,]
    }
    else if(ncol(df)<ncol(lookup.RawHeaderList)-6){#MultipleColumns
        MaterialInUnusuedColumns<-subset(lookup.RawHeaderList,select=-c(Remove,
                                                                        Header,
                                                                        OverrideSection,
                                                                        Subsection,
                                                                        Unit,
                                                                        OverrideSource))
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
    if(!is.na(Source)){
        df$Source<-Source
    }
    sPrefix<-ifelse(sSheetName %in% c("Sheet1"),NA,sSheetName)
    #Remove type names, as we figure that out based on the headers.
    # df<-subset(df,Type!="Type" | is.na(Type))
    SplitAtBlankRow(df,
                    sPrefix,
                    sDirectory=sDirectory,
                    HeaderList=lookup.CleanHeaderList,
                    Source=Source
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

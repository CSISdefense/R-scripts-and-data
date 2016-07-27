

#*************************************Source Files*****************************************************
# Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"


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
# setwd("D://Users//Greg Sanders//Documents//Development//R-scripts-and-data//FOIA SAR data")
# setwd("K://2007-01 PROFESSIONAL SERVICES//R scripts and data//FOIA SAR data")
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
    }
    
    sSection<-NA
    if ("Section" %in% colnames(df)){
        sSection<-min(df$Section)
    }
    
    sHeader<-NA
    # Label and then remove columns that are completely blank
    # in this extraction. E.g. some extractions are 8 columns, some are 4.
    df<-df[!df$Type %in% c("Name","Type"),]
    if(any(!is.na(df$Header))&
        (min(as.character(df$Header),na.rm=TRUE)==max(as.character(df$Header),na.rm=TRUE))){
        sHeader<-max(as.character(df$Header),na.rm=TRUE)
        HeaderList<-subset(HeaderList,Header==sHeader,select=-c(Header,Type))
        colnames(df)[1:ncol(HeaderList)]<-HeaderList[1,]
    }
    df<-df[!df$Type %in% c("Header"),]
    BlankCols<-colSums(!is.na(df)) == 0
    df<-df[,!BlankCols]
    
    
    write.csv(df,
              gsub(" ","_",paste(sDirectory,
                    FilePrefix,
                    "_",
                    Increment,
                    ifelse(!is.na(sHeader),paste("_",sHeader,sep=""),""),
                    ifelse(!is.na(sSection),paste("_",sSection,sep=""),""),
                    ".csv",sep="")),
              row.names=FALSE)
    df
}

# df$Header[df$Type=="Name"]

SplitAtBlankRow<-function(df,
                          FilePrefix="",
                          Increment=1,
                          sDirectory="",
                          HeaderName="",
                          HeaderList,
                          Section=NA){
    
    Header<-NA
    if(nrow(df)>1){
        df[df==""]<-NA
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
    wb<-loadWorkbook(paste(sDirectory,sFileName,sep=""), create = TRUE)
    df<-readWorksheet(wb, 
                      sheet = sSheetName,
                      header = FALSE)
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
                    paste(Platform,Source,sSheetName,sep="_"),
                    sDirectory=sDirectory,
                    HeaderList=lookup.RawHeaderCleaned
    )
}

#*************************************Options*****************************************************



ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Cost Summary",
             "MEADS//")


ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Funding Summary",
             "MEADS//")


ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Low Rate Initial Production",
             "MEADS//")


ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Foreign Military Sales",
             "MEADS//")

ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Unit Cost",
             "MEADS//")

ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Cost Variance",
             "MEADS//")



ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Contracts",
             "MEADS//")

ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Deliveries and Expenditures",
             "MEADS//")

ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
             "MEADS","2011-12",
             "Operating and Support Cost",
             "MEADS//")

undebug(SplitAtBlankRow)
# debug(ReadAndSplit)
# debug(SplitAtBlankRow)
# debug(ReadAndSplit)
# debug(SplitAtBlankRow)
# undebug(CleanExtractAndWrite)
ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Cost Summary",
             "MEADS//")




ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Funding Summary",
             "MEADS//")

ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Annual Funding by Appropriation",
             "MEADS//")



# ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
#              "MEADS","2013-12",
#              "Low Rate Initial Production",
#              "MEADS//")
# 
# 
# ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
#              "MEADS","2013-12",
#              "Foreign Military Sales",
#              "MEADS//")
# 
ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Unit Cost",
             "MEADS//")

ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Unit Cost History",
             "MEADS//")

ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Cost Variance",
             "MEADS//")


# ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
#              "MEADS","2013-12",
#              "Contracts",
#              "MEADS//")

ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Deliveries and Expenditures",
             "MEADS//")

ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
             "MEADS","2013-12",
             "Operating and Support Cost",
             "MEADS//")

#A400M
ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2015",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2014",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2013",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2012",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2011",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2010",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2009",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2008",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2007",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2006",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2005",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2004",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2003",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2002",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2001",
             "A400M//")

ReadAndSplit("NAO Cost Data Draw.xlsx",
             "A400M",
             "2000",
             "A400M//")

#SM3 Block IIA
ReadAndSplit("2011 SM3 Budget Doc Data Draw.xlsx",
             "2011_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")

ReadAndSplit("2012 SM3 Budget Doc Data Draw.xlsx",
             "2012_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")

ReadAndSplit("2013 SM-3 Budget Data Draw.xlsx",
             "2013_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")

ReadAndSplit("2014 SM-3 Budget Doc Data Draw.xlsx",
             "2014_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")

ReadAndSplit("2015 SM-3 Budget Doc Data Draw.xlsx",
             "2015_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")

ReadAndSplit("SM-3 GAO Data Draw.xlsx",
             "GAO_SM3_Block_IIA",
             "Sheet1",
             "SM-3 BLOCK IIA//")



##AGS
ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "Program Overview",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2016",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2015",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2014",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2013",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2012",
             "NATO AGS//")


ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2010",
             "NATO AGS//")

ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2009",
             "NATO AGS//")


ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2007",
             "NATO AGS//")


ReadAndSplit("AGS Budget Docs Data Draw.xlsx",
             "NATO_AGS",
             "2004",
             "NATO AGS//")


ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Cost Summary",
             "NATO AGS//")


ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Funding Summary",
             "NATO AGS//")

# 
# ReadAndSplit("RQ4 Data Draw.xlsx",
#              "RQ4_",
#              "Low Rate Initial Production",
#              "NATO AGS//")
# 

ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Foreign Military Sales",
             "NATO AGS//")

ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Unit Cost",
             "NATO AGS//")


ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Unit Cost History",
             "NATO AGS//")


ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Cost Variance",
             "NATO AGS//")



ReadAndSplit("RQ4 Data Draw.xlsx",
             "RQ4_",
             "Contracts",
             "NATO AGS//")

# ReadAndSplit("RQ4 Data Draw.xlsx",
#              "RQ4_",
#              "Deliveries and Expenditures",
#              "NATO AGS//")
# 
# ReadAndSplit("RQ4 Data Draw.xlsx",
#              "RQ4_",
#              "Operating and Support Cost",
#              "NATO AGS//")
# 

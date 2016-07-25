

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
require(xlsx)

#*************************************Functions******************************************
SplitAtBlankRow<-function(df,FilePrefix="",Increment=1,sDirectory=""){
  if(nrow(df)>1){
    df[df==""]<-NA
    BlankRows<-rowSums(!is.na(df)) == 0
    TotalRows<-df[,1] %in% c("Total","Subtotal","End Year")
    TotalPriorRow<-c(FALSE,TotalRows)[1:length(BlankRows)]
    BlankRows<-BlankRows&TotalPriorRow
    
    if(any(BlankRows)){
      NextBlankLine<-min(which(BlankRows == TRUE))
      FirstExtract<-df[1:(NextBlankLine-1),]
      write.csv(FirstExtract,paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""))
      #Recursing
      SplitAtBlankRow(df[(NextBlankLine+1):length(BlankRows),],
                      FilePrefix=FilePrefix,
                      Increment=Increment+1,
                      sDirectory=sDirectory)
    }
    else{
      write.csv(df,paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""))
    }
  }
  else if(length(df)>0){
    write.csv(df,paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""))
  }
}

ReadAndSplit<-function(sFileName,sFilePrefix="",sSheetName="",sDirectory=""){
  df<-read.xlsx2(paste(sDirectory,
                      sFileName,
                      sep="")
                ,sheetName = sSheetName)
  SplitAtBlankRow(df,
                  paste(sFilePrefix,sSheetName,sep="_"),
                  sDirectory=sDirectory
  )
  
}

#*************************************Options*****************************************************
setwd("D://Users//Greg Sanders//Documents//Development//R-scripts-and-data//FOIA SAR data")
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "CostSummary",
               "MEADS//")

  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Funding Summary",
               "MEADS//")
  
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Low Rate Initial Production",
               "MEADS//")

  debug(SplitAtBlankRow)
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Foreign Military Sales",
               "MEADS//")
  
  
  # ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
  #              "MEADS_2011-12",
  #              "Foreign Military Sales",
  #              "MEADS//")
  # 

  # ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
  #              "MEADS_2011-12",
  #              "Foreign Military Sales",
  #              "MEADS//")
  # 
  
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "CostSummary",
               "MEADS//")
  
  
  

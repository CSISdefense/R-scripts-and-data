

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
#*************************************Functions******************************************
SplitAtBlankRow<-function(df,FilePrefix="",Increment=1,sDirectory=""){
  if(nrow(df)>1){
    df[df==""]<-NA
    BlankRows<-rowSums(!is.na(df)) == 0
    
    # TotalRows<-df[,1] %in% c("Total","Subtotal","End Year")
    # TotalPriorRow<-c(FALSE,TotalRows)[1:length(BlankRows)]
    # BlankRows<-BlankRows&TotalPriorRow
    
    if(any(BlankRows)){
      NextBlankLine<-min(which(BlankRows == TRUE))
      FirstExtract<-df[1:(NextBlankLine-1),]
      # Label and then remove columns that are completely blank
      # in this extraction. E.g. some extractions are 8 columns, some are 4.
      BlankCols<-colSums(!is.na(FirstExtract)) == 0
      FirstExtract<-FirstExtract[,!BlankCols]
      write.csv(FirstExtract,paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""),
                row.names=FALSE)
      #Recursing
      SplitAtBlankRow(df[(NextBlankLine+1):length(BlankRows),],
                      FilePrefix=FilePrefix,
                      Increment=Increment+1,
                      sDirectory=sDirectory)
    }
    else{
      BlankCols<-colSums(!is.na(df)) == 0
      df<-df[,!BlankCols]
      write.csv(df,
                paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""),
                row.names=FALSE)
    }
  }
  else if(length(df)>0){
    write.csv(df,paste(sDirectory,FilePrefix,"_",Increment,".csv",sep=""),
              row.names=FALSE)
  }
}

ReadAndSplit<-function(sFileName,sFilePrefix="",sSheetName="",sDirectory=""){
  wb<-loadWorkbook(paste(sDirectory,sFileName,sep=""), create = TRUE)
  df<-readWorksheet(wb, 
                    sheet = sSheetName,
                    header = FALSE)
  SplitAtBlankRow(df,
                  paste(sFilePrefix,sSheetName,sep="_"),
                  sDirectory=sDirectory
  )
  
}

#*************************************Options*****************************************************

# undebug(SplitAtBlankRow)
# debug(ReadAndSplit)
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Cost Summary",
               "MEADS//")

  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Funding Summary",
               "MEADS//")
  
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Low Rate Initial Production",
               "MEADS//")

  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Foreign Military Sales",
               "MEADS//")
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Unit Cost",
               "MEADS//")
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Cost Variance",
               "MEADS//")
  
  
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Contracts",
               "MEADS//")
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Deliveries and Expenditures",
               "MEADS//")
  
  ReadAndSplit("2011_SAR DataDraw_MEADS.xlsx",
               "MEADS_2011-12",
               "Operating and Support Cost",
               "MEADS//")
  
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Cost Summary",
               "MEADS//")
  
  
  
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Funding Summary",
               "MEADS//")
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Annual Funding by Appropriation",
               "MEADS//")
  
  
  
  # ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
  #              "MEADS_2013-12",
  #              "Low Rate Initial Production",
  #              "MEADS//")
  # 
  # 
  # ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
  #              "MEADS_2013-12",
  #              "Foreign Military Sales",
  #              "MEADS//")
  # 
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Unit Cost",
               "MEADS//")
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Unit Cost History",
               "MEADS//")
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Cost Variance",
               "MEADS//")
  

  # ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
  #              "MEADS_2013-12",
  #              "Contracts",
  #              "MEADS//")
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
               "Deliveries and Expenditures",
               "MEADS//")
  
  ReadAndSplit("2013 SAR MEADS Data Draw.xlsx",
               "MEADS_2013-12",
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
  
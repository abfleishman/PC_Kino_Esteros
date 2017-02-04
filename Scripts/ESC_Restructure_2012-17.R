# Clear work space; the funtion below lists all objects in workspace so it is good to start with a blank slate
rm(list=ls());
# Load libraries; these are not all used
library(lubridate)
library(stringr)
library(timeDate)
library(doBy)
library(ggplot2)
library(lubridate)
library(gdata)
library(plyr)
library(data.table)
library(XLConnect)
library(gridExtra)
library(reshape2)
# Using the XLConnect library use the loop below to read in each spreadsheet from your data
# you must change the path to the data in the loadWorkbook function
# 
# for(i in 1:37){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Dropbox/PC_Kino_Esteros/DataEntryFiles/SantaCruzCensus2009-2011.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}
# for(i in 1:19){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Dropbox/PC_Kino_Esteros/DataEntryFiles/Santa Cruz_Census_2013_2014.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}
# for(i in 1:10){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Dropbox/PC_Kino_Esteros/DataEntryFiles/SantaCruzCensus_2012_2013.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}
for(i in 1:16){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Dropbox/PC_Kino_Esteros/DataEntryFiles/SantaCruz_survey_2014_2015_AF.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}

ESC_list<-ls(pattern = "ESC_In_Sheet")
 Temp1<-ESC_In_Sheet_1
 WSlist=ESC_list
 
# head(Temp1)
##################
Restructure<-function(WSlist,Estero, outPath){
  Temp3<-NULL
  for(j in 1:length(WSlist)){
    Temp1<-get(WSlist[j])
    Temp2<-NULL
    for(i in 2:length(Temp1[1,])){
      Temp<-cbind(Temp1[1],Temp1[i])
      Temp$Location  <-Temp[1,2]
      Temp$Date  <-as.Date(Temp[2,2])
      Temp$Date<- as.Date(as.POSIXct(Temp$Date,format="%Y-%m-%d"))                          
      Temp$Start  <-Temp[3,2]
      Temp$Start_Hour<-hour(Temp$Start)
      Temp$Start_Minute<-minute(Temp$Start)
      Temp$Start  <-strftime(Temp$Start, format="%H:%M")
      Temp$Start_Date_Time<-timeDate(paste(Temp$Date,Temp$Start_Hour, Temp$Start_Minute), format="%Y-%m-%d %H %M")
      Temp$End <-Temp[4,2]
      Temp$End_Hour<-hour(Temp$End)
      Temp$End_Minute<-minute(Temp$End)
      Temp$End  <-strftime(Temp$End, format="%H:%M")
      Temp$End_Date_Time<-timeDate(paste(Temp$Date,Temp$End_Hour, Temp$End_Minute), format="%Y-%m-%d %H %M")      
      Temp$Duration  <-Temp[5,2]
      Temp$Total_Count_Duration  <-Temp[6,2]
      Temp$Sky_cover_percent  <-Temp[7,2]
      Temp$Temp  <-Temp[8,2]
      Temp$Tide_Height  <-Temp[9,2]
      Temp$Tide_dir  <-Temp[10,2]
      Temp$Wind_Dir  <-Temp[11,2]
      Temp$Wind_speed_Beaufort  <-Temp[12,2]
      Temp$Wind_speed_MPH  <-NA
      Temp$Observers_num  <-Temp[13,2]
      Temp$Observers_initial  <-Temp[14,2]
      Temp$Notes  <-Temp[15,2]
      Temp$Grand_sp_total  <-Temp[16,2]

      names(Temp)[names(Temp)=="Col1"] <- "Species"
      names(Temp)[2] <- "Count"
      head(Temp,19)
      Temp<-Temp[18:length(Temp[,1]),]
      Temp$Count<-ifelse(is.na(Temp$Count),0,Temp$Count)
      Temp2<-rbind(Temp2, Temp)
    }
    Temp3<-rbind(Temp3,Temp2)

   }
    # Make a column for which estero is is    
    Temp3$Estero<-"Estero"
    # Fix location names so that they are standard 
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Estero La Cruz, ",replacement = "")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Estero Santa Cruz, ",replacement = "")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Punta la Cruz",replacement = "Punta Santa Cruz")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Oyster farm",replacement = "Oyster Farm")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Far levee site",replacement = "Far Levee Site")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Crab Coop",replacement = "Crab Camp")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Crab camp far",replacement = "Crab Camp, Far")
    
    Temp3$Loc_Code<-Temp3$Location
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Heron Island",replacement = "HI")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Far Levee Site",replacement = "FLS")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Levee Site",replacement = "LS")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Crab Camp, Far",replacement = "CCF")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Crab Camp",replacement = "CC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Middle Mud",replacement = "MM")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Oyster Farm",replacement = "OF")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Punta Santa Cruz",replacement = "PSC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Heron Colony",replacement = "HC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Montecristo",replacement = "M")
    Temp3$Tide_Height<-gsub(x = Temp3$Tide_Height,pattern = "Mid/high",replacement = "Mid/High")
  names(Temp3)  
  Temp3<-Temp3[,c("Location","Loc_Code","Estero","Date","Start_Date_Time","End_Date_Time","Start","Start_Hour","Start_Minute","End","End_Hour","End_Minute","Duration","Total_Count_Duration",
                    "Grand_sp_total","Sky_cover_percent","Temp","Tide_Height","Tide_dir","Wind_Dir","Wind_speed_Beaufort","Wind_speed_MPH","Observers_num","Observers_initial","Species","Count","Notes")]
    
    # split the notes from the counts
    Counts <- colsplit(Temp3$Count, ",", names=c("Count","Notes"))
    #fix a value with a "-" in the cell
    Counts$Count<-gsub("-","",Counts$Count)
    
    # add the fixed counts and notes to the data
    Temp3$Count<-as.numeric(Counts$Count)
    Temp3$Notes<-paste(Temp3$Notes,Counts$Notes, sep= " ; ")
    
    # Write the data to a CSV
  write.csv(Temp3,outPath,row.names = F)
}

Restructure(WSlist = ESC_list, Estero = "ESC", outPath = "~/ESCRestructured.csv")
ESCRestructured<-read.csv("~/ESCRestructured.csv")
# names(ESCRestructured)
unique(ESCRestructured$Date)

# 
# Counts <- colsplit(ESCRestructured$Count, ",", names=c("Count","Notes"))
# 
# #fix a value with a "-" in the cell
# Counts$Count<-gsub("-","",Counts$Count)
# unique(ESCRestructured$Count)
# unique(Counts$Count)
# unique(Counts$Notes)
# ESCRestructured$Notes<-paste( ESCRestructured$Notes,Counts$Notes, sep= " ; ")
# unique(hmm2)
# # add the fixed counts and notes to the data
# ESCRestructured$Count<-as.numeric(Counts$Count)
# ESCRestructured$Notes<-Counts$Notes
# 
# length(unique(ESCRestructured$Species))
# 
# #make list of species to check inconsistences in the names
# birdList<-as.data.frame(sort(unique(ESCRestructured$Species)))
# birdList$New<-birdList[,1]
# birdList$Water<-"0"
#  write.csv(birdList, "~/birdList2.csv")

#populate the correct names in the New column by opening in Excel
#also mark as waterbird "1" and non waterbird "0" in the Water column so that we can subset out the birds that we will not be analyzing. Do not include petrels and other birds that we have bever seen!

# read the data back in
birdList<-read.csv("~/birdList1.csv")
# ESCRestructured<-read.csv("~/ESCRestructured.csv")

# head(birdList)
table(is.na(ESCRestructured$Species))
#fix all the names
as.character(unique(ESCRestructured$Species[!ESCRestructured$Species%in%birdList$Species]))

ESCRestructured$Species <- birdList$New[ match(ESCRestructured$Species , birdList$Species) ]
unique(ESCRestructured$Species)
unique(ESCRestructured$Date)
write.csv(ESCRestructured,"~/Dropbox/PC_Kino_Esteros/PC_ESC_2014_2015_restructured.csv",row.names = F)


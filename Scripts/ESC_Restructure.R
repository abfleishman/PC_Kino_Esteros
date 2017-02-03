# Clear work space; the funtion below lists all objects in workspace so it is good to start with a blank slate
rm(list=ls());
# Load libraries; these are not all used
library(lubridate)
library(timeDate)
library(stringr)
library(doBy)
library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(XLConnect)
library(reshape2)
# Using the XLConnect library use the loop below to read in each spreadsheet from your data
# you must change the path to the data in the loadWorkbook function
for(i in 1:37){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Desktop/SantaCruzCensus2009-2011.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}

ESC_list<-ls(pattern = "ESC_In_Sheet")
Temp1<-ESC_In_Sheet_1
##################
i=2

Restructure<-function(WSlist,Estero, outPath){
  Temp3<-NULL
  for(j in 1:length(WSlist)){
    Temp1<-get(WSlist[j])
    Temp2<-NULL
    for(i in 2:length(Temp1[1,])){
      Temp<-cbind(Temp1[1],Temp1[i])
      Temp$Location  <-Temp[1,2]
      Temp$Date  <-as.Date(Temp[2,2])
      Temp$Date<- as.Date(as.POSIXct(Temp$Date,format="%Y-%m-%d")+(1463*60*60*24)-86400)
      Temp$Start  <-Temp[3,2]
      Temp$Start_Hour<-hour(Temp$Start)
      Temp$Start_Minute<-minute(Temp$Start)
      Temp$Start  <-strftime(Temp$Start, format="%H:%M")
      Temp$Start_Date_Time<-timeDate(paste(Temp$Date,Temp$Start_Hour, Temp$Start_Minute), format="%Y-%m-%d %H %M")
      Temp$Duration  <-as.numeric(Temp[13,2])
      Temp$End <-Temp$Start_Date_Time+Temp$Duration*60
      Temp$End_Hour<-hour(Temp$End)
      Temp$End_Minute<-minute(Temp$End)
      Temp$End  <-strftime(Temp$End, format="%H:%M")
      Temp$End_Date_Time<-timeDate(paste(Temp$Date,Temp$End_Hour, Temp$End_Minute), format="%Y-%m-%d %H %M")      
      Temp$Total_Count_Duration <-NA
      Temp$Tide_Height  <-Temp[5,2]
      Temp$Tide_dir  <-Temp[6,2]
      Temp$Sky_cover_percent  <-Temp[7,2]
      Temp$Temp  <-Temp[8,2]
      Temp$Wind_Dir  <-Temp[9,2]
      Temp$Wind_speed_MPH  <-Temp[10,2]
      Temp$Wind_speed_Beaufort  <-NA
      Temp$Observers_num  <-Temp[11,2]
      Temp$Observers_initial  <-Temp[12,2]
      Temp$Notes<-NA
      Temp$Grand_sp_total  <-length(15:length(Temp[,1]))
      
      names(Temp)[names(Temp)=="Col1"] <- "Species"
      names(Temp)[2] <- "Count"
      Temp<-Temp[15:length(Temp[,1]),]
      Temp$Count<-ifelse(is.na(Temp$Count),0,Temp$Count)
      Temp2<-rbind(Temp2, Temp)
    }
    Temp3<-rbind(Temp3,Temp2)

   }
    # Make a column for which estero is is    
    Temp3$Estero<-Estero
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
  Temp3<-Temp3[,c("Location","Loc_Code","Estero","Date","Start_Date_Time","End_Date_Time","Start","Start_Hour","Start_Minute","End","End_Hour","End_Minute","Duration","Total_Count_Duration",
                  "Grand_sp_total","Sky_cover_percent","Temp","Tide_Height","Tide_dir","Wind_Dir","Wind_speed_Beaufort","Wind_speed_MPH","Observers_num","Observers_initial","Species","Count","Notes")]
  
  # split the notes from the counts
  Counts <- colsplit(Temp3$Count, ",", names=c("Count","Notes"))
  #fix a value with a "-" in the cell
  Counts$Count<-gsub("-",0,Counts$Count)
  
  # add the fixed counts and notes to the data
  Temp3$Count<-as.numeric(Counts$Count)
  Temp3$Notes<-paste(Temp3$Notes,Counts$Notes, sep= " ; ")
  
    
    # Write the data to a CSV
  write.csv(Temp3,outPath,row.names = F)
}

Restructure(WSlist = ESC_list, Estero = "ESC", outPath = "~/ESCRestructured.csv")
ESCRestructured<-read.csv("~/ESCRestructured.csv")
names(ESCRestructured)
unique(ESCRestructured$Species)

length(unique(ESCRestructured$Species))

#make list of species to check inconsistences in the names
# birdList<-as.data.frame(sort(unique(ESCRestructured$Species)))
# birdList$New<-NULL
# birdList$Water<-NULL
# write.csv(birdList, "~/birdList.csv")

#populate the correct names in the New column by opening in Excel
#also mark as waterbird "1" and non waterbird "0" in the Water column so that we can subset out the birds that we will not be analyzing. Do not include petrels and other birds that we have bever seen!

# read the data back in
birdList<-read.csv("~/birdList.csv")
# ESCRestructured<-read.csv("~/ESCRestructured.csv")

# head(birdList)

#fix all the names
ESCRestructured$Species <- birdList$New[ match(ESCRestructured$Species , birdList$Species) ]
unique(ESCRestructured$Species)
# split the notes from the counts
# Counts <- colsplit(ESCRestructured$Count, ",", names=c("Count","Notes"))
# 
# #fix a value with a "-" in the cell
# Counts$Count<-gsub("-","",Counts$Count)
# 
# # add the fixed counts and notes to the data
# ESCRestructured$Count<-as.numeric(Counts$Count)
# ESCRestructured$Notes<-Counts$Notes

# # subset by the waterbird column that we made earlier
# Waterbirds<-subset(birdList,Water==1)
# 
# ESCRestructured_sub<-subset(ESCRestructured,(ESCRestructured$Species %in% Waterbirds$New))
# 
# sum(ESCRestructured$Count)

#you loos some because you are getting rid of some species
# sum(ESCRestructured_sub$Count)
write.csv(ESCRestructured,"~/Dropbox/PC_Kino_Esteros/PC_ESC_2009_2011_restructured.csv",row.names = F)

ggplot(ESCRestructured, aes(x=as.Date(Date),y=Count))+
  geom_line(stat="identity") +scale_fill_grey(start = 0, end = .6, na.value = "grey50")+
  geom_point(stat="identity") +
  #   geom_text(label=df1$Ind, size=3.5 , y=df1$Ind, x=df1$year, hjust=-0.4)+
  #   scale_x_continuous(breaks=seq(2001,2013,2))+
  xlab(NULL)+
  ylab(NULL)+
  ylim(0,80)+
  guides(colour=FALSE)+
  #xlim(160,260)+
  ggtitle(NULL)+
  theme(legend.position=c(0.09, .85), 
        legend.text=element_text(face="plain", size=24),
        #         legend.background = element_rect(fill="grey90", size=2, linetype="solid"),
        legend.title = element_text(face="plain", size=24),
        plot.title = element_text(face="plain", size=24),
        axis.title.x = element_text(face="plain", size=24, vjust=0.05, hjust=0.5),
        axis.title.y = element_text(face="plain", size=24, angle=90,vjust=0.3, hjust=0.5),
        axis.text.x= element_text(face="plain",size=24, angle=90, colour="black",vjust=0.3, hjust=0.5),
        axis.text.y= element_text(face="plain",size=24, angle=0, colour="black"),        
        #axis.ticks.y = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill='white', colour='black'))
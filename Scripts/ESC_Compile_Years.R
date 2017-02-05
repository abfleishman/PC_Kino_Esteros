gc()
rm(list=ls())
E17<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2016_2017_restructured.csv", stringsAsFactors=FALSE)
ESC2015<-read_csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2014_2015_restructured.csv")
ESC2014<-read_csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2013_2014_restructured.csv")
ESC2013<-read_csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2012_2013_restructured.csv")
ESC2011<-read_csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_2011_restructured.csv")
# unique(ESC2017$Date)
library(dplyr)
library(tidyr)
head(E17)
head(ESC2011)
ESC2017<-NULL
ESC2017<-ESC2011[0,]
collect1 <- data.frame(Species = E17$Species)
ESC2017<-left_join(collect1,ESC2017)
ESC2017$Location=E17$Point.Location
ESC2017$Estero="ESC"
ESC2017$Date=mdy(paste(E17$Date))
ESC2017$Start_Date_Time=mdy_hm(paste(E17$Date,E17$Time.Started),tz="US/Arizona")
ESC2017$End_Date_Time=mdy_hm(paste(E17$Date,E17$Time.Ended),tz="US/Arizona")
ESC2017$Start=E17$Time.Started
ESC2017$Start_Hour=hour(ESC2017$Start_Date_Time)
ESC2017$Start_Minute=minute(ESC2017$Start_Date_Time)
ESC2017$End=E17$Time.Ended
ESC2017$End_Hour=hour(ESC2017$Start_Date_Time)
ESC2017$End_Minute=minute(ESC2017$End_Date_Time)
ESC2017$Duration=as.numeric(difftime(ESC2017$End_Date_Time,ESC2017$Start_Date_Time,units = "min"))
ESC2017$Sky_cover_percent=as.numeric(gsub("%","",E17$Cloud.cover))
ESC2017$Temp=E17$Temp.F.
E17<-separate(E17,col = Tide,c("Tide_Height","Tide_dir"),sep = "-" )
ESC2017$Tide_Height<- E17$Tide_Height
ESC2017$Tide_dir=E17$Tide_dir
ESC2017$Wind_Dir=E17$Point.Location
ESC2017$Wind_speed_Beaufort=E17$Point.Location
ESC2017$Wind_speed_MPH=E17$Wind
ESC2017$Observers_num=3
ESC2017$Observers_initial=E17$Observer.1
ESC2017$Species=E17$Species
ESC2017$Count=E17$Count
ESC2017$Notes=E17$Notes

ESC2017$Location<-gsub("Pt. La Cruz","Punta Santa Cruz",ESC2017$Location)
ESC2017$Location<-gsub("Oyster Camp","Oyster Farm",ESC2017$Location)
ESC2017$Location<-gsub("Pt. La Cruz","Punta Santa Cruz",ESC2017$Location)
unique(ESC2017$Date)

head(ESC2017)                  

hmm<-rbind(ESC2011,ESC2013,ESC2014,ESC2015,ESC2017)
hmm$Location<-gsub("Pt. La Cruz","Punta Santa Cruz",hmm$Location)
hmm$Location<-gsub("Oyster Camp","Oyster Farm",hmm$Location)
hmm$Location<-gsub("Levee$","Levee Site",hmm$Location)
hmm$Location<-gsub("Monte Cristo","Montecristo",hmm$Location)
hmm$Location<-gsub("Punta La Cruz","Punta Santa Cruz",hmm$Location)
str(hmm)
hmm$Date<-as.Date(hmm$Date)
hmm$year<-year(hmm$Date)
hmm$month<-month(hmm$Date)
hmm$day<-day(hmm$Date)
hmm$Estero<-"ESC"
table(hmm$Location)

birdlist1<-read.csv("~/Dropbox/PC_Kino_Esteros/birdList.csv", stringsAsFactors = F)
# write.csv(birdList,"birdlist2.csv")
# write.csv(data.frame(as.character(unique(hmm$Species[!hmm$Species%in%birdlist1$Species]))),"bird.csv")
hmm$Species <- birdlist1$New[ match(hmm$Species , birdlist1$Species) ]
hmm<-left_join(hmm,birdlist1)
head(hmm)
hmm$Count<-gsub(",","",hmm$Count)
hmm$Count<-as.numeric(hmm$Count)
hmm<-filter(hmm,Count>0,!is.na(Count))
hmm$Season[hmm$month%in%c(12,1,2)]<-"Winter"
hmm$Season[hmm$month%in%c(3,4,5)]<-"Spring"
hmm$Season[hmm$month%in%c(6,7,8)]<-"Summer"
hmm$Season[hmm$month%in%c(9,10,11)]<-"Fall"
# hmm$rich<-1
hmm$Rich[str_detect(hmm$Species,"spp")]<-0
hmm$Rich[str_detect(hmm$Species,"/")]<-0
hmm$Rich[str_detect(hmm$Species,"Dark Herons")]<-0
hmm$Rich[str_detect(hmm$Species,"White Herons")]<-0


Tax<-read_excel("~/Dropbox/PC_Kino_Esteros/BirdLife_Checklist_Version_0.xlsx",sheet = 1)
Clem<-read_excel("~/Downloads/eBird-Clements-Checklist-v2016-10-August-2016.xlsx",sheet = 1)
head(Clem)
head(Tax)
Clem2<-select(Clem[,1:15],Family=Family,SName=`Scientific name`,Species=`English name`,Order,Category) %>% 
  filter(!is.na(Species),Category=="species")

head(Clem2)

unique(hmm$Species)[ ! unique(hmm$Species)%in% unique(Clem2$Species)]

Tax2<-select(Tax,Family=`Family name`,SName=`Scientific name`,Species=`Common name`)

unique(hmm$Species)[  unique(hmm$Species)%in% unique(Tax2$Species)]
# write.csv(data.frame(unique(hmm$Species)[!unique(hmm$Species)%in% unique(Tax2$Species)]),"Miss_Fam.csv")
Miss_fam<-read.csv("Miss_Fam.csv")
Tax2<-bind_rows(Tax2,Miss_fam)
hmm<-left_join(hmm,Tax2)

unique(hmm$Species)[!unique(hmm$Species)%in% unique(Tax2$Species)]

head(hmm)

Popest<-read_excel("~/Dropbox/PC_Kino_Esteros/Waterbird_Esteimates/wpe5.xlsx",sheet = 2)
head(Popest)
Popest2<-select(Popest,SName=`Scientific name`,Species=`Common name`,Pop_name=`Population Name`,OncP=`1pc`)
unique(hmm$Species)[!unique(hmm$SName)%in% unique(Popest2$SName)]

hmm<-select(hmm,Location,Loc_Code,Estero,Date,Start,End, Start_Date_Time,End_Date_Time,year,month,day,Start_Hour,End_Hour,Start_Minute, End_Minute,Season, Duration,Total_Count_Duration,Grand_sp_total,Sky_cover_percent,Temp,Tide_Height,Tide_dir,Wind_Dir,Wind_speed_Beaufort,Wind_speed_MPH,Observers_num,Observers_initial,Family, SName,Species, Rich,Water,Count,Notes)

write.csv(hmm,"~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_Jan2017_restructured.csv")
saveRDS(hmm,"~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_Jan2017_restructured.rda")
head(hmm)
View(data.frame(unique(hmm$Species)))

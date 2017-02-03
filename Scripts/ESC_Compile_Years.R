
E17<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2016_2017_restructured.csv", stringsAsFactors=FALSE)
ESC2015<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2014_2015_restructured.csv", stringsAsFactors=FALSE)
ESC2014<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2013_2014_restructured.csv", stringsAsFactors=FALSE)
ESC2013<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2012_2013_restructured.csv",stringsAsFactors=FALSE)
ESC2011<-read.csv("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_2011_restructured.csv",stringsAsFactors=FALSE)
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
ESC2017$Sky_cover_percent=E17$Cloud.cover
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
head(ESC2017)                  
hmm<-rbind(ESC2011,ESC2013,ESC2014,ESC2015,ESC2017)
hmm$Location<-gsub("Pt. La Cruz","Punta Santa Cruz",hmm$Location)
hmm$Location<-gsub("Oyster Camp","Oyster Farm",hmm$Location)
hmm$Location<-gsub("Levee$","Levee Site",hmm$Location)
hmm$Location<-gsub("Monte Cristo","Montecristo",hmm$Location)
hmm$Location<-gsub("Punta La Cruz","Punta Santa Cruz",hmm$Location)
hmm$Date<-as.Date(hmm$Date)
hmm$year<-year(hmm$Date)
hmm$month<-month(hmm$Date)
hmm$day<-day(hmm$Date)
hmm$Estero<-"ESC"
table(hmm$Location)
hmm$Species<-gsub("Yellowlegs sp.","Yellowlegs spp.",hmm$Species)
hmm$Species<-gsub("Yellowlegs spp..","Yellowlegs spp.",hmm$Species)
hmm$Species<-gsub("Yellow-Footed Gull","Yellow-footed Gull",hmm$Species)
hmm$Species<-gsub("Double Crested Cormorant","Double-crested Cormorant",hmm$Species)
hmm$Species<-gsub("Double crested Cormorant","Double-crested Cormorant",hmm$Species)
hmm$Species<-gsub("Dowitcher Sp.","Dowitcher spp.",hmm$Species)
hmm$Species<-gsub("Gull Sp.","Gull spp.",hmm$Species)
hmm$Species<-gsub("Gull spp. ","Gull spp.",hmm$Species)
hmm$Species<-gsub("Gull spp..","Gull spp.",hmm$Species)
hmm$Species<-gsub("Cedar Waxwing","Ceder Waxwing",hmm$Species)
hmm$Species<-gsub("Chihuahua Raven","Chihuahuan Raven",hmm$Species)
hmm$Species<-gsub("Great-Blue Heron","Great Blue Heron",hmm$Species)
hmm$Species<-gsub("Heerman's Gull","Heermann's Gull",hmm$Species)
hmm$Species<-gsub("Large Shorebird$","Large Shorebird spp.",hmm$Species)
hmm$Species<-gsub("large shorebird spp.","Large Shorebird spp.",hmm$Species)
hmm$Species<-gsub("shorebird","Shorebird",hmm$Species)
hmm$Species<-gsub("Redish Egret","Reddish Egret",hmm$Species)
hmm$Species<-gsub("Phalarope","Phalarope spp.",hmm$Species)
hmm$Species<-gsub("Plover Sp.","Plover spp.",hmm$Species)
hmm$Species<-gsub("Pied billed Grebe","Pied-billed Grebe",hmm$Species)
hmm$Species<-gsub("Midium","Medium",hmm$Species)
hmm$Species<-gsub("Long Billed Dowitcher","Long-billed Dowitcher",hmm$Species)
hmm$Species<-gsub("Long Billed Curlew","Long-billed Curlew",hmm$Species)
hmm$Species<-gsub("Long billed Curlew","Long-billed Curlew",hmm$Species)
hmm$Species<-gsub("Ring-Billed Gull","Ring-billed Gull",hmm$Species)
hmm$Species<-gsub("Clapper Rail","Ridgeway's Rail",hmm$Species)
hmm$Species<-gsub("Semi-palmated Plover","Semipalmated Plover",hmm$Species)
hmm$Species<-gsub("Small Shorebird Sp.","Small Shorebird spp.",hmm$Species)
hmm$Species<-gsub("^tern$","Tern",hmm$Species)
hmm$Species<-gsub("Tern Sp","Tern spp",hmm$Species)
hmm$Species<-gsub("^Tern","Tern spp.",hmm$Species)
hmm$Species<-gsub("Tern spp. spp.","Tern spp.",hmm$Species)

hmm$Species<-gsub("Tri-colored Heron","Tricolored Heron",hmm$Species)
hmm$Species<-gsub("Tri-Colored Heron","Tricolored Heron",hmm$Species)
hmm$Species<-gsub("Black Bellied Plover","Black-bellied Plover",hmm$Species)
hmm$Species<-gsub("Black bellied Plover","Black-bellied Plover",hmm$Species)
hmm$Species<-gsub("Brants","Brant",hmm$Species)
hmm$Species<-gsub("Forester's","Forster's",hmm$Species)
hmm$Species<-gsub("Loon Sp","Loon spp",hmm$Species)
hmm$Species<-gsub("Peep sp.$","Peep spp.",hmm$Species)
hmm$Species<-gsub("Peep Sp.$","Peep spp.",hmm$Species)
hmm$Species<-gsub("Peep Species","Peep spp.",hmm$Species)
hmm$Species<-gsub("Peeps Sp","Peeps spp",hmm$Species)
hmm$Species<-gsub("Peeps","Peep spp.",hmm$Species)
hmm$Species<-gsub("Peep spp. spp.","Peep spp.",hmm$Species)
hmm$Species<-gsub("Sandpiper Sp","Peep spp.",hmm$Species)
hmm$Species<-gsub("Turnstone Sp.","Turnstone spp.",hmm$Species)
hmm$Species<-gsub("Yellow Crowned Night Heron","Yellow-crowned Night Heron",hmm$Species)
hmm$Species<-gsub("Yellow-crowned Night-Heron","Yellow-crowned Night Heron",hmm$Species)
hmm$Count<-gsub(",","",hmm$Count)
hmm$Count<-as.numeric(hmm$Count)
hmm<-filter(hmm,Count>0,!is.na(Count))
hmm$Season[hmm$month%in%c(12,1,2)]<-"Winter"
hmm$Season[hmm$month%in%c(3,4,5)]<-"Spring"
hmm$Season[hmm$month%in%c(6,7,8)]<-"Summer"
hmm$Season[hmm$month%in%c(9,10,11)]<-"Fall"
hmm$rich<-1
hmm$rich[str_detect(hmm$Species,"spp")]<-0
hmm$rich[str_detect(hmm$Species,"/")]<-0
hmm$rich[str_detect(hmm$Species,"Dark Herons")]<-0
hmm$rich[str_detect(hmm$Species,"White Herons")]<-0

hmm<-filter(hmm,Count>0)
head(hmm)
hmm<-select(hmm,Location,Loc_Code,Estero,Date,Start,End, Start_Date_Time,End_Date_Time,year,month,day,Start_Hour,End_Hour,Start_Minute, End_Minute,Duration,Total_Count_Duration,Grand_sp_total,Sky_cover_percent,Temp,Tide_Height,Tide_dir,Wind_Dir,Wind_speed_Beaufort,Wind_speed_MPH,Observers_num,Observers_initial,Species, rich,Count,Notes)

write.csv(hmm,"~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_2017_restructured.csv")
saveRDS(hmm,"~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_2017_restructured.rda")
head(hmm)
View(data.frame(unique(hmm$Species)))

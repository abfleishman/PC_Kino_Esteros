library(tidyverse)
hmm<-readRDS("~/Dropbox/PC_Kino_Esteros/Data_Restruct/PC_ESC_2009_Jan2017_restructured.rda")

hmm1<-filter(hmm,Rich!=0, Water==1)
# Richness_month_Year -----------------------------------------------------

Month_Sp_Rich<-hmm1 %>%
  group_by(year, month) %>%
  summarise(SpRich=length(unique(Species)),Total_ind=sum(Count)) %>%
  group_by(year, month) %>%
  summarise(Rich=mean(SpRich),SD=sd(SpRich),num=sum(Total_ind),N=n())

Month_Sp_Rich$year<-factor(Month_Sp_Rich$year)
Month_Sp_Rich$month<-factor(Month_Sp_Rich$month)

ggplot(Month_Sp_Rich,aes(x=year,y=Rich))+facet_wrap(~month,nrow = 3)+
  geom_bar(stat="identity",position=position_dodge(.5),fill="dodgerblue1",width=.9) +
  ylab("Species Richness")+
  xlab("year")+theme_bw(base_size = 24)+theme(axis.text.x =element_text(angle = 90))
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Richness_month_Year.jpg",scale = 1,width = 12,height=8,units = "in",dpi = 450)

Month_Sp_Rich_All<-Month_Sp_Rich %>%
  group_by(month) %>%
  summarise(RICH=mean(Rich),SD=sd(Rich),Num=mean(num),SD_Num=sd(num),N=n())
ggplot(Month_Sp_Rich_All,aes(x=month,y=RICH))+
  geom_bar(stat="identity",position=position_dodge(.5),fill="dodgerblue1",width=.9)+
  geom_errorbar(aes(ymin=RICH-SD,ymax=RICH+SD),width=.5,position=position_dodge(.5))+
  ylab("Species Richness ± SD")+
  xlab("Month")+theme_bw(base_size = 24)
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Mean_Richness_month.jpg",scale = 1,width = 10,height=10,units = "in",dpi = 450)

# Mean Sp Abundance by month ----------------------------------------------
Month_Sp_Abun<-hmm %>%
  group_by(year, month) %>%
  summarise(SpRich=length(unique(Species)),Total_ind=sum(Count)) %>%
  group_by(year, month) %>%
  summarise(Rich=mean(SpRich),SD=sd(SpRich),num=sum(Total_ind),N=n())

Month_Sp_Abun$year<-factor(Month_Sp_Abun$year)
Month_Sp_Abun$month<-factor(Month_Sp_Abun$month)

Month_Sp_Abun_All<-Month_Sp_Abun %>%
  group_by(month) %>%
  summarise(RICH=mean(Rich),SD=sd(Rich),Num=mean(num),SD_Num=sd(num),N=n())

ggplot(Month_Sp_Abun_All,aes(x=month,y=Num))+
  geom_bar(stat="identity",position=position_dodge(.5),fill="dodgerblue1",width=.9)+
  geom_errorbar(aes(ymin=Num-SD_Num,ymax=Num+SD_Num),width=.5,position=position_dodge(.5))+
  ylab("Total Abundance ± SD")+
  xlab("Month")+theme_bw(base_size = 24)
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Total_Abundance_month.jpg",scale = 1,width = 10,height=10,units = "in",dpi = 450)

# Seasonal Species Richness by year ---------------------------------------------------------
Year_Sum<-hmm1 %>%
  group_by(year, Season) %>%
  summarise(SpRich=length(unique(Species)),Total_ind=sum(Count))

Year_Sum$year<-factor(Year_Sum$year)

ggplot(Year_Sum,aes(x=Season,y=SpRich, fill=(year)))+
  geom_bar(stat="identity",position=position_dodge(.5),width=.5) +
  ylab("Species Richness")+
  xlab("Year")+theme_bw(base_size = 24)+theme(axis.text.x =element_text(angle = 90))
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Seasonal_Species_Richness_by_year.jpg",scale = 1,width = 12,height=8,units = "in",dpi = 450)
# 
# ggplot(Year_Sum,aes(x=year,y=SpRich, fill=(Season)))+
#   geom_bar(stat="identity",position=position_dodge(.5),width=.5) +
#   ylab("Species Richness")+
#   xlab("Year")+theme_bw(base_size = 24)+theme(axis.text.x =element_text(angle = 90))
# ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Seasonal_Species_Richness_by_year.jpg",scale = 1,width = 12,height=8,units = "in",dpi = 450)

# Mean Abundance by season ------------------------------------------------

Season_sum<-hmm %>%
  group_by(year, Season) %>%
  summarise(SpRich=length(unique(Species)),Total_ind=sum(Count)) %>%
  group_by(Season) %>%
  summarise(Rich=mean(SpRich),SD=sd(Total_ind),num=mean(Total_ind),N=n())

ggplot(Season_sum,aes(x=Season,y=num))+
  geom_bar(stat="identity",position=position_dodge(.5),fill="dodgerblue1",width=.9) +
  geom_errorbar(aes(ymin=num-SD,ymax=num+SD),width=.5,position=position_dodge(.5))+
  ylab("Mean Abundance")+
  xlab("year")+theme_bw(base_size = 24)+theme(axis.text.x =element_text(angle = 90))
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Mean_Seasonal_Abundance.jpg",scale = 1,width = 12,height=8,units = "in",dpi = 450)

 
# Sum_Max<-hmm %>%
#   group_by(Species) %>%
#   summarise(Max=max(Count)) %>%
#   arrange(Max) %>%
#   filter(Max>50,!is.na(Species))

# Max by species ----------------------------------------------------------

Sum_Max<-hmm1 %>%
  group_by(Species) %>%
  summarise(Max=max(Count)) %>%
  arrange(Max) %>%
  filter(Max>50,!is.na(Species))
ggplot(Sum_Max,aes(x=reorder(Species, Max),y=Max))+
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Maximum # Individuals")+
  xlab("Species")+
  theme_bw(base_size = 24)
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/Max_by_spp.jpg",scale = 1,width = 12,height=12,units = "in",dpi = 450)


# Willit abundance --------------------------------------------------------
will<-filter(hmm1, Species=="Willet")
will_sum<-will %>% 
  group_by(year,month) %>% 
  summarise(Mean_Num=mean(Count),SD=sd(Count))

ggplot(will_sum,aes(x=month,y=Mean_Num))+
  geom_bar(stat="identity")+
  facet_grid(.~year)+
  theme_bw(base_size = 24)
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/mean_williet.jpg",scale = 1,width = 8,height=18,units = "in",dpi = 450)

# Species abundance --------------------------------------------------------

Species_abun_sum<-hmm1 %>% 
  group_by(Species) %>% 
  filter(n()>10,max(Count)>10) %>% 
  group_by(Species,year,month) %>% 
  summarise(Mean_Num=mean(Count),SD=sd(Count)) %>% 
  group_by(Species,month) %>% 
  summarise(Mean=mean(Mean_Num),SD=sd(Mean_Num))
Species_abun_sum$month<-factor(Species_abun_sum$month,levels =c(8,9,10,11,12,1,2,3,4,5,6) )
Species_abun_sum<-arrange(Species_abun_sum,Species, month)

ggplot(Species_abun_sum,aes(x=month,y=Mean,group=Species))+
  geom_point(colour="dodgerblue3")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)+
  geom_path(colour="dodgerblue1")+
  facet_wrap(~Species, scales = "free_y")+theme_bw(base_size = 24)
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/mean_species_by_month.jpg",scale = 1,width = 15,height=20,units = "in",dpi = 450)

OneP<-read.csv("~/Dropbox/PC_Kino_Esteros/OnePct.csv")
Species_abun_sum<-hmm1 %>% 
  group_by(Species) %>% 
  filter(Species%in% c("Reddish Egret","American Oystercatcher","Wilson's Plover","Snowy Plover", "Marbled Godwit","Willit","Black Skimmer" ,"Forster's Tern","Elegant Tern", "Royal Tern")) %>% 
  group_by(Species,year,month) %>% 
  summarise(Mean_Num=mean(Count),Max=max(Count),SD=sd(Count)) %>% 
  group_by(Species,month) %>% 
  summarise(Mean=mean(Mean_Num),Max=max(Max),SD=sd(Mean_Num))
Species_abun_sum$month<-factor(Species_abun_sum$month,levels =c(8,9,10,11,12,1,2,3,4,5,6) )
Species_abun_sum<-arrange(Species_abun_sum,Species, month)

Species_abun_sum<-left_join(Species_abun_sum,OneP)
unique(Species_abun_sum$OnePct)
ggplot(Species_abun_sum,aes(x=month,y=Max,group=Species))+
  geom_hline(aes(yintercept=OnePct),colour="red",linetype=2)+
  geom_point(colour="dodgerblue3")+
  geom_path(colour="dodgerblue1")+
  facet_wrap(~Species, scales = "free_y")+theme_bw(base_size = 24)+ylab("Maximum Seasonal Abundance")

ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/max_seasonal_abundance_by_sp.jpg",scale = 1,width = 16,height=12,units = "in",dpi = 450)

# Group by Family  -------------------------------------------------------------
Fam<-hmm %>%
  group_by(Family) %>% 
  filter(n()>10,max(Count)>3, Water==1) %>% 
  group_by(Family,year,month) %>% 
  summarise(Mean_Num=mean(Count),Max=max(Count),SD=sd(Count)) %>% 
  group_by(Family,month) %>% 
  summarise(Mean=mean(Mean_Num),Max=max(Max),SD=sd(Mean_Num))
Fam$month<-factor(Fam$month,levels =c(8,9,10,11,12,1,2,3,4,5,6) )
Fam<-arrange(Fam,Family, month)

ggplot(Fam,aes(x=month,y=Max,group=Family))+
  geom_point(colour="dodgerblue3")+
  geom_path(colour="dodgerblue1")+
  facet_wrap(~Family, scales = "free_y")+theme_bw(base_size = 24)+ylab("Maximum Seasonal Abundance")
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/max_seasonal_abundance_by_fam.jpg",scale = 1,width = 16,height=12,units = "in",dpi = 450)

ggplot(Fam,aes(x=month,y=Mean,group=Family))+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)+
  geom_point(colour="dodgerblue3")+
  geom_path(colour="dodgerblue1")+
  facet_wrap(~Family, scales = "free_y")+theme_bw(base_size = 24)+ylab("Mean ± SD (Seasonal Abundance)")
ggsave(filename  = "~/Dropbox/PC_Kino_Esteros/Plots/mean_seasonal_abundance_by_fam.jpg",scale = 1,width = 16,height=12,units = "in",dpi = 450)

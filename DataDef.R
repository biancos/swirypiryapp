library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(car)
library(shinydashboard)

#setwd("C:/Users/biancoscudellas/Documents/GitHub/swirypiryapp")
stcs<-readRDS("./data/stcsDB_L2_19-09-2017.rds")

for(i in 1:length(stcs)){
  ##replace . with _ in variables names
  colnames(stcs[[i]])<-gsub("[.]", "_",names(stcs[[i]]))
}


for(i in 1:length(stcs)){
  ##replace . with _ in variables names
  colnames(stcs[[i]])<-gsub("_0", "",names(stcs[[i]]))
}



cbsas<-stcs$cb.sas
cbsas<-mutate(cbsas,variable0= ifelse(multiple_entry=="multiple",
                                      paste(variable,"0",sep="_"),
                                      paste(variable)))

types<-distinct(cbsas, crf,variable,type)

organs <- c("kidney", "heart","pat")
forms<- c("bl","fup")
versions <-c("v1","v2")


labels<-readRDS('./data/ecbLabels.rds')
labels[labels$variable=="trust",]$default_label<-"How much do you trust your transplant team, where '0' is 'not at all' and '10' is 'completely'? (please select only one answer)"
labels[labels$variable=="anticmvdate",]$default_label<-"Anti-CMV IgG assessment date"

labels[labels$variable=="donantihbc",]$default_label<-"Donor Anti-HBc test"
labels[labels$variable=="donantiebv",]$default_label<-"Donor Anti-EBV test"
labels[labels$variable=="donantihbs",]$default_label<-"Donor Anti-HBs test"
labels[labels$variable=="donantihcv",]$default_label<-"Donor Anti-HCV test"
labels[labels$variable=="donantihiv",]$default_label<-"Donor Anti-HIV test"
labels[labels$variable=="donantihsv",]$default_label<-"Donor Anti-HSV test"
labels[labels$variable=="donantihsv",]$default_label<-"Donor Anti-toxo test"


labels<-labels %>% select(crf,variable, default_label)
labels$figure_title<-ifelse(is.na(labels$default_label), "! Figure title missing : label not recorded in the electronic codebook", 
                         paste(labels$default_label,"",sep = ""))



blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )



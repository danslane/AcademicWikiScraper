#Load Packages (likely need to install some first)
library(plyr)
library(tidyverse) 
library(ggmap)
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)
library(fuzzywuzzyR)

#set your working directory
setwd("") 

#Load higher ed databases
schooldata <- read.csv("hd2015.csv", header=TRUE,stringsAsFactors=FALSE)
enrollment <- read.csv("effy2015.csv", header=TRUE,stringsAsFactors=FALSE)

#scrape wiki
url <-'http://academicjobs.wikia.com/wiki/Communication_and_Media_Studies_2018-2019'
page <- read_html(url)

#Extract School & State
schools1<-html_text(html_nodes(page,"[class='mw-headline']"))
schools1<-schools1[(which(schools1=="Communication and Media Studies Jobs for Academic Year 2018-2019")+1)
                   :(which(schools1=="Demographics")-1)]
schools1<-iconv(schools1,to="UTF8")
schools<-gsub("\\(.*","",schools1)
state<-gsub("^[^\\(]*\\(\\s*|\\)[^\\)]*$", "", schools1)
status<-sapply(strsplit(schools1,"-"), "[", 2)
M<-data.frame(schools,state,status)

##Extract Ad text
o<-html_text(html_nodes(page,xpath=paste("//*[@id='mw-content-text']",sep="")))
o<-gsub("2019Edit\n","",o)
o<-gsub("WikiEdit\n","",o)
gsub("2019Edit\n","",o)
split<-as.data.frame(unlist((strsplit(o,"Edit\n"))))
colnames(split)<-c("V1")
split$V1<-as.character(split$V1)
M$ad<-as.vector(split[2:(which(split$V1==" Visitor Type ")-1),])
M$ad<-gsub("List Updates Here:.*","",M$ad)

#Extract beginning date
M$Begin<-gsub("\\..*","",substr(M$ad, str_locate(M$ad, "beginning")+10, nchar(M$ad)))

#Code for occurence of keywords
M$Quant<-0
M$Quant[grepl("quantitative",M$ad)]<-1
M$Qual<-0
M$Qual[grepl("qualitative",M$ad)]<-1
M$Mixed<-0
M$Mixed[M$Quant==1&M$Qual==1]<-1
M$SocSci<-0
M$SocSci[grepl("social science",M$ad)]<-1
M$Human<-0
M$Human[grepl("humanities",M$ad)]<-1
M$AstProf<-0
M$AstProf[grepl("assistant professor",M$ad)]<-1
M$Type<-"Unknown"
M$Type[M$Quant==1]<-"Quantiative"
M$Type[M$Qual==1]<-"Qualitative"
M$Type[M$Mixed==1]<-"Mixed"

table(M$Quant)
table(M$Qual)
table(M$Mixed)
table(M$SocSci)
table(M$Human)
table(M$AstProf)
table(M$Pol)

#Merge w/ higher education data
colnames(schooldata)[2]<-"schools"
S<-schooldata
S$schools2<-S$schools
M$schools2<-NA

#this loop find the "best match" between wiki school name and names in the instution database

n<-0
for (i in M$schools){
n<-n+1
tryCatch({
  
 
  
  M$schools2[n]<-GetCloseMatches(as.character(M$schools[n]),schooldata$schools,n=1)
}, error=function(e){})
}

#Check to see how well matching worked
data.frame(M$schools,M$schools2)

#this often needs correction if the best matching doesn't work

#for example
M$schools2[M$schools2=="University of Michigan-Flint"]<-"University of Michigan-Ann Arbor"

#join the two data frames
M2<-join(M,S,by="schools2",type="left",match="first")
table(schooldata$schools)

#look at the final dataframe
View(M2)

#Export to .csv
write.csv(M2, file="2018-2019 Comm & Media Job Wiki (Scraped).csv")

#Plot Schools
M2$LONGITUD<-as.numeric(M2$LONGITUD)
M2$LATITUDE<-as.numeric(M2$LATITUDE)
mapImageData <- get_googlemap(center = c(-97,40),
                              zoom = 4,
                              #size = c(600, 600),
                              maptype = c("roadmap"))
pdf(5,5,file="2017-JobsMap.pdf")
ggmap(mapImageData,
      extent = "device") + # takes out axes, etc.
  geom_point(aes(x = LONGITUD,
                 y = LATITUDE, color=Type),
             data = M2,
             size = 2,
             pch = 20)
dev.off()

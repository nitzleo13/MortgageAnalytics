setwd( "C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
lar_str <- read.csv( 'loans.csv')
inst_str<-read.csv('institutions.csv')
View(inst_str)
View(lar_str)
install.packages('dplyr')
library(dplyr)
sub_inst<-as.data.frame( inst_str[,c('As_of_Year','Agency_Code','Respondent_ID','Respondent_Name_TS')], drop=false)
View(sub_inst)
Hmda <- merge(lar_str,sub_inst,by=c('As_of_Year','Agency_Code','Respondent_ID'))
View(Hmda)
write.csv2(Hmda,'expanded_hmda.csv')
head(Hmda)
names(Hmda)

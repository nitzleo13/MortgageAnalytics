setwd( "C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
lar_str <- read.csv( 'loans.csv')
inst_str<-read.csv('institutions.csv')
Hmda <- merge(lar_str,inst_str,by=c('As_of_Year','Agency_Code','Respondent_ID'))
write.csv2(Hmda,'expanded_hmda.csv')
#**********************************************************************************
install.packages('sqldf')
library(sqldf)
res<-sqldf('SELECT As_of_Year, State, COUNT(Sequence_Number) AS Loan_Count FROM Hmda GROUP BY As_of_Year, State')
View(res)
write.csv2(res,"result.csv")

#*********************************************************************************

#Plotting Market size with X-axis: state and year and Y-axis:Total
group<-sqldf('SELECT * FROM res GROUP BY State, As_of_Year')
View(group)
library(ggplot2)
year<-as.factor(group$As_of_Year)
p<-ggplot(group,aes(x=State,y=Total))+
  geom_bar(aes(x=State,y=Loan_Count,fill=year), stat="identity",position="dodge")+
  scale_fill_discrete(name="Year", breaks=c(1, 2,3),labels=c("2012", "2013","2014"))+
  xlab("State")+ylab("Loan Count")
require(scales)
p1<-p + scale_y_continuous(labels = comma)
p1 + scale_fill_discrete(guide = guide_legend())


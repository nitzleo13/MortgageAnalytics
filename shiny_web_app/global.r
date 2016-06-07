setwd("C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
# Load required libraries
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(mapproj)
library(plotly)
library(sqldf)
library(grid)

lar_str <- read.csv( 'loans.csv')
inst_str<-read.csv('institutions.csv')
Hmda <- merge(lar_str,inst_str,by=c('As_of_Year','Agency_Code','Respondent_ID'))
write.csv2(Hmda,'expanded_hmda.csv')
#obtaining state data
states<-read.csv('usa-states.csv')
code_state<-as.data.frame(states[,c('State','StateName')])
stategp<- merge(Hmda,code_state,by='State')
View(stategp)
#Finding the total assets of institutions per state and year
res_one<-sqldf('SELECT As_of_Year,Respondent_Name_TS, Respondent_ID, State,StateName, Sum(Loan_Amount_000) AS Total FROM stategp GROUP BY As_of_Year, State, Respondent_ID Order by total DESC')
View(res_one)
#finding total assets of institutions per year
tot_per_id<-sqldf('SELECT As_of_Year ,Respondent_ID ,Respondent_Name_TS , Sum(Loan_Amount_000) AS Total_State_Asset from stategp GROUP BY As_of_Year, Respondent_ID order by Total_State_Asset DESC')
View(tot_per_id)
#finding the contribution of each institution to total share
Shared<- sqldf('Select r.As_of_Year,r.Respondent_Name_TS, r.Respondent_ID, r.State,r.StateName , r.Total, t.Total_State_Asset From res_one  r,tot_per_id t where r.As_of_Year=t.As_of_Year and r.Respondent_ID=t.Respondent_ID Order by r.Respondent_ID DESC') 
View(Shared)
Shared$contribution<-(Shared$Total*100)/Shared$Total_State_Asset
View(Shared)

#**************************************************************************************************************************
#filtering per state FOR THE YEAR 2012
#VIRGINIA
virginia<-sqldf('Select * from Shared where StateName="Virginia" AND As_of_Year="2012" Limit 10')
#District of Columbia
doc<-sqldf('Select * from Shared where StateName="District of Columbia"AND As_of_Year="2012" LIMIT 10 ')
#Maryland
maryland<-sqldf('Select * from Shared where StateName="Maryland" AND As_of_Year="2012" LIMIT 10')
#WestVirginia
westvirginia<-sqldf('Select * from Shared where StateName="West Virginia" AND As_of_Year="2012" LIMIT 10')
#Delaware
delaware<-sqldf('Select * from Shared where StateName="Delaware" AND As_of_Year="2012" LIMIT 10')

#**************************************************************************************************************************
#filtering per state FOR THE YEAR 2013
#VIRGINIA
virginia13<-sqldf('Select * from Shared where StateName="Virginia" AND As_of_Year="2013" Limit 10')
View(virginia13)

#District of Columbia
doc13<-sqldf('Select * from Shared where StateName="District of Columbia"AND As_of_Year="2013" LIMIT 10 ')
#Maryland
maryland13<-sqldf('Select * from Shared where StateName="Maryland" AND As_of_Year="2013" LIMIT 10')
#WestVirginia
westvirginia13<-sqldf('Select * from Shared where StateName="West Virginia" AND As_of_Year="2013" LIMIT 10')
#Delaware
delaware13<-sqldf('Select * from Shared where StateName="Delaware" AND As_of_Year="2013" LIMIT 10')


#**************************************************************************************************************************
#filtering per state FOR THE YEAR 2014
#VIRGINIA
virginia14<-sqldf('Select * from Shared where StateName="Virginia" AND As_of_Year="2014" Limit 10')
View(virginia14)
#District of Columbia
doc14<-sqldf('Select * from Shared where StateName="District of Columbia"AND As_of_Year="2014" LIMIT 10 ')
#Maryland
maryland14<-sqldf('Select * from Shared where StateName="Maryland" AND As_of_Year="2014" LIMIT 10')
#WestVirginia
westvirginia14<-sqldf('Select * from Shared where StateName="West Virginia" AND As_of_Year="2014" LIMIT 10')
#Delaware
delaware14<-sqldf('Select * from Shared where StateName="Delaware" AND As_of_Year="2014" LIMIT 10')

#***********************************************************************************************
library(ggplot2)
#Plots of District of Columbia

doc_plot2012<-ggplot(doc,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='red')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
doc_plot2013<-ggplot(doc13,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='blue')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
doc_plot2014<-ggplot(doc14,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='green')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))

#****************************************************************************************
#Plots of Delaware

del_plot2012<-ggplot(delaware,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='red')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
del_plot2013<-ggplot(delaware13,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='violet')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
del_plot2014<-ggplot(delaware14,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='green')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))

#****************************************************************************************
#Plots of Maryland

mary_plot2012<-ggplot(maryland,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='yellow')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
mary_plot2013<-ggplot(maryland13,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='maroon')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
mary_plot2014<-ggplot(maryland14,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='darkgreen')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))

#****************************************************************************************

#Plots of Virginia

vir_plot2012<-ggplot(virginia,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='violet')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
vir_plot2013<-ggplot(virginia13,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='lightblue')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
vir_plot2014<-ggplot(virginia14,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='brown')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))

#****************************************************************************************

#Plots of West Virginia

wvir_plot2012<-ggplot(westvirginia,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='pink')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
wvir_plot2013<-ggplot(westvirginia13,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='maroon')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
wvir_plot2014<-ggplot(westvirginia14,aes(x=Respondent_Name_TS,y=contribution))+geom_bar(stat="identity",color='black',fill='blue')+ xlab("Responding Institution")+ylab("Share(in %)")+
  ggtitle("Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))


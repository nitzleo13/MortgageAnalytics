install.packages('sqldf')
install.packages('ggplot2')
install.packages('scales')
install.packages('plyr')
install.packages('dplyr')

setwd( "C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
  lar_str <- read.csv( 'loans.csv')
  inst_str<-read.csv('institutions.csv')
  Hmda <- merge(lar_str,inst_str,by=c('As_of_Year','Agency_Code','Respondent_ID'))
  write.csv2(Hmda,'expanded_hmda.csv')
  #**********************************************************************************
  library(sqldf)
  View(Hmda)
  # grouping by year and state we get the total loan per institution 
  res_one<-sqldf('SELECT As_of_Year,Respondent_Name_TS, Respondent_ID, State, Sum(Loan_Amount_000) AS Total FROM Hmda GROUP BY As_of_Year, State, Respondent_ID')
  # grouping by year  we get the total loan per institution to get yearly asset amount of each institution
  res_two<-sqldf('SELECT As_of_Year,Respondent_Name_TS, Respondent_ID, Sum(Loan_Amount_000) AS Total FROM Hmda GROUP BY As_of_Year, Respondent_ID')
  #compute the total asset per year and total over three years
  total_yearly<-sqldf('SELECT As_of_Year,  Sum(Loan_Amount_000) AS TotalAsset FROM Hmda GROUP BY As_of_Year')
  total_yearly
  #total_asset<- sum(Hmda$Loan_Amount_000)
  View(res_one)
  View(res_two)
  #total_asset
  #ordering the records in descending order according to the total lending amount
  dat_2012<- sqldf('Select * from res_two where As_of_Year="2012" Order By Total DESC')
  View(dat_2012)
  merged1<-merge(dat_2012,total_yearly,by='As_of_Year')
  View(merged1)
  
  #*********************************************************************************
  merged1$share<-(merged1$Total*100)/merged1$TotalAsset
  View(merged1)
  #filter the top 10 institutions
  filtered1<- sqldf('SELECT Respondent_Name_TS, share, As_of_Year from merged1 Order By share LIMIT 10')
  library(ggplot2)
  library(scales)
  
  plot<-ggplot(filtered1,aes(x=Respondent_Name_TS,y=share,fill=As_of_Year))+geom_bar(stat="identity",color='yellow',fill='red')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle("Institutions-wise Market share in 2012 ")
  plot<-plot + theme(axis.text.x =
                         element_text(size  = 10,
                                      angle = 60,
                                      hjust = 1,
                                      vjust = 1))
  plot
  
  #**************************************************************************************
  #2013 analysis
  
  dat_2013<- sqldf('Select * from res_two where As_of_Year="2013" Order By Total DESC')
  View(dat_2013)
  merged2<-merge(dat_2013,total_yearly,by='As_of_Year')
  View(merged2)
  
  #*********************************************************************************
  merged2$share<-(merged2$Total*100)/merged2$TotalAsset
  View(merged2)
  #filter the top 10 institutions
  filtered2<- sqldf('SELECT Respondent_Name_TS, share, As_of_Year from merged2 Order By share LIMIT 10')
  library(scales)
  
  
  plot2<-ggplot(filtered2,aes(x=Respondent_Name_TS,y=share,fill=As_of_Year))+geom_bar(stat="identity",color='green',fill='darkblue')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle("Institutions-wise Market share in 2013 ")
  #plot2<-plot2 + scale_x_discrete(labels = abbreviate)
  plot2<-plot2 + theme(axis.text.x =
                               element_text(size  = 10,
                                            angle = 60,
                                            hjust = 1,
                                            vjust = 1))
  plot2
  
  #**************************************************************************************
  #2014 analysis
  
  dat_2014<- sqldf('Select * from res_two where As_of_Year="2014" Order By Total DESC')
  View(dat_2014)
  merged3<-merge(dat_2014,total_yearly,by='As_of_Year')
  View(merged3)
  
  #*********************************************************************************
  merged3$share<-(merged3$Total*100)/merged3$TotalAsset
  View(merged3)
  #filter the top 10 institutions
  filtered3<- sqldf('SELECT Respondent_Name_TS, share, As_of_Year from merged3 Order By share LIMIT 10')
  library(scales)
  
  plot3<-ggplot(filtered3,aes(x=Respondent_Name_TS,y=share,fill=As_of_Year))+geom_bar(stat="identity",color='red',fill='blue')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle("Institutions-wise Market share in 2014 ")
  plot3<-plot3 + theme(axis.text.x =
                              element_text(size  = 10,
                                           angle = 70,
                                           hjust = 1,
                                           vjust = 1))
  plot3
  
  #***************************************************************************************
  tab<-rbind(filtered3,filtered2,filtered1)
  View(tab)
  tab_filtered12<-sqldf('SELECT  Respondent_Name_TS, share, As_of_Year from tab where As_of_Year="2012"  ')
  tab_filtered13<-sqldf('SELECT  Respondent_Name_TS, share, As_of_Year from tab where As_of_Year="2013"')
  tab_filtered14<-sqldf('SELECT  Respondent_Name_TS, share, As_of_Year from tab where As_of_Year="2014"  ')
  
  plot12<-ggplot(tab_filtered12,aes(x=Respondent_Name_TS,y=share))+geom_bar(stat="identity",color='black',fill='maroon')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle(" Market share-2012 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
  plot13<-ggplot(tab_filtered12,aes(x=Respondent_Name_TS,y=share))+geom_bar(stat="identity",color='black',fill='blue')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle(" Market share-2013 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
  plot14<-ggplot(tab_filtered12,aes(x=Respondent_Name_TS,y=share))+geom_bar(stat="identity",color='black',fill='green')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle(" Market share-2014 ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
  source("fn.R",local = TRUE)
  
  multiplot(plot12, plot13, plot14, cols=3,layout=NULL)
  #**************************************************************************************#
  #loan purpose trends
  
  library(plyr)
  library(dplyr)
  
  loan_count_ref<-sqldf('SELECT As_of_Year, COUNT(Sequence_Number) AS Count FROM Hmda Where Loan_Purpose_Description="Refinance" GROUP BY As_of_Year')
  loan_count_pur<-sqldf('SELECT As_of_Year, COUNT(Sequence_Number) AS Count FROM Hmda Where Loan_Purpose_Description="Purchase" GROUP BY As_of_Year')
  View(loan_count_ref)
  View(loan_count_pur)
  p1<-ggplot(loan_count_ref,aes(x=As_of_Year,y=Count))+geom_bar(stat="identity",color='black',fill='maroon')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle(" Refinance ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
  p2<-ggplot(loan_count_pur,aes(x=As_of_Year,y=Count))+geom_bar(stat="identity",color='black',fill='blue')+ xlab("Responding Institution")+ylab("Share(in %)")+
    ggtitle(" Purchase ")+theme(axis.text.x =element_text(size  = 10, angle = 60, hjust = 1, vjust = 1))
  source("fn.R",local = TRUE)
  multiplot(p2, p1, cols=2,layout=NULL)

setwd( "C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
hmda_to_json2<- function(data, states, conv_conf)
{
  if(missing(states)){
    # filtering the data based on flag,
    gp<- merge(data,conv_conf,by='Conventional_Conforming_Flag')
    res<-sqldf('SELECT  * FROM data Order BY Conventional_Conforming_Flag')
    res_stat<-sqldf('SELECT  Conventional_Conforming_Flag, Loan_Type_Description, Loan_Amount_000, Conforming_Status, Conventional_Status FROM res Order BY Conventional_Status')
    #View(res_stat1)
    res_stat2<-sqldf('SELECT Conventional_Conforming_Flag, Loan_Type_Description, Loan_Amount_000, Conforming_Status, Conventional_Status FROM res_stat Order BY Conforming_Status')
    #View(res_stat2)
    res_seg<-sqldf('SELECT Conventional_Conforming_Flag, Loan_Type_Description,Count(Loan_Type_Description) as Count, Sum(Loan_Amount_000) as Total FROM res_stat2 Group BY Loan_Type_Description')
    write.csv2(res_seg)
    
  }
}
hmda_to_json<- function(data, states, conv_conf)
{
  
  if (missing(conv_conf)){
    #merge data and states on state_code 
    stategp<- merge(data,states,by='State')
    View(stategp)
    #order the records by state name and code or group by state name or code
    res<-sqldf('SELECT  * FROM stategp Order BY State')
    View(res)
    res_pdts<-sqldf('Select State,Loan_Type_Description,Count(Loan_Type_Description) as Count, Sum(Loan_Amount_000) as Total from res Group By State,Loan_Type_Description')
    #write state based grouping to disk
    write.csv2(res_pdts)
  } else{
    #merge data, state and conv_conf data frames
    data<- merge(data,states,by='State')
#typegp<-merge(data,conv_conf, by='Conventional_Conforming_Flag')
    #filtering and grouping data sets to produce product segments observed per state with respect to loan type status information
    stategp<-sqldf('SELECT  * FROM data Order BY State')
    res<-sqldf('SELECT  * FROM stategp Order BY Conventional_Conforming_Flag')
    res_stat<-sqldf('SELECT  State,Conventional_Conforming_Flag, Loan_Type_Description, Loan_Amount_000, Conforming_Status, Conventional_Status FROM res Order BY Conventional_Status')
    View(res_stat)
    res_stat2<-sqldf('SELECT State,Conventional_Conforming_Flag, Loan_Type_Description, Loan_Amount_000, Conforming_Status, Conventional_Status FROM res_stat Order BY Conforming_Status')
    View(res_stat2)
    res_seg<-sqldf('SELECT State, Conventional_Conforming_Flag, Loan_Type_Description,Count(Loan_Type_Description) as Count, Sum(Loan_Amount_000) as Total FROM res_stat2 Group BY State, Loan_Type_Description')
    write.csv2(res_seg)
  }
  
}
library(sqldf)
lar_str <- read.csv( 'loans.csv')
View(lar_str)
states<-read.csv('usa-states.csv')
View(states)
code_state<-as.data.frame(states[,c('State','StateName')])
View(code_state)
data<- read.csv2("expanded_hmda.csv")
conv_conf<- sqldf('Select Distinct conventional_conforming_flag, Conforming_Status, Conventional_Status from lar_str')
conv_conf
#Case1: missing loan type data
hmda_to_json(lar_str,code_state)
#******************************************************************************************************************************************
#Case 2: missing states data
hmda_to_json2(data,conv_conf)

#*******************************************************************************************************************************************
#Case2:no missing data
hmda_to_json(data,states,conv_conf)


# The other optional parameters that allow division of the loan data per state 
#according to product segments are by using conventional and conforming status,
#loan type,loan purpose, we can categorise based on the income groups i.e 
#cluster loan type according to the income they belong to. These are the optional parameter for product based
#segmentation
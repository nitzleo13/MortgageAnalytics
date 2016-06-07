setwd( "C:/Users/KarthikNithya/Downloads/data-challenge 2/data-challenge" )
#function to read and merge the data followed by returning object pointer
hmda_init <- function() 
{
 lar_str <- read.csv( 'loans.csv')
 inst_str<-read.csv('institutions.csv')
 sub_inst<-as.data.frame( inst_str[,c('As_of_Year','Agency_Code','Respondent_ID','Respondent_Name_TS')], drop=false)
 Hmda <- merge(lar_str,sub_inst,by=c('As_of_Year','Agency_Code','Respondent_ID'))
 return (Hmda)
}
expand<-hmda_init() 
write.csv(expand, file = "ExpandedDataSet.csv",row.names=FALSE, na="")

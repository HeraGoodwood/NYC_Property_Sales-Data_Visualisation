Monthly<-Monthly2014
Monthly$Sales2015<-Monthly2015$Total_Sales
Monthly$Sales2016<-Monthly2016$Total_Sales
Monthly$Sales2017<-Monthly2017$Total_Sales
Monthly$Sales2018<-Monthly2018$Total_Sales
names(Monthly)[names(Monthly) == "Total_Sales"] <- "Sales2014"
Monthly %>% select(Sales2014,Sales2015,Sales2016,Sales2017,Sales2018,Month) %>% 
  gather("key", "value", Sales2014,Sales2015,Sales2016,Sales2017,Sales2018) %>%
  hchart(type='line', hcaes(x='Month', y='value', group='key'))

Yearly<-read.csv("C:/Users/User/Documents/R/Data Visual/D/Yearly.csv")

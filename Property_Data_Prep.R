#LOAD DATA(Change the data accordingly)
df<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2018_NYC_Property_Sales__10172019.csv")

#-------------
#REMOVE DOLLAR SYMBOL AND COMMAS
df$SALE.PRICE.= gsub("[\\$,]", "", df$SALE.PRICE.)
df$SALE.PRICE.<-sub("-",0,df$SALE.PRICE.)
df$COMMERCIAL.UNITS.<-sub("-",0,df$COMMERCIAL.UNITS.)
df$RESIDENTIAL.UNITS.= gsub("[\\,]", "", df$RESIDENTIAL.UNITS.)
df$COMMERCIAL.UNITS.= gsub("[\\,]", "", df$COMMERCIAL.UNITS.)
df$TOTAL.UNITS.= gsub("[\\,]", "", df$TOTAL.UNITS.)
df$LAND.SQUARE.FEET.= gsub("[\\,]", "", df$LAND.SQUARE.FEET.)
df$GROSS.SQUARE.FEET.= gsub("[\\,]", "", df$GROSS.SQUARE.FEET.)
head(df)
#------------------
#DATE CORRECTION
str(df$SALE.DATE.)
df$date2<-as.Date(df$SALE.DATE.,format = "%m/%d/%y")
df$date3<-as.Date(as.character(df$SALE.DATE.), '%m/%d/%y')
df$date<-paste(df$date2,df$date3)
head(df$date)
df$date= gsub("[\\NA,]", "", df$date)
df$date<-as.Date(df$date)
str(df$date)
df$SALE.DATE.<-df$date2
head(df$date)
#-----------------
#REMOVE EXCESSIVE COLUMN WITH UNWANTED COLUMNS
library(dplyr)
df = select(df, -10:-11, -23:-25)
#-----------------
#Standardize
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "1A"] <- "1"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "1B"] <- "1"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "1C"] <- "1"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "1D"] <- "1"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "2A"] <- "2"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "2B"] <- "2"
df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df$TAX.CLASS.AS.OF.FINAL.ROLL.18.19 == "2C"] <- "2"
head(df)
#SAVE THE MODIFIED
write.csv(df, file = "2016_NYC_Property_Sales__10212019-Modified.csv")
#------------------------
#SEPERATE YEAR AND MONTH FOR LATER USE
df2014 <- df2014 %>% 
  mutate(year=year(SALE.DATE.))
df2014$yearmonth<-paste(df2014$month,df2014$year)

df2015 <- df2015 %>% 
  mutate(year=year(SALE.DATE.))
df2015$yearmonth<-paste(df2015$month,df2015$year)

df2016 <- df2016 %>% 
  mutate(year=year(SALE.DATE.))
df2016$yearmonth<-paste(df2016$month,df2016$year)

df2017 <- df2017 %>% 
  mutate(year=year(SALE.DATE.))
df2017$yearmonth<-paste(df2017$month,df2017$year)

df2018 <- df2018 %>% 
  mutate(year=year(SALE.DATE.))
df2018$yearmonth<-paste(df2018$month,df2018$year)

#GROUP DATA YEAR-MONTH
Yearly2014<-df2014 %>%
  group_by(df2014$yearmonth) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Yearly2014)[names(Yearly2014) == "df2014$yearmonth"] <- "YearMonth"
Yearly2014$YearMonth<-as.factor(Yearly2014$YearMonth)

Yearly2015<-df2015 %>%
  group_by(df2015$yearmonth) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Yearly2015)[names(Yearly2015) == "df2015$yearmonth"] <- "YearMonth"
Yearly2015$YearMonth<-as.factor(Yearly2015$YearMonth)

Yearly2016<-df2016 %>%
  group_by(df2016$yearmonth) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Yearly2016)[names(Yearly2016) == "df2016$yearmonth"] <- "YearMonth"
Yearly2016$YearMonth<-as.factor(Yearly2016$YearMonth)

Yearly2017<-df2017 %>%
  group_by(df2017$yearmonth) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Yearly2017)[names(Yearly2017) == "df2017$yearmonth"] <- "YearMonth"
Yearly2017$YearMonth<-as.factor(Yearly2017$YearMonth)

Yearly2018<-df2018 %>%
  group_by(df2018$yearmonth) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Yearly2018)[names(Yearly2018) == "df2018$yearmonth"] <- "YearMonth"
Yearly2018$YearMonth<-as.factor(Yearly2018$YearMonth)

Yearly<-rbind(Yearly2014,Yearly2015)
Yearly<-rbind(Yearly,Yearly2016)
Yearly<-rbind(Yearly,Yearly2017)
Yearly<-rbind(Yearly,Yearly2018)

write.csv(Yearly, file = "Yearly.csv")

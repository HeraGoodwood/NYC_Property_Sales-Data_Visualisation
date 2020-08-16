#IMPORT FILES
df2017<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2017_NYC_Property_Sales__10172019-Modified.csv")

#FOR 2017---------------------------------------------------------
#FILTER OUT MONTH AND WEEK 
df2017 <- df2017 %>% 
  mutate(month=month(SALE.DATE.),week = wday(SALE.DATE.,label = TRUE))

#GROUP DATA MONTHLY
Monthly2017<-df2017 %>%
  group_by(df2017$month) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Monthly2017)[names(Monthly2017) == "df2017$month"] <- "Month"
Monthly2017$Month<-as.factor(Monthly2017$Month)

#SUMMARISE BY MONTH and RENAME AUTO NAMING AND CHANGING TO CATEGORICAL VALUE
sales_by_week2017 <- df2017 %>% group_by(week,month) %>%
  summarise(Total_Sales=sum(SALE.PRICE.)) %>% ungroup()

Borough2017<-df2017 %>%
  group_by(df2017$ï..BOROUGH.NAME) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Borough2017)[names(Borough2017) == "df2017$ï..BOROUGH.NAME"] <- "Borough"
Borough2017$Borough<-as.factor(Borough2017$Borough)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$TAX.CLASS.AS.OF.FINAL.ROLL.17.18[df2017$TAX.CLASS.AS.OF.FINAL.ROLL.17.18==0] <- NA

TaxPres2017<-df2017[!is.na(df2017$TAX.CLASS.AS.OF.FINAL.ROLL.17.18), ] %>%
  group_by(df2017[!is.na(df2017$TAX.CLASS.AS.OF.FINAL.ROLL.17.18), ]$TAX.CLASS.AS.OF.FINAL.ROLL.17.18) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(TaxPres2017)[names(TaxPres2017) == "df2017[!is.na(df2017$TAX.CLASS.AS.OF.FINAL.ROLL.17.18), ]$TAX.CLASS.AS.OF.FINAL.ROLL.17.18"] <- "Tax_At_Present"
TaxPres2017$Tax_At_Present<-as.factor(TaxPres2017$Tax_At_Present)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$YEAR.BUILT.[df2017$YEAR.BUILT.==0] <- NA

Year2017<-df2017[!is.na(df2017$YEAR.BUILT.), ] %>%
  group_by(df2017[!is.na(df2017$YEAR.BUILT.), ]$YEAR.BUILT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Year2017)[names(Year2017) == "df2017[!is.na(df2017$YEAR.BUILT.), ]$YEAR.BUILT."] <- "Year_Built"
Year2017$Year_Built<-as.factor(Year2017$Year_Built)

#-------------------------------------------------------------------
Ease2017<-df2017 %>%
  group_by(df2017$EASE.MENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Ease2017)[names(Ease2017) == "df2017$EASE.MENT."] <- "Easement"
Ease2017$Easement<-as.factor(Ease2017$Easement)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$ZIP.CODE.[df2017$ZIP.CODE.==0] <- NA

Zip2017<-df2017[!is.na(df2017$ZIP.CODE.), ] %>%
  group_by(df2017[!is.na(df2017$ZIP.CODE.), ]$ZIP.CODE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Zip2017)[names(Zip2017) == "df2017[!is.na(df2017$ZIP.CODE.), ]$ZIP.CODE."] <- "Zipcode"
Zip2017$Zipcode<-as.factor(Zip2017$Zipcode)

#-------------------------------------------------------------------
Nei2017<-df2017 %>%
  group_by(df2017$NEIGHBORHOOD.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Nei2017<-Nei2017[1:10,]
Nei2017<-as.data.frame(Nei2017)
names(Nei2017)[names(Nei2017) == "df2017$NEIGHBORHOOD."] <- "Neighborhood"

#-------------------------------------------------------------------
Bui2017<-df2017 %>%
  group_by(df2017$BUILDING.CLASS.CATEGORY.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Bui2017<-Bui2017[1:10,]
Bui2017<-as.data.frame(Bui2017)
names(Bui2017)[names(Bui2017) == "df2017$BUILDING.CLASS.CATEGORY."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$TAX.CLASS.AT.TIME.OF.SALE.<-as.factor(df2017$TAX.CLASS.AT.TIME.OF.SALE.)
df2017$TAX.CLASS.AT.TIME.OF.SALE.[df2017$TAX.CLASS.AT.TIME.OF.SALE.==0] <- NA

Tax2017<-df2017[!is.na(df2017$TAX.CLASS.AT.TIME.OF.SALE.), ] %>%
  group_by(df2017[!is.na(df2017$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

#SORTING OUT TOP 10 OCCURRENCE
Tax2017<-as.data.frame(Tax2017)
names(Tax2017)[names(Tax2017) == "df2017[!is.na(df2017$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE."] <- "Tax_Class_Category"

#-------------------------------------------------------------------
BuiSales2017<-df2017 %>%
  group_by(df2017$BUILDING.CLASS.AS.OF.FINAL.ROLL.17.18) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
BuiSales2017<-BuiSales2017[1:10,]
BuiSales2017<-as.data.frame(BuiSales2017)
names(BuiSales2017)[names(BuiSales2017) == "df2017$BUILDING.CLASS.AS.OF.FINAL.ROLL.17.18"] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$RESIDENTIAL.UNITS.<-as.factor(df2017$RESIDENTIAL.UNITS.)
df2017$RESIDENTIAL.UNITS.[df2017$RESIDENTIAL.UNITS.==0] <- NA

Res2017<-df2017[!is.na(df2017$RESIDENTIAL.UNITS.), ] %>%
  group_by(df2017[!is.na(df2017$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Res2017<-Res2017[1:10,]
Res2017<-as.data.frame(Res2017)
names(Res2017)[names(Res2017) == "df2017[!is.na(df2017$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS."] <- "Residental_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$COMMERCIAL.UNITS.<-as.factor(df2017$COMMERCIAL.UNITS.)
df2017$COMMERCIAL.UNITS.[df2017$COMMERCIAL.UNITS.==0] <- NA

Com2017<-df2017[!is.na(df2017$COMMERCIAL.UNITS.), ] %>%
  group_by(df2017[!is.na(df2017$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Com2017<-Com2017[1:10,]
Com2017<-as.data.frame(Com2017)
names(Com2017)[names(Com2017) == "df2017[!is.na(df2017$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS."] <- "Commercial_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$TOTAL.UNITS.<-as.factor(df2017$TOTAL.UNITS.)
df2017$TOTAL.UNITS.[df2017$TOTAL.UNITS.==0] <- NA

Tot2017<-df2017[!is.na(df2017$TOTAL.UNITS.), ] %>%
  group_by(df2017[!is.na(df2017$TOTAL.UNITS.), ]$TOTAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Tot2017<-Tot2017[1:10,]
Tot2017<-as.data.frame(Tot2017)
names(Tot2017)[names(Tot2017) == "df2017[!is.na(df2017$TOTAL.UNITS.), ]$TOTAL.UNITS."] <- "Total_units"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$LAND.SQUARE.FEET.<-as.factor(df2017$LAND.SQUARE.FEET.)
df2017$LAND.SQUARE.FEET.[df2017$LAND.SQUARE.FEET.==0] <- NA

Lan2017<-df2017[!is.na(df2017$LAND.SQUARE.FEET.), ] %>%
  group_by(df2017[!is.na(df2017$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Lan2017<-Lan2017[1:10,]
Lan2017<-as.data.frame(Lan2017)
names(Lan2017)[names(Lan2017) == "df2017[!is.na(df2017$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET."] <- "Land_Square_Feet"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2017$GROSS.SQUARE.FEET.<-as.factor(df2017$GROSS.SQUARE.FEET.)
df2017$GROSS.SQUARE.FEET.[df2017$GROSS.SQUARE.FEET.==0] <- NA

Gro2017<-df2017[!is.na(df2017$GROSS.SQUARE.FEET.), ] %>%
  group_by(df2017[!is.na(df2017$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Gro2017<-Gro2017[1:10,]
Gro2017<-as.data.frame(Gro2017)
names(Gro2017)[names(Gro2017) == "df2017[!is.na(df2017$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET."] <- "Gross_Square_Feet"

#CHECK FOR PROPERTY TRANSFER WITHOUT CASH WHICH IS ZERO SALES PRICE
Nul2017<-length(which(df2017$SALE.PRICE.==0))
Total2017<-length(df2017$SALE.PRICE.)

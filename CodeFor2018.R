#IMPORT FILES
df2018<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2018_NYC_Property_Sales__10172019-Modified.csv")

#FOR 2018---------------------------------------------------------
#FILTER OUT MONTH AND WEEK 
df2018 <- df2018 %>% 
  mutate(month=month(SALE.DATE.),week = wday(SALE.DATE.,label = TRUE))

#GROUP DATA MONTHLY
Monthly2018<-df2018 %>%
  group_by(df2018$month) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Monthly2018)[names(Monthly2018) == "df2018$month"] <- "Month"
Monthly2018$Month<-as.factor(Monthly2018$Month)

#SUMMARISE BY MONTH and RENAME AUTO NAMING AND CHANGING TO CATEGORICAL VALUE
sales_by_week2018 <- df2018 %>% group_by(week,month) %>%
  summarise(Total_Sales=sum(SALE.PRICE.)) %>% ungroup()

Borough2018<-df2018 %>%
  group_by(df2018$ï..BOROUGH.NAME) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Borough2018)[names(Borough2018) == "df2018$ï..BOROUGH.NAME"] <- "Borough"
Borough2018$Borough<-as.factor(Borough2018$Borough)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$TAX.CLASS.AS.OF.FINAL.ROLL.18.19[df2018$TAX.CLASS.AS.OF.FINAL.ROLL.18.19==0] <- NA

TaxPres2018<-df2018[!is.na(df2018$TAX.CLASS.AS.OF.FINAL.ROLL.18.19), ] %>%
  group_by(df2018[!is.na(df2018$TAX.CLASS.AS.OF.FINAL.ROLL.18.19), ]$TAX.CLASS.AS.OF.FINAL.ROLL.18.19) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(TaxPres2018)[names(TaxPres2018) == "df2018[!is.na(df2018$TAX.CLASS.AS.OF.FINAL.ROLL.18.19), ]$TAX.CLASS.AS.OF.FINAL.ROLL.18.19"] <- "Tax_At_Present"
TaxPres2018$Tax_At_Present<-as.factor(TaxPres2018$Tax_At_Present)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$YEAR.BUILT.[df2018$YEAR.BUILT.==0] <- NA

Year2018<-df2018[!is.na(df2018$YEAR.BUILT.), ] %>%
  group_by(df2018[!is.na(df2018$YEAR.BUILT.), ]$YEAR.BUILT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Year2018)[names(Year2018) == "df2018[!is.na(df2018$YEAR.BUILT.), ]$YEAR.BUILT."] <- "Year_Built"
Year2018$Year_Built<-as.factor(Year2018$Year_Built)

#-------------------------------------------------------------------
Ease2018<-df2018 %>%
  group_by(df2018$EASE.MENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Ease2018)[names(Ease2018) == "df2018$EASE.MENT."] <- "Easement"
Ease2018$Easement<-as.factor(Ease2018$Easement)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$ZIP.CODE.[df2018$ZIP.CODE.==0] <- NA

Zip2018<-df2018[!is.na(df2018$ZIP.CODE.), ] %>%
  group_by(df2018[!is.na(df2018$ZIP.CODE.), ]$ZIP.CODE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Zip2018)[names(Zip2018) == "df2018[!is.na(df2018$ZIP.CODE.), ]$ZIP.CODE."] <- "Zipcode"
Zip2018$Zipcode<-as.factor(Zip2018$Zipcode)

#-------------------------------------------------------------------
Nei2018<-df2018 %>%
  group_by(df2018$NEIGHBORHOOD.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Nei2018<-Nei2018[1:10,]
Nei2018<-as.data.frame(Nei2018)
names(Nei2018)[names(Nei2018) == "df2018$NEIGHBORHOOD."] <- "Neighborhood"

#-------------------------------------------------------------------
Bui2018<-df2018 %>%
  group_by(df2018$BUILDING.CLASS.CATEGORY.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Bui2018<-Bui2018[1:10,]
Bui2018<-as.data.frame(Bui2018)
names(Bui2018)[names(Bui2018) == "df2018$BUILDING.CLASS.CATEGORY."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$TAX.CLASS.AT.TIME.OF.SALE.<-as.factor(df2018$TAX.CLASS.AT.TIME.OF.SALE.)
df2018$TAX.CLASS.AT.TIME.OF.SALE.[df2018$TAX.CLASS.AT.TIME.OF.SALE.==0] <- NA

Tax2018<-df2018[!is.na(df2018$TAX.CLASS.AT.TIME.OF.SALE.), ] %>%
  group_by(df2018[!is.na(df2018$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

#SORTING OUT TOP 10 OCCURRENCE
Tax2018<-as.data.frame(Tax2018)
names(Tax2018)[names(Tax2018) == "df2018[!is.na(df2018$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE."] <- "Tax_Class_Category"

#-------------------------------------------------------------------
BuiSales2018<-df2018 %>%
  group_by(df2018$BUILDING.CLASS.AS.OF.FINAL.ROLL.18.19) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
BuiSales2018<-BuiSales2018[1:10,]
BuiSales2018<-as.data.frame(BuiSales2018)
names(BuiSales2018)[names(BuiSales2018) == "df2018$BUILDING.CLASS.AS.OF.FINAL.ROLL.18.19"] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$RESIDENTIAL.UNITS.<-as.factor(df2018$RESIDENTIAL.UNITS.)
df2018$RESIDENTIAL.UNITS.[df2018$RESIDENTIAL.UNITS.==0] <- NA

Res2018<-df2018[!is.na(df2018$RESIDENTIAL.UNITS.), ] %>%
  group_by(df2018[!is.na(df2018$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Res2018<-Res2018[1:10,]
Res2018<-as.data.frame(Res2018)
names(Res2018)[names(Res2018) == "df2018[!is.na(df2018$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS."] <- "Residental_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$COMMERCIAL.UNITS.<-as.factor(df2018$COMMERCIAL.UNITS.)
df2018$COMMERCIAL.UNITS.[df2018$COMMERCIAL.UNITS.==0] <- NA

Com2018<-df2018[!is.na(df2018$COMMERCIAL.UNITS.), ] %>%
  group_by(df2018[!is.na(df2018$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Com2018<-Com2018[1:10,]
Com2018<-as.data.frame(Com2018)
names(Com2018)[names(Com2018) == "df2018[!is.na(df2018$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS."] <- "Commercial_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$TOTAL.UNITS.<-as.factor(df2018$TOTAL.UNITS.)
df2018$TOTAL.UNITS.[df2018$TOTAL.UNITS.==0] <- NA

Tot2018<-df2018[!is.na(df2018$TOTAL.UNITS.), ] %>%
  group_by(df2018[!is.na(df2018$TOTAL.UNITS.), ]$TOTAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Tot2018<-Tot2018[1:10,]
Tot2018<-as.data.frame(Tot2018)
names(Tot2018)[names(Tot2018) == "df2018[!is.na(df2018$TOTAL.UNITS.), ]$TOTAL.UNITS."] <- "Total_units"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$LAND.SQUARE.FEET.<-as.factor(df2018$LAND.SQUARE.FEET.)
df2018$LAND.SQUARE.FEET.[df2018$LAND.SQUARE.FEET.==0] <- NA

Lan2018<-df2018[!is.na(df2018$LAND.SQUARE.FEET.), ] %>%
  group_by(df2018[!is.na(df2018$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Lan2018<-Lan2018[1:10,]
Lan2018<-as.data.frame(Lan2018)
names(Lan2018)[names(Lan2018) == "df2018[!is.na(df2018$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET."] <- "Land_Square_Feet"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2018$GROSS.SQUARE.FEET.<-as.factor(df2018$GROSS.SQUARE.FEET.)
df2018$GROSS.SQUARE.FEET.[df2018$GROSS.SQUARE.FEET.==0] <- NA

Gro2018<-df2018[!is.na(df2018$GROSS.SQUARE.FEET.), ] %>%
  group_by(df2018[!is.na(df2018$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Gro2018<-Gro2018[1:10,]
Gro2018<-as.data.frame(Gro2018)
names(Gro2018)[names(Gro2018) == "df2018[!is.na(df2018$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET."] <- "Gross_Square_Feet"

#CHECK FOR PROPERTY TRANSFER WITHOUT CASH WHICH IS ZERO SALES PRICE
Nul2018<-length(which(df2018$SALE.PRICE.==0))
Total2018<-length(df2018$SALE.PRICE.)

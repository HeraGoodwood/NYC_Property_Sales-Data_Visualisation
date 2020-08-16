#IMPORT FILES
df2014<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2014_NYC_Property_Sales__10212019-Modified.csv")

#FOR 2014---------------------------------------------------------
#FILTER OUT MONTH AND WEEK 
df2014 <- df2014 %>% 
  mutate(month=month(SALE.DATE.),week = wday(SALE.DATE.,label = TRUE))

#GROUP DATA MONTHLY
Monthly2014<-df2014 %>%
  group_by(df2014$month) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Monthly2014)[names(Monthly2014) == "df2014$month"] <- "Month"
Monthly2014$Month<-as.factor(Monthly2014$Month)

#SUMMARISE BY MONTH and RENAME AUTO NAMING AND CHANGING TO CATEGORICAL VALUE
sales_by_week2014 <- df2014 %>% group_by(week,month) %>%
  summarise(Total_Sales=sum(SALE.PRICE.)) %>% ungroup()

Borough2014<-df2014 %>%
  group_by(df2014$ï..BOROUGH.NAME) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Borough2014)[names(Borough2014) == "df2014$ï..BOROUGH.NAME"] <- "Borough"
Borough2014$Borough<-as.factor(Borough2014$Borough)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$TAX.CLASS.AT.PRESENT.[df2014$TAX.CLASS.AT.PRESENT.==0] <- NA

TaxPres2014<-df2014[!is.na(df2014$TAX.CLASS.AT.PRESENT.), ] %>%
  group_by(df2014[!is.na(df2014$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(TaxPres2014)[names(TaxPres2014) == "df2014[!is.na(df2014$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT."] <- "Tax_At_Present"
TaxPres2014$Tax_At_Present<-as.factor(TaxPres2014$Tax_At_Present)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$YEAR.BUILT.[df2014$YEAR.BUILT.==0] <- NA

Year2014<-df2014[!is.na(df2014$YEAR.BUILT.), ] %>%
  group_by(df2014[!is.na(df2014$YEAR.BUILT.), ]$YEAR.BUILT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Year2014)[names(Year2014) == "df2014[!is.na(df2014$YEAR.BUILT.), ]$YEAR.BUILT."] <- "Year_Built"
Year2014$Year_Built<-as.factor(Year2014$Year_Built)

#-------------------------------------------------------------------
Ease2014<-df2014 %>%
  group_by(df2014$EASE.MENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Ease2014)[names(Ease2014) == "df2014$EASE.MENT."] <- "Easement"
Ease2014$Easement<-as.factor(Ease2014$Easement)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$ZIP.CODE.[df2014$ZIP.CODE.==0] <- NA

Zip2014<-df2014[!is.na(df2014$ZIP.CODE.), ] %>%
  group_by(df2014[!is.na(df2014$ZIP.CODE.), ]$ZIP.CODE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Zip2014)[names(Zip2014) == "df2014[!is.na(df2014$ZIP.CODE.), ]$ZIP.CODE."] <- "Zipcode"
Zip2014$Zipcode<-as.factor(Zip2014$Zipcode)

#-------------------------------------------------------------------
Nei2014<-df2014 %>%
  group_by(df2014$NEIGHBORHOOD.) %>%
    summarise(Total_Sales = sum(SALE.PRICE.)) %>%
      arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Nei2014<-Nei2014[1:10,]
Nei2014<-as.data.frame(Nei2014)
names(Nei2014)[names(Nei2014) == "df2014$NEIGHBORHOOD."] <- "Neighborhood"

#-------------------------------------------------------------------
Bui2014<-df2014 %>%
  group_by(df2014$BUILDING.CLASS.CATEGORY.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Bui2014<-Bui2014[1:10,]
Bui2014<-as.data.frame(Bui2014)
names(Bui2014)[names(Bui2014) == "df2014$BUILDING.CLASS.CATEGORY."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$TAX.CLASS.AT.TIME.OF.SALE.<-as.factor(df2014$TAX.CLASS.AT.TIME.OF.SALE.)
df2014$TAX.CLASS.AT.TIME.OF.SALE.[df2014$TAX.CLASS.AT.TIME.OF.SALE.==0] <- NA

Tax2014<-df2014[!is.na(df2014$TAX.CLASS.AT.TIME.OF.SALE.), ] %>%
  group_by(df2014[!is.na(df2014$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

#SORTING OUT TOP 10 OCCURRENCE
Tax2014<-as.data.frame(Tax2014)
names(Tax2014)[names(Tax2014) == "df2014[!is.na(df2014$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE."] <- "Tax_Class_Category"

#-------------------------------------------------------------------
BuiSales2014<-df2014 %>%
  group_by(df2014$BUILDING.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
BuiSales2014<-BuiSales2014[1:10,]
BuiSales2014<-as.data.frame(BuiSales2014)
names(BuiSales2014)[names(BuiSales2014) == "df2014$BUILDING.CLASS.AT.PRESENT."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$RESIDENTIAL.UNITS.<-as.factor(df2014$RESIDENTIAL.UNITS.)
df2014$RESIDENTIAL.UNITS.[df2014$RESIDENTIAL.UNITS.==0] <- NA

Res2014<-df2014[!is.na(df2014$RESIDENTIAL.UNITS.), ] %>%
  group_by(df2014[!is.na(df2014$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Res2014<-Res2014[1:10,]
Res2014<-as.data.frame(Res2014)
names(Res2014)[names(Res2014) == "df2014[!is.na(df2014$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS."] <- "Residental_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$COMMERCIAL.UNITS.<-as.factor(df2014$COMMERCIAL.UNITS.)
df2014$COMMERCIAL.UNITS.[df2014$COMMERCIAL.UNITS.==0] <- NA

Com2014<-df2014[!is.na(df2014$COMMERCIAL.UNITS.), ] %>%
  group_by(df2014[!is.na(df2014$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Com2014<-Com2014[1:10,]
Com2014<-as.data.frame(Com2014)
names(Com2014)[names(Com2014) == "df2014[!is.na(df2014$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS."] <- "Commercial_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$TOTAL.UNITS.<-as.factor(df2014$TOTAL.UNITS.)
df2014$TOTAL.UNITS.[df2014$TOTAL.UNITS.==0] <- NA

Tot2014<-df2014[!is.na(df2014$TOTAL.UNITS.), ] %>%
  group_by(df2014[!is.na(df2014$TOTAL.UNITS.), ]$TOTAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Tot2014<-Tot2014[1:10,]
Tot2014<-as.data.frame(Tot2014)
names(Tot2014)[names(Tot2014) == "df2014[!is.na(df2014$TOTAL.UNITS.), ]$TOTAL.UNITS."] <- "Total_units"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$LAND.SQUARE.FEET.<-as.factor(df2014$LAND.SQUARE.FEET.)
df2014$LAND.SQUARE.FEET.[df2014$LAND.SQUARE.FEET.==0] <- NA

Lan2014<-df2014[!is.na(df2014$LAND.SQUARE.FEET.), ] %>%
  group_by(df2014[!is.na(df2014$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Lan2014<-Lan2014[1:10,]
Lan2014<-as.data.frame(Lan2014)
names(Lan2014)[names(Lan2014) == "df2014[!is.na(df2014$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET."] <- "Land_Square_Feet"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2014$GROSS.SQUARE.FEET.<-as.factor(df2014$GROSS.SQUARE.FEET.)
df2014$GROSS.SQUARE.FEET.[df2014$GROSS.SQUARE.FEET.==0] <- NA

Gro2014<-df2014[!is.na(df2014$GROSS.SQUARE.FEET.), ] %>%
  group_by(df2014[!is.na(df2014$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Gro2014<-Gro2014[1:10,]
Gro2014<-as.data.frame(Gro2014)
names(Gro2014)[names(Gro2014) == "df2014[!is.na(df2014$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET."] <- "Gross_Square_Feet"

#CHECK FOR PROPERTY TRANSFER WITHOUT CASH WHICH IS ZERO SALES PRICE
Nul2014<-length(which(df2014$SALE.PRICE.==0))
Total2014<-length(df2014$SALE.PRICE.)

#IMPORT FILES
df2016<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2016_NYC_Property_Sales__10212019-Modified.csv")

#FOR 2016---------------------------------------------------------
#FILTER OUT MONTH AND WEEK 
df2016 <- df2016 %>% 
  mutate(month=month(SALE.DATE.),week = wday(SALE.DATE.,label = TRUE))

#GROUP DATA MONTHLY
Monthly2016<-df2016 %>%
  group_by(df2016$month) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Monthly2016)[names(Monthly2016) == "df2016$month"] <- "Month"
Monthly2016$Month<-as.factor(Monthly2016$Month)

#SUMMARISE BY MONTH and RENAME AUTO NAMING AND CHANGING TO CATEGORICAL VALUE
sales_by_week2016 <- df2016 %>% group_by(week,month) %>%
  summarise(Total_Sales=sum(SALE.PRICE.)) %>% ungroup()

Borough2016<-df2016 %>%
  group_by(df2016$ï..BOROUGH.NAME) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Borough2016)[names(Borough2016) == "df2016$ï..BOROUGH.NAME"] <- "Borough"
Borough2016$Borough<-as.factor(Borough2016$Borough)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$TAX.CLASS.AT.PRESENT.[df2016$TAX.CLASS.AT.PRESENT.==0] <- NA

TaxPres2016<-df2016[!is.na(df2016$TAX.CLASS.AT.PRESENT.), ] %>%
  group_by(df2016[!is.na(df2016$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(TaxPres2016)[names(TaxPres2016) == "df2016[!is.na(df2016$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT."] <- "Tax_At_Present"
TaxPres2016$Tax_At_Present<-as.factor(TaxPres2016$Tax_At_Present)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$YEAR.BUILT.[df2016$YEAR.BUILT.==0] <- NA

Year2016<-df2016[!is.na(df2016$YEAR.BUILT.), ] %>%
  group_by(df2016[!is.na(df2016$YEAR.BUILT.), ]$YEAR.BUILT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Year2016)[names(Year2016) == "df2016[!is.na(df2016$YEAR.BUILT.), ]$YEAR.BUILT."] <- "Year_Built"
Year2016$Year_Built<-as.factor(Year2016$Year_Built)

#-------------------------------------------------------------------
Ease2016<-df2016 %>%
  group_by(df2016$EASE.MENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Ease2016)[names(Ease2016) == "df2016$EASE.MENT."] <- "Easement"
Ease2016$Easement<-as.factor(Ease2016$Easement)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$ZIP.CODE.[df2016$ZIP.CODE.==0] <- NA

Zip2016<-df2016[!is.na(df2016$ZIP.CODE.), ] %>%
  group_by(df2016[!is.na(df2016$ZIP.CODE.), ]$ZIP.CODE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Zip2016)[names(Zip2016) == "df2016[!is.na(df2016$ZIP.CODE.), ]$ZIP.CODE."] <- "Zipcode"
Zip2016$Zipcode<-as.factor(Zip2016$Zipcode)

#-------------------------------------------------------------------
Nei2016<-df2016 %>%
  group_by(df2016$NEIGHBORHOOD.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Nei2016<-Nei2016[1:10,]
Nei2016<-as.data.frame(Nei2016)
names(Nei2016)[names(Nei2016) == "df2016$NEIGHBORHOOD."] <- "Neighborhood"

#-------------------------------------------------------------------
Bui2016<-df2016 %>%
  group_by(df2016$BUILDING.CLASS.CATEGORY.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Bui2016<-Bui2016[1:10,]
Bui2016<-as.data.frame(Bui2016)
names(Bui2016)[names(Bui2016) == "df2016$BUILDING.CLASS.CATEGORY."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$TAX.CLASS.AT.TIME.OF.SALE.<-as.factor(df2016$TAX.CLASS.AT.TIME.OF.SALE.)
df2016$TAX.CLASS.AT.TIME.OF.SALE.[df2016$TAX.CLASS.AT.TIME.OF.SALE.==0] <- NA

Tax2016<-df2016[!is.na(df2016$TAX.CLASS.AT.TIME.OF.SALE.), ] %>%
  group_by(df2016[!is.na(df2016$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

#SORTING OUT TOP 10 OCCURRENCE
Tax2016<-as.data.frame(Tax2016)
names(Tax2016)[names(Tax2016) == "df2016[!is.na(df2016$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE."] <- "Tax_Class_Category"

#-------------------------------------------------------------------
BuiSales2016<-df2016 %>%
  group_by(df2016$BUILDING.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
BuiSales2016<-BuiSales2016[1:10,]
BuiSales2016<-as.data.frame(BuiSales2016)
names(BuiSales2016)[names(BuiSales2016) == "df2016$BUILDING.CLASS.AT.PRESENT."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$RESIDENTIAL.UNITS.<-as.factor(df2016$RESIDENTIAL.UNITS.)
df2016$RESIDENTIAL.UNITS.[df2016$RESIDENTIAL.UNITS.==0] <- NA

Res2016<-df2016[!is.na(df2016$RESIDENTIAL.UNITS.), ] %>%
  group_by(df2016[!is.na(df2016$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Res2016<-Res2016[1:10,]
Res2016<-as.data.frame(Res2016)
names(Res2016)[names(Res2016) == "df2016[!is.na(df2016$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS."] <- "Residental_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$COMMERCIAL.UNITS.<-as.factor(df2016$COMMERCIAL.UNITS.)
df2016$COMMERCIAL.UNITS.[df2016$COMMERCIAL.UNITS.==0] <- NA

Com2016<-df2016[!is.na(df2016$COMMERCIAL.UNITS.), ] %>%
  group_by(df2016[!is.na(df2016$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Com2016<-Com2016[1:10,]
Com2016<-as.data.frame(Com2016)
names(Com2016)[names(Com2016) == "df2016[!is.na(df2016$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS."] <- "Commercial_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$TOTAL.UNITS.<-as.factor(df2016$TOTAL.UNITS.)
df2016$TOTAL.UNITS.[df2016$TOTAL.UNITS.==0] <- NA

Tot2016<-df2016[!is.na(df2016$TOTAL.UNITS.), ] %>%
  group_by(df2016[!is.na(df2016$TOTAL.UNITS.), ]$TOTAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Tot2016<-Tot2016[1:10,]
Tot2016<-as.data.frame(Tot2016)
names(Tot2016)[names(Tot2016) == "df2016[!is.na(df2016$TOTAL.UNITS.), ]$TOTAL.UNITS."] <- "Total_units"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$LAND.SQUARE.FEET.<-as.factor(df2016$LAND.SQUARE.FEET.)
df2016$LAND.SQUARE.FEET.[df2016$LAND.SQUARE.FEET.==0] <- NA

Lan2016<-df2016[!is.na(df2016$LAND.SQUARE.FEET.), ] %>%
  group_by(df2016[!is.na(df2016$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Lan2016<-Lan2016[1:10,]
Lan2016<-as.data.frame(Lan2016)
names(Lan2016)[names(Lan2016) == "df2016[!is.na(df2016$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET."] <- "Land_Square_Feet"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2016$GROSS.SQUARE.FEET.<-as.factor(df2016$GROSS.SQUARE.FEET.)
df2016$GROSS.SQUARE.FEET.[df2016$GROSS.SQUARE.FEET.==0] <- NA

Gro2016<-df2016[!is.na(df2016$GROSS.SQUARE.FEET.), ] %>%
  group_by(df2016[!is.na(df2016$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Gro2016<-Gro2016[1:10,]
Gro2016<-as.data.frame(Gro2016)
names(Gro2016)[names(Gro2016) == "df2016[!is.na(df2016$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET."] <- "Gross_Square_Feet"

#CHECK FOR PROPERTY TRANSFER WITHOUT CASH WHICH IS ZERO SALES PRICE
Nul2016<-length(which(df2016$SALE.PRICE.==0))
Total2016<-length(df2016$SALE.PRICE.)

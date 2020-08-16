#IMPORT FILES
df2015<-read.csv("C:/Users/User/Documents/R/Data Visual/D/2015_NYC_Property_Sales__10212019-Modified.csv")

#FOR 2015---------------------------------------------------------
#FILTER OUT MONTH AND WEEK 
df2015 <- df2015 %>% 
  mutate(month=month(SALE.DATE.),week = wday(SALE.DATE.,label = TRUE))

#GROUP DATA MONTHLY
Monthly2015<-df2015 %>%
  group_by(df2015$month) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))
#RENAME AUTO NAMES
names(Monthly2015)[names(Monthly2015) == "df2015$month"] <- "Month"
Monthly2015$Month<-as.factor(Monthly2015$Month)

#SUMMARISE BY MONTH and RENAME AUTO NAMING AND CHANGING TO CATEGORICAL VALUE
sales_by_week2015 <- df2015 %>% group_by(week,month) %>%
  summarise(Total_Sales=sum(SALE.PRICE.)) %>% ungroup()

Borough2015<-df2015 %>%
  group_by(df2015$ï..BOROUGH.NAME) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Borough2015)[names(Borough2015) == "df2015$ï..BOROUGH.NAME"] <- "Borough"
Borough2015$Borough<-as.factor(Borough2015$Borough)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$TAX.CLASS.AT.PRESENT.[df2015$TAX.CLASS.AT.PRESENT.==0] <- NA

TaxPres2015<-df2015[!is.na(df2015$TAX.CLASS.AT.PRESENT.), ] %>%
  group_by(df2015[!is.na(df2015$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(TaxPres2015)[names(TaxPres2015) == "df2015[!is.na(df2015$TAX.CLASS.AT.PRESENT.), ]$TAX.CLASS.AT.PRESENT."] <- "Tax_At_Present"
TaxPres2015$Tax_At_Present<-as.factor(TaxPres2015$Tax_At_Present)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$YEAR.BUILT.[df2015$YEAR.BUILT.==0] <- NA

Year2015<-df2015[!is.na(df2015$YEAR.BUILT.), ] %>%
  group_by(df2015[!is.na(df2015$YEAR.BUILT.), ]$YEAR.BUILT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Year2015)[names(Year2015) == "df2015[!is.na(df2015$YEAR.BUILT.), ]$YEAR.BUILT."] <- "Year_Built"
Year2015$Year_Built<-as.factor(Year2015$Year_Built)

#-------------------------------------------------------------------
Ease2015<-df2015 %>%
  group_by(df2015$EASE.MENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Ease2015)[names(Ease2015) == "df2015$EASE.MENT."] <- "Easement"
Ease2015$Easement<-as.factor(Ease2015$Easement)

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$ZIP.CODE.[df2015$ZIP.CODE.==0] <- NA

Zip2015<-df2015[!is.na(df2015$ZIP.CODE.), ] %>%
  group_by(df2015[!is.na(df2015$ZIP.CODE.), ]$ZIP.CODE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

names(Zip2015)[names(Zip2015) == "df2015[!is.na(df2015$ZIP.CODE.), ]$ZIP.CODE."] <- "Zipcode"
Zip2015$Zipcode<-as.factor(Zip2015$Zipcode)

#-------------------------------------------------------------------
Nei2015<-df2015 %>%
  group_by(df2015$NEIGHBORHOOD.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Nei2015<-Nei2015[1:10,]
Nei2015<-as.data.frame(Nei2015)
names(Nei2015)[names(Nei2015) == "df2015$NEIGHBORHOOD."] <- "Neighborhood"

#-------------------------------------------------------------------
Bui2015<-df2015 %>%
  group_by(df2015$BUILDING.CLASS.CATEGORY.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.)) %>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Bui2015<-Bui2015[1:10,]
Bui2015<-as.data.frame(Bui2015)
names(Bui2015)[names(Bui2015) == "df2015$BUILDING.CLASS.CATEGORY."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$TAX.CLASS.AT.TIME.OF.SALE.<-as.factor(df2015$TAX.CLASS.AT.TIME.OF.SALE.)
df2015$TAX.CLASS.AT.TIME.OF.SALE.[df2015$TAX.CLASS.AT.TIME.OF.SALE.==0] <- NA

Tax2015<-df2015[!is.na(df2015$TAX.CLASS.AT.TIME.OF.SALE.), ] %>%
  group_by(df2015[!is.na(df2015$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))

#SORTING OUT TOP 10 OCCURRENCE
Tax2015<-as.data.frame(Tax2015)
names(Tax2015)[names(Tax2015) == "df2015[!is.na(df2015$TAX.CLASS.AT.TIME.OF.SALE.), ]$TAX.CLASS.AT.TIME.OF.SALE."] <- "Tax_Class_Category"

#-------------------------------------------------------------------
BuiSales2015<-df2015 %>%
  group_by(df2015$BUILDING.CLASS.AT.PRESENT.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
BuiSales2015<-BuiSales2015[1:10,]
BuiSales2015<-as.data.frame(BuiSales2015)
names(BuiSales2015)[names(BuiSales2015) == "df2015$BUILDING.CLASS.AT.PRESENT."] <- "Building_Class_Category"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$RESIDENTIAL.UNITS.<-as.factor(df2015$RESIDENTIAL.UNITS.)
df2015$RESIDENTIAL.UNITS.[df2015$RESIDENTIAL.UNITS.==0] <- NA

Res2015<-df2015[!is.na(df2015$RESIDENTIAL.UNITS.), ] %>%
  group_by(df2015[!is.na(df2015$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Res2015<-Res2015[1:10,]
Res2015<-as.data.frame(Res2015)
names(Res2015)[names(Res2015) == "df2015[!is.na(df2015$RESIDENTIAL.UNITS.), ]$RESIDENTIAL.UNITS."] <- "Residental_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$COMMERCIAL.UNITS.<-as.factor(df2015$COMMERCIAL.UNITS.)
df2015$COMMERCIAL.UNITS.[df2015$COMMERCIAL.UNITS.==0] <- NA

Com2015<-df2015[!is.na(df2015$COMMERCIAL.UNITS.), ] %>%
  group_by(df2015[!is.na(df2015$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Com2015<-Com2015[1:10,]
Com2015<-as.data.frame(Com2015)
names(Com2015)[names(Com2015) == "df2015[!is.na(df2015$COMMERCIAL.UNITS.), ]$COMMERCIAL.UNITS."] <- "Commercial_units"

#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$TOTAL.UNITS.<-as.factor(df2015$TOTAL.UNITS.)
df2015$TOTAL.UNITS.[df2015$TOTAL.UNITS.==0] <- NA

Tot2015<-df2015[!is.na(df2015$TOTAL.UNITS.), ] %>%
  group_by(df2015[!is.na(df2015$TOTAL.UNITS.), ]$TOTAL.UNITS.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Tot2015<-Tot2015[1:10,]
Tot2015<-as.data.frame(Tot2015)
names(Tot2015)[names(Tot2015) == "df2015[!is.na(df2015$TOTAL.UNITS.), ]$TOTAL.UNITS."] <- "Total_units"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$LAND.SQUARE.FEET.<-as.factor(df2015$LAND.SQUARE.FEET.)
df2015$LAND.SQUARE.FEET.[df2015$LAND.SQUARE.FEET.==0] <- NA

Lan2015<-df2015[!is.na(df2015$LAND.SQUARE.FEET.), ] %>%
  group_by(df2015[!is.na(df2015$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Lan2015<-Lan2015[1:10,]
Lan2015<-as.data.frame(Lan2015)
names(Lan2015)[names(Lan2015) == "df2015[!is.na(df2015$LAND.SQUARE.FEET.), ]$LAND.SQUARE.FEET."] <- "Land_Square_Feet"
#-------------------------------------------------------------------
#NOT CONSIDERING ZERO VALUES
df2015$GROSS.SQUARE.FEET.<-as.factor(df2015$GROSS.SQUARE.FEET.)
df2015$GROSS.SQUARE.FEET.[df2015$GROSS.SQUARE.FEET.==0] <- NA

Gro2015<-df2015[!is.na(df2015$GROSS.SQUARE.FEET.), ] %>%
  group_by(df2015[!is.na(df2015$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET.) %>%
  summarise(Total_Sales = sum(SALE.PRICE.))%>%
  arrange(desc(Total_Sales))

#SORTING OUT TOP 10 OCCURRENCE
Gro2015<-Gro2015[1:10,]
Gro2015<-as.data.frame(Gro2015)
names(Gro2015)[names(Gro2015) == "df2015[!is.na(df2015$GROSS.SQUARE.FEET.), ]$GROSS.SQUARE.FEET."] <- "Gross_Square_Feet"

#CHECK FOR PROPERTY TRANSFER WITHOUT CASH WHICH IS ZERO SALES PRICE
Nul2015<-length(which(df2015$SALE.PRICE.==0))
Total2015<-length(df2015$SALE.PRICE.)

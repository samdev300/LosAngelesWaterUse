# explore.R

# Data From:
# https://data.lacity.org/A-Livable-and-Sustainable-City/Water-and-Electric-Usage-from-2005-2013/asvq-scwp

# Water usage is measured in Hundred Cubic Feet (HCF)
# 1 HCF is approximately equal to 748 gallons

waterPower <- read.csv("data/Water_and_Electric_Usage_from_2005_-_2013.csv")

# Water is measured in HCF (One Hundred Cubit Feet ~ about  748.5 gallons of water)
# Electricity is measured in kWh (kilowatt-hour)

install.packages("zoo")
install.packages("dplyr")
install.packages("tidyr")
install.packages('stringr')

library(zoo)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# conver the date to Date format
waterPower$Date <- as.Date(as.yearmon(waterPower$Text.Date, "%b_%Y"))

# Split the zip to zip & geo codes
zipSplit <- ldply(str_split(waterPower$Zip.Code, "\n"))
geoSplit <- ldply(str_split(zipSplit$V2, ","))

geoSplit$V1 <- str_replace_all(geoSplit$V1, "\\(", "")
geoSplit$V1 <- str_replace_all(geoSplit$V1, " ", "")

geoSplit$V2 <- str_replace_all(geoSplit$V2, "\\)", "")
geoSplit$V2 <- str_replace_all(geoSplit$V2, " ", "")

waterPower$ZipCode <- zipSplit$V1
waterPower$Lon <- geoSplit$V1
waterPower$Lat <- geoSplit$V2

# Remove the old variables
waterPower$Zip.Code <- NULL
waterPower$Text.Date <- NULL
waterPower$Value.Date <- NULL

# Reorder the columns just for neatness sake
waterPower <- waterPower[, c(3, 4, 5, 6, 1, 2)]

# Clean up the temporary objects
rm(zipSplit)
rm(geoSplit)


# Write the cleaned data to a table
write.table(waterPower, "data/WaterAndPowerUsage_2005-2013.csv", row.names=FALSE)


# ------------------------------------------------------------------------------------
# Get just the 2011 data;  Get total by zipcode
waterPower2011 <- waterPower %>%
    filter(Date >= as.Date('2011-01-01')) %>%
    filter(Date < as.Date('2012-01-01')) %>%
    group_by(ZipCode) %>%
    summarise(Water.Use = sum(Water.Use), Power.Use = sum(Power.Use)) %>%
    arrange(desc(Water.Use))


# ------------------------------------------------------------------------------------
# Load Population info
# Merge with Population table to get population
# The result is the data for just the Los Angeles
population <- read.csv("data/2010+Census+Population+By+Zipcode+(ZCTA).csv")
colnames(population) <- c("ZipCode", "Population")

population$ZipCode <- as.character(population$ZipCode)
waterPower2011 <- waterPower2011 %>%
    inner_join(population, by="ZipCode")


# Add Latitude, Longitude for each zip code
install.packages("zipcode")
library(zipcode)
data(zipcode)

waterPower2011$Lon <- sapply(waterPower2011$ZipCode, function(x) zipcode$longitude[zipcode$zip == x])
waterPower2011$Lat <- sapply(waterPower2011$ZipCode, function(x) zipcode$latitude[zipcode$zip == x])

# Write to a file
write.table(waterPower2011, "data/WaterPowerPopulation2011.csv", sep=",", row.names=FALSE)


# ------------------------------------------------------------------------------------
waterPower2011$Water.Use.Per.Month <- waterPower2011$Water.Use / 12
waterPower2011$Water.Use.Per.Person <- signif(waterPower2011$Water.Use / waterPower2011$Population, 4)

# Water Use Per Person Per Month in gallons
waterPower2011$Water.Use.Per.Month.Per.Person.Gallons <- signif((waterPower2011$Water.Use.Per.Month / waterPower2011$Population) * 748.05, 5)

# Make a backup
waterPower2011_BAK <- waterPower2011


# Remove the outliers
waterPower2011 <- subset(waterPower2011, Water.Use > 0)
waterPower2011 <- subset(waterPower2011, Population > 100)


library(ggplot2)
ggplot(data=waterPower2011, aes(x=Water.Use.Per.Month.Per.Person.Gallons)) + geom_histogram(binwidth=0.1)

ggplot(data=waterPower2011, aes(x=log(Water.Use.Per.Month.Per.Person.Gallons))) + geom_histogram(binwidth=0.1)



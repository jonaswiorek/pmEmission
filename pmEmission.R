require(dplyr) || install.packages("dplyr")
library(dplyr)
require(ggplot2) || install.packages("ggplot2")
library(ggplot2)

if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

#1
pm25emission <- filter(NEI, Pollutant=="PM25-PRI")
totalPm25emission <- summarize(group_by(pm25emission,year), emission = sum(Emissions, na.rm=TRUE))
with(totalPm25emission, plot(year, emission, xaxp = c(1999,2008,3), pch=20, type = "o", col = "dark red", main="Total emissions from PM2.5 in the US"))

#2
pm25emissionBaltimoreCity <- filter(NEI,Pollutant == "PM25-PRI", fips == "24510")
totalPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year), emission = sum(Emissions, na.rm=TRUE))
with(totalPm25emissionBaltimoreCity, plot(year, emission, xaxp = c(1999,2008,3), pch=20, type = "o", col = "dark red", main="Total emissions from PM2.5 in Baltimore City, MD"))

#3
typeOfPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year, type), emission = sum(Emissions, na.rm=TRUE))
qplot(year, emission, 
      data = typeOfPm25emissionBaltimoreCity,
      scale_x_continuous(breaks = seq(1999,2008,3)),
      col = type, 
      geom = c("line", "point"), 
      main="Type of emission sources of PM2.5 in Baltimore City, MD"
)
ggplot(data = typeOfPm25emissionBaltimoreCity, aes(x=year, y=emission, col = type)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point() +
        geom_line() +
        ggtitle("Type of emission sources of PM2.5 in Baltimore City, MD")
        
#4
#coal <- which(grepl("[Cc]oal",SCC$Short.Name))
coal <- which(grepl("[Cc]oal",SCC$EI.Sector))
coalEmission <- filter(NEI, NEI$SCC %in% SCC$SCC[coal])
#coalEmissionSource <- summarize(group_by(coalEmission,SCC, year), emission = sum(Emissions))
coalEmissionSource <- summarize(group_by(coalEmission, year), emission = sum(Emissions))

#ggplot(coalEmissionSource, aes(x=year, y=emission, col = SCC)) +
ggplot(coalEmissionSource, aes(x=year, y=emission)) +
        geom_point() +
        geom_line()
coalEmissionSource        

#5
vehicle <- which(grepl("[Vv]ehicle", SCC$EI.Sector))
vehicleEmissionBaltimoreCity <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "24510")
totalVehicleEmissionBaltimoreCity <- summarize(group_by(vehicleEmissionBaltimoreCity, year, fips), emission = sum(Emissions))
totalVehicleEmissionBaltimoreCity

ggplot(totalVehicleEmissionBaltimoreCity, aes(year, emission)) +
        geom_point() +
        geom_line()

#6
vehicleEmissionLosAngelesCounty <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "06037")
totalVehicleEmissionLosAngelesCounty <- summarize(group_by(vehicleEmissionLosAngelesCounty, year, fips), emission = sum(Emissions))
totalVehicleEmissionLosAngelesCounty

totalVehicleEmission <- rbind(totalVehicleEmissionBaltimoreCity, totalVehicleEmissionLosAngelesCounty)
        
ggplot(totalVehicleEmission, aes(year, emission, col = fips)) +
        geom_point() +
        geom_line()


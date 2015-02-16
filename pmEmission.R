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
with(totalPm25emission, plot(year, emission, xaxp = c(1999,2008,3), ylab = "PM2.5 emission [ton]", pch=20, type = "o", col = "dark green", main="Total PM2.5 emissions in the US"))

#2
pm25emissionBaltimoreCity <- filter(NEI,Pollutant == "PM25-PRI", fips == "24510")
totalPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year), emission = sum(Emissions, na.rm=TRUE))
with(totalPm25emissionBaltimoreCity, plot(year, emission, xaxp = c(1999,2008,3), ylab = "PM2.5 emission [ton]", pch=20, type = "o", col = "dark red", main="Total PM2.5 emissions in Baltimore City, MD"))

#3
typeOfPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year, type), emission = sum(Emissions, na.rm=TRUE))
plot(
  qplot(year, emission, 
      data = typeOfPm25emissionBaltimoreCity,
      scale_x_continuous(breaks = seq(1999,2008,3)),
      col = type, 
      geom = c("line", "point"),
      ylab = "PM2.5 emission [ton]",
      main="Type of PM2.5 emission sources in Baltimore City, MD"
  )
)
plot(
  ggplot(data = typeOfPm25emissionBaltimoreCity, aes(x=year, y=emission, col = type)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point() +
        geom_line() +
        ylab("PM2.5 emission [ton]") + 
        ggtitle("Type of PM2.5 emission sources in Baltimore City, MD")
)

#4
#coal <- which(grepl("[Cc]oal",SCC$Short.Name))
coal <- which(grepl("[Cc]oal",SCC$EI.Sector))
coalEmission <- filter(NEI, NEI$SCC %in% SCC$SCC[coal])
#coalEmissionSource <- summarize(group_by(coalEmission,SCC, year), emission = sum(Emissions))
coalEmissionSource <- summarize(group_by(coalEmission, year), emission = sum(Emissions))

plot(
  #ggplot(coalEmissionSource, aes(x=year, y=emission, col = SCC)) +
  ggplot(coalEmissionSource, aes(x=year, y=emission)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point(col ="dark green") +
        geom_line(col ="dark green") +
        ylab("PM2.5 emission [ton]") +
        ggtitle("PM2.5 emissions from coal combustion-related sources in the US")
)

coalEmissionSource        

#5
vehicle <- which(grepl("[Vv]ehicle", SCC$EI.Sector))
vehicleEmissionBaltimoreCity <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "24510")
totalVehicleEmissionBaltimoreCity <- summarize(group_by(vehicleEmissionBaltimoreCity, year, fips), emission = sum(Emissions))
totalVehicleEmissionBaltimoreCity

plot(
  ggplot(totalVehicleEmissionBaltimoreCity, aes(year, emission)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point(col ="dark red") +
        geom_line(col ="dark red") +
        ylab("PM2.5 emission [ton]") +
        ggtitle("PM2.5 emissions from motor vehicle sources in Baltimore City, MD")
)
#6
vehicleEmissionLosAngelesCounty <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "06037")
totalVehicleEmissionLosAngelesCounty <- summarize(group_by(vehicleEmissionLosAngelesCounty, year, fips), emission = sum(Emissions))
totalVehicleEmissionLosAngelesCounty

totalVehicleEmission <- rbind(totalVehicleEmissionBaltimoreCity, totalVehicleEmissionLosAngelesCounty)

plot(
  ggplot(totalVehicleEmission, aes(year, emission, col = fips)) +
        geom_point() +
        geom_line() +
        ggtitle("PM2.5 emissions from motor vehicle sources") +
        ylab("PM2.5 emission [ton]") + 
        scale_color_manual("", 
                            labels = c("Los Angeles County", "Baltimore City"),
                            values = c("dark blue", "dark red"))
)        


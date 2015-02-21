library(dplyr)
library(ggplot2)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Emissions from motor vehicle sources for each year in Baltimore City, MD and
# Los Angeles County, CA
vehicle <- which(grepl("[Vv]ehicle", SCC$EI.Sector))
vehicleEmissionBaltimoreCity <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "24510")
totalVehicleEmissionBaltimoreCity <- summarize(group_by(vehicleEmissionBaltimoreCity, year, fips), emission = sum(Emissions))
vehicleEmissionLosAngelesCounty <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "06037")
totalVehicleEmissionLosAngelesCounty <- summarize(group_by(vehicleEmissionLosAngelesCounty, year, fips), emission = sum(Emissions))
totalVehicleEmission <- rbind(totalVehicleEmissionBaltimoreCity, totalVehicleEmissionLosAngelesCounty)

# Make plot
ggplot(totalVehicleEmission, aes(year, emission, col = fips)) +
        geom_point() +
        geom_line() +
        ggtitle("PM2.5 emissions from motor vehicle sources") +
        ylab("PM2.5 emission [ton]") + 
        scale_color_manual("", 
                           labels = c("Los Angeles County", "Baltimore City"),
                           values = c("dark blue", "dark red"))


# Save plot to PNG
dev.copy(png, file ="plot6.png", width = 480, height = 480)
dev.off()
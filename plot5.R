library(dplyr)
library(ggplot2)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Emissions from motor vehicle sources for each year in Baltimore City, MD
vehicle <- which(grepl("[Vv]ehicle", SCC$EI.Sector))
vehicleEmissionBaltimoreCity <- filter(NEI, NEI$SCC %in% SCC$SCC[vehicle], fips == "24510")
totalVehicleEmissionBaltimoreCity <- summarize(group_by(vehicleEmissionBaltimoreCity, year, fips), emission = sum(Emissions))

# Make plot
ggplot(totalVehicleEmissionBaltimoreCity, aes(year, emission)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point(col ="dark red") +
        geom_line(col ="dark red") +
        ylab("PM2.5 emission [ton]") +
        ggtitle("PM2.5 emissions from motor vehicle sources in Baltimore City, MD")

# Save plot to PNG
dev.copy(png, file ="plot5.png", width = 480, height = 480)
dev.off()
library(dplyr)
library(ggplot2)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Type of PM2.5 emissions for each year in Baltimore City, MD
pm25emissionBaltimoreCity <- filter(NEI,Pollutant == "PM25-PRI", fips == "24510")
typeOfPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year, type), emission = sum(Emissions, na.rm=TRUE))

# Make plot
ggplot(data = typeOfPm25emissionBaltimoreCity, aes(x=year, y=emission, col = type)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point() +
        geom_line() +
        ylab("PM2.5 emission [ton]") + 
        ggtitle("Type of PM2.5 emission sources in Baltimore City, MD")

# Save plot to PNG
dev.copy(png, file ="plot3.png", width = 480, height = 480)
dev.off()
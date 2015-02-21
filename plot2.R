library(dplyr)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Total PM2.5 emission for each year in Baltimore City, MD
pm25emissionBaltimoreCity <- filter(NEI,Pollutant == "PM25-PRI", fips == "24510")
totalPm25emissionBaltimoreCity <- summarize(group_by(pm25emissionBaltimoreCity, year), emission = sum(Emissions, na.rm=TRUE))

# Make plot
with(totalPm25emissionBaltimoreCity, plot(year, emission, xaxp = c(1999,2008,3), ylab = "PM2.5 emission [ton]", pch=20, type = "o", col = "dark red", main="Total PM2.5 emissions in Baltimore City, MD"))

# Save plot to PNG
dev.copy(png, file ="plot2.png", width = 480, height = 480)
dev.off()

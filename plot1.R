library(dplyr)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Total PM2.5 emission for each year in th U.S.
pm25emission <- filter(NEI, Pollutant=="PM25-PRI")
totalPm25emission <- summarize(group_by(pm25emission,year), emission = sum(Emissions, na.rm=TRUE))

# Make plot
par(mfrow = c(1,1))
with(totalPm25emission, plot(year, emission, xaxp = c(1999,2008,3), ylab = "PM2.5 emission [ton]", pch=20, type = "o", col = "dark green", main="Total PM2.5 emissions in the US"))

# Save plot to PNG
dev.copy(png, file ="plot1.png", width = 480, height = 480)
dev.off()

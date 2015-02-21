library(dplyr)
library(ggplot2)

# Read input data
if( "summarySCC_PM25.rds" %in% dir("./exdata-data-NEI_data/"))  {
        NEI <- readRDS("./exdata-data-NEI_data//summarySCC_PM25.rds")
}

if( "Source_Classification_Code.rds" %in% dir("./exdata-data-NEI_data/"))  {
        SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
}

# Emissions from coal combustion-related sources for each year in th U.S.
coal <- which(grepl("[Cc]oal",SCC$EI.Sector))
coalEmission <- filter(NEI, NEI$SCC %in% SCC$SCC[coal])
coalEmissionSource <- summarize(group_by(coalEmission, year), emission = sum(Emissions))

# Make plot
ggplot(coalEmissionSource, aes(x=year, y=emission)) +
        scale_x_continuous(breaks = seq(1999,2008, 3)) +
        geom_point(col ="dark green") +
        geom_line(col ="dark green") +
        ylab("PM2.5 emission [ton]") +
        ggtitle("PM2.5 emissions from coal combustion-related sources in the US")


# Save plot to PNG
dev.copy(png, file ="plot4.png", width = 480, height = 480)
dev.off()

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

Total_PM25 <- with(NEI, tapply(Emissions, year, sum, na.rm=TRUE))
Total_PM25 <- Total_PM25/100000

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot1

barplot(Total_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2, 
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 80),
        ylab = "PM25 (x100000 tons)",
        xlab = "Year",
        main = "Annual PM25 Emissions")

# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot1.png", width = 480, height = 480)
barplot(Total_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2, 
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 80),
        ylab = "PM25 (x100000 tons)",
        xlab = "Year",
        main = "Annual PM25 Emissions")
dev.off()

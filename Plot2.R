
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510")from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_Baltimore <- subset(NEI, fips == 24510)

Total_PM25 <- with(NEI_Baltimore, tapply(Emissions, year, sum, na.rm=TRUE))

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot2

barplot(Total_PM25, col ="goldenrod2", space = 0.8, lwd = 2, axis.lty = 1,
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 3500),
        ylab = "PM25 (tons)",
        xlab = "Year",
        main = "Baltimore City Annual PM25 Emissions")

# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot2.png", width = 480, height = 480)
barplot(Total_PM25, col ="goldenrod2", space = 0.8, lwd = 2, axis.lty = 1,
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 3500),
        ylab = "PM25 (tons)",
        xlab = "Year",
        main = "Baltimore City Annual PM25 Emissions")

dev.off()
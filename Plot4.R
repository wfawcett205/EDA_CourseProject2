
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999-2008?

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_Point <- subset(NEI, NEI$type == "POINT")
NEI_NonPoint <- subset(NEI, NEI$type == "NONPOINT")
SCC_Point <- subset(SCC, Data.Category == "Point")
SCC_NonPoint <- subset(SCC, Data.Category == "Nonpoint")
SCC_OnRoad <- subset(SCC, Data.Category == "Onroad")
SCC_NonRoad <- subset(SCC, Data.Category == "Nonroad")
SCC_Event <- subset(SCC, Data.Category == "Event")
SCC_Biogenic <- subset(SCC, Data.Category == "Biogenic")
SCC_Coal <- rbind(SCC_Point, SCC_NonPoint)
SCC_Coal <- SCC_Coal[,1:3]
SCC_Index <- read.csv("SCC_Coal_Index.csv")
SCC_Index <- SCC_Index[, c(2,4)]
NEI_Coal <- filter(NEI, NEI$SCC %in% SCC_Index$SCC)

Total_PM25 <- with(NEI_Coal, tapply(Emissions, year, sum, na.rm=TRUE))
Total_PM25 <- Total_PM25/10000

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot4

barplot(Total_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2,
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 80),
        ylab = "PM25 (x10000 tons)", 
        xlab = "Year", 
        main = "Coal: Annual PM25 Emissions")

# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot4.png", width = 480, height = 480)
barplot(Total_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2,
        cex.names = 1,
        cex.axis = 1,
        ylim = c(0, 80),
        ylab = "PM25 (x10000 tons)", 
        xlab = "Year", 
        main = "Coal: Annual PM25 Emissions")
dev.off()

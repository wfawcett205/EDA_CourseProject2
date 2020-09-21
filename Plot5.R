
# How have emissions from motor vehicle sources changed from 1999-2008 in 
# Baltimore City? (fips == 24510)

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC_Biogenic <- subset(SCC, Data.Category == "Biogenic")
SCC_Event <- subset(SCC, Data.Category == "Event")
SCC_NonPoint <- subset(SCC, Data.Category == "Nonpoint")
SCC_NonRoad <- subset(SCC, Data.Category == "Nonroad")
SCC_OnRoad <- subset(SCC, Data.Category == "Onroad")
SCC_Point <- subset(SCC, Data.Category == "Point")

write.csv(SCC_OnRoad, "SCC_OnRoad.csv")
write.csv(SCC_NonRoad, "SCC_NonRoad.csv")

NEI_Baltimore <- subset(NEI, fips == 24510)
NEI_Baltimore_OnRoad <- filter(NEI_Baltimore, NEI_Baltimore$SCC %in% 
                                SCC_OnRoad$SCC)
NEI_Baltimore_NonRoad <- filter(NEI_Baltimore, NEI_Baltimore$SCC %in% 
                                SCC_NonRoad$SCC)
NEI_Baltimore_MotorVehicle <- rbind(NEI_Baltimore_OnRoad, NEI_Baltimore_NonRoad)

Motor_PM25 <- with(NEI_Baltimore_MotorVehicle, tapply(Emissions, year, sum, 
                   na.rm=TRUE))
OnRoad_PM25 <-  with(NEI_Baltimore_OnRoad, tapply(Emissions, year, sum, 
                     na.rm=TRUE))
NonRoad_PM25 <-  with(NEI_Baltimore_NonRoad, tapply(Emissions, year, sum, 
                      na.rm=TRUE))

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot5

barplot(Motor_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2,
        cex.names = 1,
        cex.axis = 1,
        cex.main = 1.4,
        cex.lab = 1.2, 
        font.lab = 2,
        ylim = c(0, 460),
        ylab = "PM25 (tons)", 
        xlab = "Year", 
        main = "Baltimore City Annual PM25 Emissions (Motor Vehicle)")


# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot5.png", width = 480, height = 480)
barplot(Motot_PM25, col = "slategray", space = 0.8, axis.lty = 1, lwd = 2,
        cex.names = 1,
        cex.axis = 1,
        cex.main = 1.4,
        cex.lab = 1.2, 
        font.lab = 2,
        ylim = c(0, 460),
        ylab = "PM25 (tons)", 
        xlab = "Year", 
        main = "Baltimore City Annual PM25 Emissions (Motor Vehicle)")
dev.off()

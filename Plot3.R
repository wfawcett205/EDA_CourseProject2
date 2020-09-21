
# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in emissions
# from 1999-2008 for Baltimore City? Which have seen increases in emissions from
# 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_Baltimore <- subset(NEI, fips == 24510)

PM25 <- with(NEI_Baltimore, tapply(Emissions, list(type, year), sum, 
                                         na.rm=TRUE))
PM25 <- as.data.frame(PM25)
PM25 <- rownames_to_column(PM25, var = "Type")

PM25_1999 <- PM25[,1:2]
PM25_2002 <- PM25[,c(1,3)]
PM25_2005 <- PM25[,c(1,4)]
PM25_2008 <- PM25[,c(1,5)]

cnames <- c("Type", "Emissions")
colnames(PM25_1999) <- cnames
colnames(PM25_2002) <- cnames
colnames(PM25_2005) <- cnames
colnames(PM25_2008) <- cnames

Year <- c("1999","1999","1999","1999","2002","2002","2002","2002","2005","2005",
          "2005","2005","2008","2008","2008","2008")

Total_PM25 <- rbind(PM25_1999, PM25_2002, PM25_2005, PM25_2008)
Total_PM25 <- cbind(Total_PM25, Year)
Total_PM25 <- Total_PM25[,c(3,2,1)]
Total_PM25$Type <- as.factor(Total_PM25$Type)

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot3

plot3 <- ggplot(Total_PM25, aes(y=Emissions, x=Year, fill=(Type))) + 
            geom_bar(stat="identity",  width = 0.5) +
            facet_wrap(vars(Type), nrow = 2, ncol = 2) +
            ggtitle("Baltimore City Annual PM25 Emissions") +
            ylab("PM25 Emissions (tons)") +
            xlab("Year") +
            scale_fill_manual(values = c("blue4", "forestgreen", "darkred",
                                         "darkgoldenrod"))
plot3

# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot3.png", width = 480, height = 480)
plot3 <- ggplot(Total_PM25, aes(y=Emissions, x=Year, fill=(Type))) + 
      geom_bar(stat="identity",  width = 0.5) +
      facet_wrap(vars(Type), nrow = 2, ncol = 2) +
      ggtitle("Baltimore City Annual PM25 Emissions") +
      ylab("PM25 Emissions (tons)") +
      xlab("Year") +
      scale_fill_manual(values = c("blue4", "forestgreen", "darkred",
                                   "darkgoldenrod"))
plot3
dev.off()

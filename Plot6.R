
# Compare emissions from motor vehicle sources in Baltimore City (fips == 24510) 
# with emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

library(tidyverse)
library(extrafont)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC_NonRoad <- subset(SCC, Data.Category == "Nonroad")
SCC_OnRoad <- subset(SCC, Data.Category == "Onroad")

NEI_Baltimore <- subset(NEI, fips == "24510")
NEI_LosAngeles <- subset(NEI, fips == "06037")

NEI_Baltimore_OnRoad <- filter(NEI_Baltimore, NEI_Baltimore$SCC %in% 
                                SCC_OnRoad$SCC)
NEI_Baltimore_NonRoad <- filter(NEI_Baltimore, NEI_Baltimore$SCC %in% 
                                SCC_NonRoad$SCC)
NEI_Baltimore_MotorVehicle <- rbind(NEI_Baltimore_OnRoad, NEI_Baltimore_NonRoad)


NEI_LosAngeles_OnRoad <- filter(NEI_LosAngeles, NEI_LosAngeles$SCC %in% 
                                       SCC_OnRoad$SCC)
NEI_LosAngeles_NonRoad <- filter(NEI_LosAngeles, NEI_LosAngeles$SCC %in% 
                                        SCC_NonRoad$SCC)
NEI_LosAngeles_MotorVehicle <- rbind(NEI_LosAngeles_OnRoad, NEI_LosAngeles_NonRoad)


Baltimore_Motor_PM25 <- with(NEI_Baltimore_MotorVehicle, tapply(Emissions, year,
                                sum, na.rm=TRUE))
Baltimore_OnRoad_PM25 <-  with(NEI_Baltimore_OnRoad, tapply(Emissions, year, sum, 
                                na.rm=TRUE))
Baltimore_NonRoad_PM25 <-  with(NEI_Baltimore_NonRoad, tapply(Emissions, year, 
                                sum, na.rm=TRUE))


LosAngeles_Motor_PM25 <- with(NEI_LosAngeles_MotorVehicle, tapply(Emissions, year,
                                sum, na.rm=TRUE))
LosAngeles_OnRoad_PM25 <-  with(NEI_LosAngeles_OnRoad, tapply(Emissions, year, sum, 
                                na.rm=TRUE))
LosAngeles_NonRoad_PM25 <-  with(NEI_LosAngeles_NonRoad, tapply(Emissions, year, 
                                sum, na.rm=TRUE))

# Normalize the data

Baltimore_Motor_PM25_Normalize <- Baltimore_Motor_PM25/sum(Baltimore_Motor_PM25)
LosAngeles_Motor_PM25_Normalize <- LosAngeles_Motor_PM25/sum(LosAngeles_Motor_PM25)

Normalized_Motor_PM25 <- rbind(Baltimore_Motor_PM25_Normalize, 
                               LosAngeles_Motor_PM25_Normalize)

Normalized_Motor_PM25 <- t(Normalized_Motor_PM25)
Normalized_Motor_PM25 <- as.data.frame(Normalized_Motor_PM25)
Normalized_Motor_PM25 <- rownames_to_column(Normalized_Motor_PM25, var = "Year")

cnames <- c("Year", "Baltimore", "LosAngeles")
colnames(Normalized_Motor_PM25) <- cnames

df1 <- Normalized_Motor_PM25[,c(1,2)]
colnames(df1) <- c("Year", "Data")
df2 <- Normalized_Motor_PM25[,c(1,3)]
colnames(df2) <- c("Year", "Data")
df1 <- rbind(df1, df2)
City <- c("Baltimore", "Baltimore", "Baltimore","Baltimore", 
          "LosAngeles", "LosAngeles", "LosAngeles", "LosAngeles")
df1 <- cbind(df1, City)
df1 <- df1[,c(1,3,2)]
df1[,2] <- as.factor(df1[,2])

# Exploratory Data Analysis using default graphics device (results not shown)
# Set plot6

plot6 <- ggplot(df1, aes(x=Year, y= Data, fill=(City))) + 
         geom_bar(stat="identity",  width = 0.6, position=position_dodge()) +
         geom_text(aes(label = format(Data, digits = 2), vjust=-0.6, size = 3),
                  position=position_dodge(0.6)) +
         labs(title = "Normalized Annual PM25 Emissions",
             y = "Relative PM25 Emissions",
             x = "Year") +
         theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (15)), 
             legend.title = element_blank(), 
             legend.text = element_text(family = "Times New Roman", face = "bold"), 
             axis.title = element_text(family = "Times New Roman", face = "bold", size = (12)),
             axis.text = element_text(family = "Courier New", face = "bold", size = (11))) +
         scale_fill_manual(values = c("darkgoldenrod", "darkred"))

plot6

# Open 2nd graphics device, upload plot, close 2nd graphics device

png(filename = "plot6.png", width = 480, height = 480)

plot6 <- ggplot(df1, aes(x=Year, y= Data, fill=(City))) + 
        geom_bar(stat="identity",  width = 0.6, position=position_dodge()) +
        geom_text(aes(label = format(Data, digits = 2), vjust=-0.6, size = 3),
                  position=position_dodge(0.6)) +
        labs(title = "Normalized Annual PM25 Emissions",
             y = "Relative PM25 Emissions",
             x = "Year") +
        theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (15)), 
              legend.title = element_blank(), 
              legend.text = element_text(family = "Times New Roman", face = "bold"), 
              axis.title = element_text(family = "Times New Roman", face = "bold", size = (12)),
              axis.text = element_text(family = "Courier New", face = "bold", size = (11))) +
        scale_fill_manual(values = c("darkgoldenrod", "darkred"))

plot6

dev.off()

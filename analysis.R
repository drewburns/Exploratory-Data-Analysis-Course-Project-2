#Andrew Burns
#Exploratory Data Analysis Project

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission
#from all sources for each of the years 1999, 2002, 2005, and 2008.
install.packages("ggplot2")
library('ggplot2')

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

givenyears <- c(1999, 2002, 2005, 2008)
emissions <- c()
for (theyear in givenyears) {  
  subsetdata <- subset(baltimore, year == toString(theyear)) #change NEI to baltimore for just baltimore
  assign(paste( "years", theyear, sep = ""), subsetdata)
  emissions <- c(emissions, sum(subsetdata$Emissions, na.rm = TRUE))
}
barplot(emissions, xlab="Year", ylab="Total Emissions from PM2.5 in Tons",
        axes=TRUE, names.arg=givenyears, 
        main = "Total Emissions from PM2.5 from 1999 to 2008")
abline(lm(emissions ~ c(0,1,2,3)))

dev.copy(png,'plot1.png')
dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# fips == "24510"
baltimore <- subset(NEI, fips == "24510")

barplot(emissions, xlab="Year", ylab="Total Emissions from PM2.5 in Kilotons",
        axes=TRUE, names.arg=givenyears,
        main = "Total Emissions from PM2.5 from 1999 to 2008 in Baltimore", 
        ylim=c(0,3500))
abline(lm(emissions ~ c(0,1,2,3)))

dev.copy(png,'plot2.png')
dev.off()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

baltimore <- subset(NEI, fips == "24510")
baltimore$year <- factor(baltimore$year,levels=c('1999', '2002', '2005', '2008'))
#ggplot() + geom_boxplot(data = baltimore, aes(x=year, y= Emissions)) + facet_grid(. ~ type)
#ggplot() + geom_point(data = baltimore, aes(x=year, y= Emissions)) + facet_grid(. ~ type)
ggplot(data = baltimore, aes(x=year, y= Emissions, fill=type)) + geom_bar(stat="identity") + facet_grid(. ~ type) + labs(title ="Amount of Emission from Sources over 1999-2008 in Baltimore", y="Emissions in Kilotons")
setwd('graphs') 

dev.copy(png,'plot3.png')
dev.off()

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coalMatches  <- grepl("coal", SCC$Short.Name, ignore.case=TRUE)
combMatches  <- grepl("comb", SCC$Short.Name, ignore.case=TRUE)
coalandcomb <- combMatches & coalMatches
combustionSCC <- SCC[coalandcomb,]$SCC
combustionNEI <- NEI[NEI$SCC %in%  combustionSCC, ]
combustionNEI$year <- factor(combustionNEI$year,levels=c('1999', '2002', '2005', '2008'))

ggplot(data = combustionNEI, aes(x=year, y= Emissions, fill=year)) + geom_bar(stat="identity")+ labs(title= "Emissions from Coal and Combustion Sources from 1999-2008", y = "Emissions in Kilotons")
setwd('graphs')

dev.copy(png,'plot4.png')
dev.off()

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
motorMatches  <- grepl("vehicle", SCC$Short.Name, ignore.case=TRUE)
motorSCC <- SCC[motorMatches,]$SCC
motorNEI <- NEI[NEI$SCC %in%  motorSCC, ]
motorNEIBalt <- subset(motorNEI, fips == "24510")
motorNEIBalt$year <- factor(motorNEIBalt$year,levels=c('1999', '2002', '2005', '2008'))

ggplot(data = motorNEIBalt, aes(x=year, y= Emissions, fill=year)) + geom_bar(stat="identity")+ labs(title= "Emissions from Vehicle Sources from 1999-2008 in Baltimore", y = "Emissions in Kilotons")

dev.copy(png,'plot5.png')
dev.off()

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California 
#Which city has seen greater changes over time in motor vehicle emissions?
# fips = "06037"
motorMatches  <- grepl("vehicle", SCC$Short.Name, ignore.case=TRUE)
motorSCC <- SCC[motorMatches,]$SCC
motorNEI <- NEI[NEI$SCC %in%  motorSCC, ]
LAandBalt <- subset(motorNEI, fips == "24510" | fips=="06037")
LAandBalt$year <- factor(LAandBalt$year,levels=c('1999', '2002', '2005', '2008'))
LAandBalt$fips <- factor(LAandBalt$fips, levels=c("06037", "24510"))
LAandBalt$city <- LAandBalt$fips
library(plyr)
LAandBalt$city <- revalue(LAandBalt$city, c('24510'='Baltimore', '06037' = 'Los Angeles'))

ggplot(data = LAandBalt, aes(x=year, y= Emissions, fill=year)) + geom_bar(stat="identity") + facet_grid(. ~ city) +labs(title= "Emissions from Vehicle Sources from 1999-2008 in Baltimore vs Los Angeles", y = "Emissions in Kilotons")

dev.copy(png, 'plot6.png')
dev.off()

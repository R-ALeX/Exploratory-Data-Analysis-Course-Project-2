# -----  add library
library(dplyr)
library(ggplot2)

# -----  create variables
FileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
NameFile <- "Dataset.zip"

# -----  download archive
## ----- if we don't have file with the same name
if (!file.exists(NameFile)) {
  download.file(FileUrl, NameFile, mode = "wb")
}

# -----  download archive
unzip(NameFile)

# -----  read files
PM25 <- readRDS("summarySCC_PM25.rds")
Source_Class <- readRDS("Source_Classification_Code.rds")

# -----  aggregation for plot1
agg <- aggregate(Emissions ~ year,PM25, sum)

# -----  aggregation for plot2, plot3
Baltimor <- PM25[PM25$fips=="24510",]
aggBaltimor <- aggregate(Emissions ~ year, Baltimor,sum)

# -----  aggregation for plot4
combRel <- grepl("comb", Source_Class$SCC.Level.One, ignore.case=TRUE)
coalRel <- grepl("coal", Source_Class$SCC.Level.Four, ignore.case=TRUE) 
Combination <- (combRel & coalRel)
CombinationSC <- Source_Class[Combination,]$SCC
SuperCombination <- PM25[PM25$SCC %in% CombinationSC,]

# -----  aggregation for plot5, plot6
veh <- grepl("vehicle", Source_Class$SCC.Level.Two, ignore.case=TRUE)
vehSC <- Source_Class[veh,]$SCC
vehNEI <- PM25[PM25$SCC %in% vehSC,]
BaltimorNEI <- vehNEI[vehNEI$fips=="24510",]
BaltimorNEI$city <- "Baltimor"
LANEI <- vehNEI[vehNEI$fips=="06037",]
LANEI$city <- "Los Angeles"
bothcity <- rbind(BaltimorNEI,LANEI)

# -----  Create namespace
dev.new()
par(mfcol = c(1,1))

# -----  Create plot
barplot(
  (agg$Emissions)/10^6,
  names.arg=agg$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)

#------   Save our picture  -------
dev.copy(png, file="plot1.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()

# -----  Create plot2
barplot(
  aggBaltimor$Emissions,
  names.arg=aggBaltimor$year,
  xlab="Year",
  ylab="PM2.5 Emissions (Tons)",
  main="Total PM2.5 Emissions From all Baltimore City Sources"
)

#------   Save our picture  -------
dev.copy(png, file="plot2.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()

# -----  Create plot3
print(ggplot(Baltimor,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type")))

#------   Save our picture  -------
dev.copy(png, file="plot3.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()

# -----  Create plot4
print(ggplot(SuperCombination,aes(factor(year),Emissions/10^5)) +
        geom_bar(stat="identity",fill="grey",width=0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
        labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008")))

#------   Save our picture  -------
dev.copy(png, file="plot4.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()

# -----  Create plot5
print(ggplot(BaltimorNEI,aes(factor(year),Emissions)) +
        geom_bar(stat="identity",fill="grey",width=0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008")))

#------   Save our picture  -------
dev.copy(png, file="plot5.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dev.new()

# -----  Create plot6
print(ggplot(bothcity, aes(x=factor(year), y=Emissions, fill=city)) +
        geom_bar(aes(fill=year),stat="identity") +
        facet_grid(scales="free", space="free", .~city) +
        guides(fill=FALSE) + theme_bw() +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008")))

#------   Save our picture  -------
dev.copy(png, file="plot6.png", height=480, width=480,units="px",bg="transparent")
dev.off()



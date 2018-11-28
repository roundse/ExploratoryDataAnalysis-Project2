# Step 1: Load data and subset appropriately.
data <- readRDS("summarySCC_PM25.rds")
correct_years <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")

# Step 2: Since this question looks at coal combustion pollution, 
# extract coal as pollutant. This requires subsetting by
# the key.
key <- readRDS("Source_Classification_Code.rds")

# Merge key and correct years data by SCC.
keyed_data <- merge(correct_years,key,by="SCC")

# Look for coal and combustion in relevant columns to find
# coal combustion-related sources. Agnostic to order and
# case.
coal_comb <- keyed_data[grep("[Cc]omb.*[Cc]oal|[Cc]oal.*[Cc]omb",keyed_data$EI.Sector) |
                        grep("[Cc]omb.*[Cc]oal|[Cc]oal.*[Cc]omb",keyed_data$Short.Name),]

# Sum all observations of emissions by year, as before
em_by_yr <- tapply(coal_comb$Emissions,coal_comb$year,sum,na.rm=TRUE)

# Scale down emissions
em_by_yr <- em_by_yr/(10^6)

# Get the years
years <- names(em_by_yr)

# Step 3: Create a line plot showing the change in
# total coal combustion-related emissions over time.
plot(years,em_by_yr,
     col="blue",
     pch=16,
     ylim=c(0,ceiling(max(em_by_yr))),
     xlab="Year",
     ylab=expression('Total PM2.5 Coal-Comb. Emissions (Tons, 10'^6*')'),
     main="Change in Total Coal Combustion-Related \nPM2.5 Emissions Over 1999-2008 in the US")
lines(years,em_by_yr,col="blue")

# Add regression line.
abline(lm(em_by_yr~as.numeric(years)),col="blue",lty=2)

# Step 4: Save the line plot to a PNG device.
dev.copy(png,"plot4.png")
dev.off()
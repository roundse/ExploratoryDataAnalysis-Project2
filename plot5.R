# Step 1: Load data and subset appropriately.
data <- readRDS("summarySCC_PM25.rds")
correct_years <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")
balt <- subset(correct_years,fips=="24510")

# Step 2: Get all motor vehicles (i.e., those that
# are of type on-road.
mv <- subset(balt,balt$type == "ON-ROAD")

# Sum all observations of emissions by year, as before
em_by_yr <- tapply(mv$Emissions,mv$year,sum)
years <- names(em_by_yr)

# Step 3: Create a line plot showing the change in
# total coal combustion-related emissions over time.
plot(years,em_by_yr,
     col="blue",
     pch=16,
     ylim=c(0,ceiling(max(em_by_yr))),
     xlab="Year",
     ylab="Total PM2.5 Emissions from \nMotor Vehicle Sources (Tons)",
     main="Change in Total PM2.5 Motor Vehicle \nEmissions Over 1999-2008 in Baltimore City, MD")
lines(years,em_by_yr,col="blue")

# Add regression line.
abline(lm(em_by_yr~as.numeric(years)),col="blue",lty=2)

# Step 4: Save the line plot to a PNG device.
dev.copy(png,"plot5.png")
dev.off()
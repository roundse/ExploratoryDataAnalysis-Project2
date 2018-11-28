# Step 1: Load data and subset appropriately.
data <- readRDS("summarySCC_PM25.rds")
correct_years <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")


# Sum all observations of emissions by year (assumption: 'total'
# means sum.)
em_by_yr <- tapply(correct_years$Emissions,correct_years$year,sum,na.rm=TRUE)

# Scale down emissions
em_by_yr <- em_by_yr/(10^6)

# Get the years.
years <- names(em_by_yr)

# Step 2: Create a line plot showing the change in
# total emissions over time.
plot(years,em_by_yr,
     col="blue",
     pch=16,
     ylim=c(0,ceiling(max(em_by_yr))),
     xlab="Year",
     ylab=expression('Total PM2.5 Emissions (Tons, 10'^6*')'),
     main="Change in Total PM2.5 Emissions Over \n1999-2008 in the United States")
lines(years,em_by_yr,col="blue")

# Add regression line.
abline(lm(em_by_yr~as.numeric(years)),col="blue",lty=2)

# Step 3: Save the line plot to a PNG device.
dev.copy(png,"plot1.png")
dev.off()
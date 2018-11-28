# Step 1: Load data and subset appropriately.
data <- readRDS("summarySCC_PM25.rds")
correct_years <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")
balt <- subset(correct_years,fips=="24510")

# This question is essentially the same as the first one,
# so use the same method.

# Sum all observations of emissions by year (assumption: 'total'
# means sum.)
em_by_yr <- tapply(balt$Emissions,balt$year,sum,na.rm=TRUE)

# Get the years.
years <- names(em_by_yr)

# Step 2: Create a line plot showing the change in
# total emissions over time.
plot(years,em_by_yr,
     col="blue",
     alpha = 0.7,
     pch=16,
     ylim=c(0,ceiling(max(em_by_yr))),
     xlab="Year",
     ylab="Total PM2.5 Emissions (Tons)",
     main="Change in Total PM2.5 Emissions Over \n1999-2008 in Baltimore City, MD")
lines(years,em_by_yr,col="blue")

# Add regression line.
abline(lm(em_by_yr~as.numeric(years)),col="blue",lty=2)

# Step 3: Save the line plot to a PNG device.
dev.copy(png,"plot2.png")
dev.off()

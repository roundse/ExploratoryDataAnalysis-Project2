# This question requires ggplot2
library(ggplot2)

# Step 1: Load data and subset appropriately.
data <- readRDS("summarySCC_PM25.rds")
correct_years <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")
balt <- subset(correct_years,fips=="24510")

# Step 2: Create a plot with ggplot2.
p <- ggplot(balt, aes(x=as.numeric(year), y=Emissions, col=type)) +         # plot emissions by year, set colors by type.
        stat_summary(fun.y = "sum", geom = "line")  +                       # get sum of emissions by type, plot a line.
        stat_summary(fun.y = "sum", geom = "point") +                       # do the same for points
        scale_colour_manual(values = c("green", "blue","red","black")) +    # set colors for types manually.
        labs(title="Changes in Total PM2.5 Emissions in Baltimore \nCity, MD By Source Over 1999-2008",
               x="Year",y="Total PM2.5 Emissions (Tons)") +
        facet_grid(rows=vars(type))
print(p)

# Step 3: Save the plot.
dev.copy(png,"plot3.png")
dev.off()

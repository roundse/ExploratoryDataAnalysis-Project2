library(ggplot2)

# Step 1: Load data and subset appropriately, first by year and then by city.
data <- readRDS("summarySCC_PM25.rds")
correct <- subset(data,year=="1999" | year == "2002" | year == "2005" | year == "2008")
correct <- subset(correct, fips=="24510" | fips=="06037")

# Step 2: Since this question looks at motor vehicle emissions,
# extract motor vehicles as pollutant using type 'on-road.'
mv <- subset(correct,type=="ON-ROAD")

# Step 3: Create a plot comparing LA and Baltimore City.
p <- ggplot(mv, aes(x=year, y=Emissions,group=fips,shape=fips,col=fips)) +     # plot emissions by year, set colors by type.
  stat_summary(fun.y = "sum", geom = "line")  +                                # get sum of emissions by type, plot a line.
  stat_summary(fun.y = "sum", geom = "point") +                                # do the same for points
  labs(title="Comparison of Total Motor Vehicle PM2.5 Emissions Changes in
       Baltimore City, MD and Los Angeles, CA Over 1999-2008",
       x="Year",y="Total PM2.5 Emissions from \nMotor Vehicle Sources (Tons)") +
  facet_grid(rows=vars(fips)) +
  theme(strip.background = element_blank(), strip.text = element_blank()) + # remove facet labels
  scale_colour_manual(name="City",
                      values = c("green","red"),
                      labels=c("Los Angeles, CA","Baltimore City, MD")) +  # set legend colors manually
  scale_shape_discrete(name="City",
                       breaks=c("06037","24510"),
                       labels=c("Los Angeles, CA","Baltimore City, MD"))   # set legend key manually
print(p)

# Step 4: Save the plot.
dev.copy(png,"plot6.png")
dev.off()
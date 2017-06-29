library(readxl)

monthLevels <- c("August", "September", "October", "November", "December","January", "February", "March", "April", "May", "June", "July")
colOverrides <- cols(
  `Car Type` = col_factor(c("Passenger cars", "Trucks"))
  #`Export or Production` = col_factor(c("Export", "Production"))
)
germanCarProd <- read_excel("~/MakeoverMonday/GermanCars/GermanCarProductionAndExportsByMonth.xlsx") %>% 
    mutate(
      month = factor(months(Date), monthLevels)
    )
#Pattern for export v production and cars v trucks
#Dip in truck prod in 2009
ggplot(germanCarProd, aes(x=Date, y=`Number of cars`)) + geom_point() + geom_smooth() + facet_grid(`Car Type` ~ `Export or Production`, scales = "free_y")

germanCarProd %>% head() %>% View()

#Yearly cyccle - Dip in augusts
ggplot(germanCarProd %>% filter(`Export or Production` == "Production"), aes(x=Date, y=`Number of cars`)) + geom_point() + geom_smooth() + facet_grid(`Car Type` ~ month, scales = "free_y")
ggplot(germanCarProd %>% filter(`Export or Production` == "Export"), aes(x=Date, y=`Number of cars`)) + geom_point() + geom_smooth() + facet_grid(`Car Type` ~ month, scales = "free_y")


yearAvg <- germanCarProd %>% filter(`Car Type` == "Passenger cars") %>% filter(`Export or Production` == "Production") %>% 
  group_by(
    year = strftime(Date, format="%Y")
  ) %>%
  summarise(sumProd = mean(`Number of cars`))

ggplot() + geom_col(data=yearAvg, aes(x=as.POSIXct(year, format="%Y"), y=sumProd)) + geom_point(data= (germanCarProd %>% filter(`Car Type` == "Passenger cars") %>% filter(`Export or Production` == "Production")), aes(x=Date, y=`Number of cars`))

 yearDiff <- germanCarProd %>% filter(`Car Type` == "Passenger cars") %>% filter(`Export or Production` == "Production") %>% 
  group_by(
    year = strftime(Date, format="%Y")
  ) %>%
  mutate(
    yearAvg = quantile(`Number of cars`, 0.5),
    #yearAvg = mean(`Number of cars`),
    monthDiff = `Number of cars` - yearAvg
  )

 ggplot(yearDiff, aes(x=year, y=`Number of cars`)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + expand_limits(y=0)

 ( ggThing <- ggplot(yearDiff, aes(x=Date, y=yearAvg+monthDiff)) + 
    geom_col(aes(color=as.logical(monthDiff > 0))) +
    theme(legend.position="none") + scale_y_continuous(labels=comma)
) # Show output
ggThing + geom_crossbar(aes(x=Date, ymin=yearAvg, ymax=yearAvg))
ggThing + facet_grid(~month) #+ geom_smooth(method="lm")

library(scales)
ggplot(yearDiff, aes(x=Date, ymin=yearAvg, y=yearAvg, ymax=yearAvg+monthDiff, fill=as.logical(monthDiff > 0))) + 
  geom_crossbar() + scale_y_continuous(labels=comma) + scale_x_datetime() + xlab("Year") + ylab("Number of passenger cars produced") +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust=1)) + expand_limits(y=0) + ggtitle("Monthly production versus the yearly median") #+ facet_grid(~month)

ggplot(yearDiff, aes(x=as.POSIXct(year, format="%Y"), ymin=yearAvg, y=yearAvg, ymax=yearAvg+monthDiff, fill=as.logical(monthDiff > 0))) + 
  geom_crossbar() + scale_y_continuous(labels=comma) + scale_x_datetime() + xlab("Year") + ylab("Number of passenger cars produced") +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust=1)) + expand_limits(y=0) + ggtitle("Monthly production versus the yearly median") + facet_grid(~month)



monthDiff <- germanCarProd %>% filter(`Car Type` == "Passenger cars") %>% filter(`Export or Production` == "Production") %>% 
  group_by(
    month
  ) %>%
  mutate(
    monthAvg = quantile(`Number of cars`, 0.5),
    yearDiff = `Number of cars` - monthAvg,
    year = strftime(Date, format="%Y")
  )

ggplot(monthDiff, aes(x=as.POSIXct(year, format="%Y"), ymin=monthAvg, y=monthAvg, ymax=monthAvg+yearDiff, fill=as.logical(yearDiff > 0))) + 
  geom_crossbar() + scale_y_continuous(labels=comma) + scale_x_datetime() + xlab("Year") + ylab("Number of passenger cars produced") +
  theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust=1)) + expand_limits(y=0) + ggtitle("Production versus the monthly median") + facet_grid(~month)


# Clearing Lists
ls()
rm(list=ls())


library("RCurl")
library("jsonlite")
library("plyr")
library("lubridate")
library("psych")
library("magrittr")
library("ggplot2")
library("MASS")
library("rpart")
library("DMwR")
library("rpart.plot")
library("randomForest")
library("nnet")
library("stringr")
library("reshape2")
library("tseries")
library("forecast")
library("plyr")
library("dplyr")



####################
##   Chicago "L"  ##
##    API DATA    ##
####################

library("RCurl")
library("jsonlite")

##### store all pages in a list first
baseurl <- "https://data.cityofchicago.org/resource/5neh-572f.json"
pages <- list()
for(i in 0:13){
  mydata <- fromJSON(paste0(baseurl, "?$order=date&$limit=50000&$offset=", i*50000), 
                     flatten=TRUE)
  message("Retrieving page ", i)
  pages[[i+1]] <- mydata
}

# Total Records - Obtained Records = 712,938 - 700,000 = 12,938
# pages[[15]] <- fromJSON(paste0(baseurl, "?$order=date&$limit=12938&$offset=", 14*50000), 
#                         flatten=TRUE)

# str(mydata)
# head(mydata)
# 
# str(pages)
#head(pages)


##### combine all into one 
L.api <- rbind.fill(pages)


##### check output
nrow(L.api)
colnames(L.api)

str(L.api)
head(L.api)
tail(L.api)


##### convert date from "Epoc" to conventional date
L.api$date <- as.Date(as.POSIXct(L.api$date, origin="1970-01-01")
                      )

str(L.api)
head(arrange(L.api, date, stationname), 100)
tail(arrange(L.api, date, stationname), 100)


##### check station-date counts
table(L.api$date, L.api$stationname)

########## No Unique RowId in API data, so duplicates show up  -->  Try CVS Data
########## No Unique RowId in API data, so duplicates show up  -->  Try CVS Data
########## No Unique RowId in API data, so duplicates show up  -->  Try CVS Data


####################
##   Chicago "L"  ##
##    CSV DATA    ##
####################

##### read-in data from .csv file from GitHub
El <- getURL("https://raw.githubusercontent.com/supermdat/Chicago_El_Rides/master/Data/Raw%20Data/CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")
L.csv <- read.csv(text = El)

str(L.csv)
head(L.csv)


##### convert date from factor format to date format
library("lubridate")
L.csv$date <- as.Date(mdy(L.csv$date))

str(L.csv)
head(L.csv)


##### check station-date counts
table(L.csv$date, L.csv$stationname)


##### add variables for days (Sun = 1)
L.csv$day <- wday(L.csv$date)
L.csv$dayname <- wday(L.csv$date, label=TRUE)

str(L.csv)
head(L.csv)
tail(L.csv)


##### add variables for Holidays
L.csv$Holiday <- as.factor(ifelse(L.csv$daytype == "U" & L.csv$day != 1, 
                                   "Holiday", "Normal")
                            )


# Test on Thanksgiving Day 2013
head(filter(L.csv, date == "2013-11-28"))

str(L.csv)
head(L.csv)
tail(L.csv)


##### Add variables for date parts (e.g., year, month, etc.)
L.csv$date_year <- year(L.csv$date)
L.csv$date_month <- as.factor(month(L.csv$date, label = TRUE))
L.csv$date_day <- day(L.csv$date)

str(L.csv)
head(L.csv)
tail(L.csv)

save(L.csv, file = "L.csv.Rdata")
load("L.csv.Rdata")

####################
##     Chicago    ##
##  Weather Data  ##
####################

##### read-in data from .csv file from GitHub
Wthr <- getURL("https://raw.githubusercontent.com/supermdat/Chicago_El_Rides/master/Data/Raw%20Data/ChicagoWeather(2015-01-06).csv")
Weather <- read.csv(text = Wthr)

str(Weather)
head(Weather)
summary(Weather)


##### fill in -9999 in TOBS (using TMAX, TMIN, or average of TMAX and TMIN)
Weather$TOBS2 <- ifelse((Weather$TOBS==-9999 & Weather$TMAX==-9999 & Weather$TMIN==-9999), 
                         88888,
                 ifelse((Weather$TOBS==-9999 & Weather$TMAX==-9999 & Weather$TMIN!=-9999), 
                         Weather$TMIN,
                 ifelse((Weather$TOBS==-9999 & Weather$TMAX!=-9999 & Weather$TMIN==-9999), 
                         Weather$TMAX,
                 ifelse((Weather$TOBS==-9999 & Weather$TMAX!=-9999 & Weather$TMIN!=-9999), 
                         ((Weather$TMAX + Weather$TMIN) / 2), 
                        Weather$TOBS))))


##### fill in -9999 in TMAX (using TOBS, TMIN, or average of TOBS and TMIN)
Weather$TMAX2 <- ifelse((Weather$TOBS==-9999 & Weather$TMAX==-9999 & Weather$TMIN==-9999), 
                         88888,
                 ifelse((Weather$TOBS==-9999 & Weather$TMAX==-9999 & Weather$TMIN!=-9999), 
                         Weather$TMIN,
                 ifelse((Weather$TOBS!=-9999 & Weather$TMAX==-9999 & Weather$TMIN==-9999), 
                         Weather$TOBS,
                 ifelse((Weather$TOBS!=-9999 & Weather$TMAX==-9999 & Weather$TMIN!=-9999), 
                         ((Weather$TOBS + Weather$TMIN) / 2), 
                         Weather$TMAX))))


##### fill in -9999 in TMIN (using TOBS, TMAX, or average of TOBS and TMAX)
Weather$TMIN2 <- ifelse((Weather$TOBS==-9999 & Weather$TMAX==-9999 & Weather$TMIN==-9999), 
                         88888,
                 ifelse((Weather$TOBS!=-9999 & Weather$TMAX==-9999 & Weather$TMIN==-9999), 
                         Weather$TOBS,
                 ifelse((Weather$TOBS==-9999 & Weather$TMAX!=-9999 & Weather$TMIN==-9999), 
                         Weather$TMAX,
                 ifelse((Weather$TOBS!=-9999 & Weather$TMAX!=-9999 & Weather$TMIN==-9999), 
                         ((Weather$TOBS + Weather$TMAX) / 2), 
                         Weather$TMIN))))

test <- (Weather[ , c("DATE", "TOBS2", "TMAX2", "TMIN2", "TOBS", "TMAX", "TMIN")])
str(test)
#colnames(test)
head(filter(test, TOBS==-9999, TMAX!=-9999, TMIN==-9999))


##### Convert temperatures to Farenheight
Weather$TMIN_F <- ifelse(Weather$TMIN2!=88888, (((Weather$TMIN2)/10*9/5) + 32), 
                         Weather$TMIN2)
Weather$TMAX_F <- ifelse(Weather$TMAX2!=88888, (((Weather$TMAX2)/10*9/5) + 32), 
                         Weather$TMAX2)
Weather$TOBS_F <- ifelse(Weather$TOBS2!=88888, (((Weather$TOBS2)/10*9/5) + 32), 
                         Weather$TOBS2)


##### Introduce "blanks" for "88888
Weather$TMIN_F <- as.numeric(ifelse(Weather$TMIN_F==88888, "", Weather$TMIN_F))
Weather$TMAX_F <- as.numeric(ifelse(Weather$TMAX_F==88888, "", Weather$TMAX_F))
Weather$TOBS_F <- as.numeric(ifelse(Weather$TOBS_F==88888, "", Weather$TOBS_F))

test2 <- (Weather[ , c("DATE", "TOBS_F", "TMAX_F", "TMIN_F", "TOBS", "TMAX", "TMIN")])
head(filter(test2, TOBS==-9999, TMAX!=-9999, TMIN!=-9999))

str(Weather)
head(Weather)
summary(Weather)


##### Introduce "blanks" for "-9999" for:  PRCP, SNOW, and SNWD (and convert measurement to inches)
##### 1inch = 25.4mm
Weather$PRCP2_inch <- as.numeric(ifelse(Weather$PRCP==-9999, "", (Weather$PRCP/10/25.4)))
Weather$SNOW2_inch <- as.numeric(ifelse(Weather$SNOW==-9999, "", Weather$SNOW/25.4))
Weather$SNWD2_inch <- as.numeric(ifelse(Weather$SNWD==-9999, "", Weather$SNWD/25.4))

str(Weather)
head(Weather)
summary(Weather)


##### convert DATE from integer format to date format
library("lubridate")
Weather$DATE <- as.Date(ymd(Weather$DATE))

str(Weather)
head(Weather)
summary(Weather)

save(Weather, file = "Weather.Rdata")
load("Weather.Rdata")


##### take averages per day
DailyWeather <- Weather %>%
  group_by(DATE) %>%
  summarize(
    AvgDayTempMIN_F = mean(TMIN_F, na.rm=TRUE),
    AvgDayTempMAX_F = mean(TMAX_F, na.rm=TRUE),
    AvgDayTempOBS_F = mean(TOBS_F, na.rm=TRUE),
    AvgDayPRCP_Inch = mean(PRCP2_inch, na.rm=TRUE),
    AvgDaySNOW_Inch = mean(SNOW2_inch, na.rm=TRUE),
    AvgDaySNWD_Inch = mean(SNWD2_inch, na.rm=TRUE)
   )

str(DailyWeather)
head(DailyWeather)
tail(DailyWeather)
summary(DailyWeather)

# Test for no missing values
summary(DailyWeather)
nrow(is.na(DailyWeather$AvgDaySNOW_Inch))
for (name in colnames(DailyWeather)) {
  testing <- paste(1:length(colnames(DailyWeather)), nrow(is.na(name)), sep=":")
}
testing

save(DailyWeather, file = "DailyWeather.Rdata")
load("DailyWeather.Rdata")


##### merge "L" data and weather data
str(L.csv)
str(DailyWeather)

CompleteData <- merge(x=L.csv, y=DailyWeather, by.x="date", by.y="DATE", all.x=TRUE)

str(CompleteData)
head(CompleteData)
summary(CompleteData)

##### remove unneeded or duplicate variables
CompleteData_Select <- select(CompleteData, date
                                           ,rides
                                           ,stationname
                                           ,date_year
                                           ,date_month
                                           ,date_day
                                           ,dayname
                                           ,Holiday
                                           ,AvgDayTempMIN_F
                                           ,AvgDayTempMAX_F
                                           ,AvgDayTempOBS_F
                                           ,AvgDayPRCP_Inch
                                           ,AvgDaySNOW_Inch
                                           ,AvgDaySNWD_Inch
                              )
                       
str(CompleteData_Select)
head(CompleteData_Select)
summary(CompleteData_Select)

save(CompleteData_Select, file = "CompleteData_Select.Rdata")
load("CompleteData_Select.Rdata")



########################
##   Create Training  ##
##    and Test Data   ##
########################

# Training Data (aproximately 73% of CompleteData_Select)
TrainingData <- filter(CompleteData_Select, date < "2011-01-01")
TrainingData$stationname <- factor(TrainingData$stationname)
str(TrainingData)
head(TrainingData)
summary(TrainingData)
nrow(TrainingData)

save(TrainingData, file = "TrainingData.Rdata")
load("TrainingData.Rdata")


# Test Data (approximatley 27% of CompleteData_Select)
TestData <- filter(CompleteData_Select, date >= "2011-01-01")
TestData$stationname <- factor(TestData$stationname)
str(TestData)
summary(TestData)
nrow(TestData)

save(TestData, file = "TestData.Rdata")
load("TestData.Rdata")


# Equate number of factors in "stationname" between Training Data & Test Data
str(TrainingData)
str(TestData)
str(TrainingData$stationname)
str(TestData$stationname)

TrainingData2 <- filter(TrainingData, stationname %in% TestData$stationname)
TrainingData2$stationname <- factor(TrainingData2$stationname)
str(TrainingData2)
save(TrainingData2, file = "TrainingData2.Rdata")
load("TrainingData2.Rdata")

TestData2 <- filter(TestData, stationname %in% TrainingData2$stationname)
TestData2$stationname <- factor(TestData2$stationname)
str(TestData2)
save(TestData2, file = "TestData2.Rdata")
load("TestData2.Rdata")


# Training Data with the LOG of Rides
TrainingData2_LOG <- TrainingData2
TrainingData2_LOG$ridesLOG <- log(1+(TrainingData2_LOG$rides))
str(TrainingData2_LOG)
head(TrainingData2_LOG)
summary(TrainingData2_LOG)

save(TrainingData2_LOG, file = "TrainingData2_LOG.Rdata")
load("TrainingData2_LOG.Rdata")


# TestData with the LOG of Rides
TestData2_LOG <- TestData2
TestData2_LOG$ridesLOG <- log(1+(TestData2_LOG$rides))
str(TestData2_LOG)
head(TestData2_LOG)
summary(TestData2_LOG)

save(TestData2_LOG, file = "TestData2_LOG.Rdata")
load("TestData2_LOG.Rdata")



#########################
##   Data Exploration  ##
#########################

# Histograms & Quintile Plots
# Histograms & Quintile Plots
# Histograms & Quintile Plots

# Histogram and Quintile Plot (Rides NORMAL)
g1 <- ggplot(TrainingData2, aes(x=rides, y=..density..)) +
  geom_histogram(binwidth=500, fill="lightblue", colour="grey60", size=0.2) +
  geom_line(stat="density", colour="red") +
  xlim(-0,20000) +
  xlab("Number of L Rides") + 
  ylab("Density") + 
  ggtitle(expression(atop("Variation in L Rides")))


ggQQ <- function(TrainingData2) # argument: a linear model
{
  y     <- quantile(na.omit(TrainingData2$rides), c(0.25, 0.75))
  x     <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int   <- y[1L] - slope * x[1L]
  p     <- ggplot(TrainingData2, aes(sample=rides)) + 
           stat_qq(alpha = 0.5) +
           geom_abline(slope = slope, intercept = int, color="red") + 
           ggtitle(expression(atop("QQ Plot for L Rides")))
  
  return(p)
}
g2 <- ggQQ(TrainingData2)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

save(multiplot, file = "multiplot.Rdata")
load("multiplot.Rdata")


Plot_HistoQuin_Norm <- multiplot(g1, g2,cols=2)

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_HistoQuin_Norm.jpg", scale=1.5)



# Histogram and Quintile Plot (Rides LOGGED)
g3 <- ggplot(TrainingData2_LOG, aes(x=ridesLOG, y=..density..)) +
  geom_histogram(binwidth=0.25, fill="lightblue", colour="grey60", size=0.2) +
  geom_line(stat="density", colour="red") +
  xlim(4,10) +
  xlab("Number of L Rides (logged)") + 
  ylab("Density") + 
  ggtitle(expression(atop("Variation in L Rides (logged)")))


ggQQ <- function(TrainingData2_LOG) # argument: a linear model
{
  y     <- quantile(na.omit(TrainingData2_LOG$ridesLOG), c(0.25, 0.75))
  x     <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int   <- y[1L] - slope * x[1L]
  p     <- ggplot(TrainingData2_LOG, aes(sample=ridesLOG)) + 
           stat_qq(alpha = 0.5) +
           geom_abline(slope = slope, intercept = int, color="red") + 
           ggtitle(expression(atop("QQ Plot for L Rides (logged)")))
  
  return(p)
}
g4 <- ggQQ(TrainingData2_LOG)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

Plot_HistoQuin_LOG <- multiplot(g3, g4,cols=2)

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_HistoQuin_LOG.jpg", scale=1.5)



##### Counts of Missing Data Per Column
##### Counts of Missing Data Per Column
##### Counts of Missing Data Per Column
apply(TrainingData2, 2, function(x) sum(is.na(x)))


##### Basic Summary Statistics
##### Basic Summary Statistics
##### Basic Summary Statistics
summary(TrainingData2)
describe(TrainingData2)


##### Box Plots Factored by Day of Week
##### Box Plots Factored by Day of Week
##### Box Plots Factored by Day of Week
TrainingData2_Day <- TrainingData2 %>%
  group_by(date, dayname) %>%
  summarize(
    rides_n = sum(rides)
    )

Count_Values <- ddply(TrainingData2_Day, .(dayname), summarise, 
                      Value_Counts = median(rides_n, na.rm = TRUE)
                      )

BoxPlot_Day <- ggplot(TrainingData2_Day, aes(factor(dayname), rides_n, 
                                             fill=factor(dayname)
                                            )
                      ) + 
  geom_boxplot(outlier.colour="red", notch=TRUE) + 
  coord_cartesian(ylim = c(150000, 700000)) + 
  geom_text(data=Count_Values, aes(y=Value_Counts, label=round(Value_Counts,0)), 
            size = 3, vjust = -0.5) +
  xlab("Day of Week") + 
  ylab("Median Daily L Rides") + 
  theme(legend.position="none") + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("Median Daily L Rides (by Day of Week)")

BoxPlot_Day

ggsave("/Users/mdturse/Google Drive/TimeSeries/BoxPlot_Day.jpg", scale=1.5)


##### Box Plots Factored by Holiday
##### Box Plots Factored by Holiday
##### Box Plots Factored by Holiday
TrainingData2_Holiday <- TrainingData2 %>%
  group_by(date, Holiday) %>%
  summarize(
    rides_n = sum(rides)
  )

Count_Values <- ddply(TrainingData2_Holiday, .(Holiday), summarise, 
                      Value_Counts = median(rides_n, na.rm = TRUE)
                      )

BoxPlot_Holiday <- ggplot(TrainingData2_Holiday, aes(factor(Holiday), rides_n, 
                                                     fill=factor(Holiday)
                                                    )
                          ) + 
  geom_boxplot(outlier.colour="red", notch=TRUE) + 
  coord_cartesian(ylim = c(50000, 800000)) + 
  geom_text(data=Count_Values, aes(y=Value_Counts, label=round(Value_Counts,0)), 
            size = 3, vjust = -0.5) +
  xlab("Day of Week") + 
  ylab("Median Daily L Rides") + 
  theme(legend.position="none") + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("Median Daily L Rides (by Holiday/Regular Day)")

BoxPlot_Holiday

ggsave("/Users/mdturse/Google Drive/TimeSeries/BoxPlot_Holiday.jpg", scale=1.5)


##### Box Plots Factored by Day of Week & Holiday
##### Box Plots Factored by Day of Week & Holiday
##### Box Plots Factored by Day of Week & Holiday
TrainingData2_DayHoliday <- TrainingData2 %>%
  group_by(date, Holiday, dayname) %>%
  summarize(
    rides_n = sum(rides)
  )
TrainingData2_DayHoliday$Holiday_Day <- paste(TrainingData2_DayHoliday$dayname, 
                                              TrainingData2_DayHoliday$Holiday, sep="-"
                                              )
TrainingData2_DayHoliday <- TrainingData2_DayHoliday[ , c(1, 5, 4)]
TrainingData2_DayHoliday


Count_Values <- ddply(TrainingData2_DayHoliday, .(Holiday_Day), summarise, 
                      Value_Counts = median(rides_n, na.rm = TRUE)
                      )

BoxPlot_DayHoliday <- ggplot(TrainingData2_DayHoliday, aes(factor(Holiday_Day), 
                                                           rides_n, 
                                                           fill=factor(Holiday_Day)
                                                          )
                             ) + 
  geom_boxplot(outlier.colour="red") + 
  coord_cartesian(ylim = c(25000, 700000)) + 
  geom_text(data=Count_Values, aes(y=Value_Counts, label=round(Value_Counts,0)), 
            size = 3, vjust = -0.5) +
  xlab("Day of Week & Holiday") + 
  ylab("Median Daily L Rides") + 
  theme(legend.position="none", axis.text.x = element_text(angle=45)) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("Median Daily L Rides (by Day of Week & Holiday)") +
  scale_x_discrete(limits = c(
                               "Sun-Normal"
                              #,"Sun-Holiday"
                              ,"Mon-Normal"
                              ,"Mon-Holiday"
                              ,"Tues-Normal"
                              ,"Tues-Holiday"
                              ,"Wed-Normal"
                              ,"Wed-Holiday"
                              ,"Thurs-Normal"
                              ,"Thurs-Holiday"
                              ,"Fri-Normal"
                              ,"Fri-Holiday"
                              ,"Sat-Normal"
                              ,"Sat-Holiday"
                              )
                   )

BoxPlot_DayHoliday

ggsave("/Users/mdturse/Google Drive/TimeSeries/BoxPlot_DayHoliday.jpg", scale=1.5)



#####################
##   Linear Model  ##
##  All Variables  ##
#####################

##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)

fit_NORM <- lm(rides ~ ., data = TrainingData2)
step_NORM <- stepAIC(fit_NORM, direction = "both")

save(step_NORM, file = "step_NORM.Rdata")
load("step_NORM.Rdata")

summary(step_NORM)
step_NORM$anova


##### QQ Plot of Residuals
qqnorm(residuals(step_NORM)
       )
qqline(residuals(step_NORM)
       )


##### Predictions TRAIN
step_NORM.predictions <- predict(step_NORM, TrainingData2)
save(step_NORM.predictions, file = "step_NORM.predictions.Rdata")
load("step_NORM.predictions.Rdata")


##### Predictions TEST
step_NORM.predictions.Test <- predict(step_NORM, TestData2)
save(step_NORM.predictions.Test, file = "step_NORM.predictions.Test.Rdata")
load("step_NORM.predictions.Test.Rdata")


##### LOG data
##### LOG data
##### LOG data
str(TrainingData2_LOG)

fit_LOG <- lm(ridesLOG ~ . - rides, data = TrainingData2_LOG)
step_LOG <- stepAIC(fit_LOG, direction = "both")

save(step_LOG, file = "step_LOG.Rdata")
load("step_LOG.Rdata")

summary(step_LOG)
step_LOG$anova


##### QQ Plot of Residuals
qqnorm(residuals(step_LOG)
       )
qqline(residuals(step_LOG)
       )


##### Predictions TRAINING
step_LOG.predictions <- predict(step_LOG, TrainingData2_LOG)
save(step_LOG.predictions, file = "step_LOG.predictions.Rdata")
load("step_LOG.predictions.Rdata")

##### Predictions TEST
step_LOG.predictions.Test <- predict(step_LOG, TestData2_LOG)
save(step_LOG.predictions.Test, file = "step_LOG.predictions.Test.Rdata")
load("step_LOG.predictions.Test.Rdata")



########################
##   Regression Tree  ##
########################

##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)

##### Minimize _____
rt.Norm <- rpart(rides ~ ., data = TrainingData2)
rt.Norm
save(rt.Norm, file = "rt.Norm.Rdata")
load("rt.Norm.Rdata")

prettyTree(rt.Norm)
printcp(rt.Norm)
plotcp(rt.Norm) # visualize cross-validation results
summary(rt.Norm) # detailed summary of splits
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(rt.Norm) # visualize cross-validation results
par(mfrow=c(1,1)) # reset plots


##### Predictions TRAINING
rt.Norm.predictions <- predict(rt.Norm, TrainingData2)
save(rt.Norm.predictions, file = "rt.Norm.predictions.Rdata")
load("rt.Norm.predictions.Rdata")


##### Predictions TEST
rt.Norm.predictions.Test <- predict(rt.Norm, TestData2)
save(rt.Norm.predictions.Test, file = "rt.Norm.predictions.Test.Rdata")
load("rt.Norm.predictions.Test.Rdata")


##### Within One Standard Error  --  RUNS TOO SLOWLY !!!!!
##### Within One Standard Error  --  RUNS TOO SLOWLY !!!!!
##### Within One Standard Error  --  RUNS TOO SLOWLY !!!!!
##### Within One Standard Error  --  RUNS TOO SLOWLY !!!!!

# rt.Norm.1SE <- rpartXse (rides ~ ., data = TrainingData2, method = "anova")
# rt.Norm.1SE
# save(rt.Norm.1SE, file = "rt.Norm.1SE.Rdata")
# load("rt.Norm.1SE.Rdata")
# 
# prettyTree(rt.Norm.1SE, font = 10, cex = .5)
# printcp(rt.Norm.1SE)
# plotcp(rt.Norm.1SE) # visualize cross-validation results
# summary(rt.Norm.1SE) # detailed summary of splits
# par(mfrow=c(1,2)) # two plots on one page
# rsq.rpart(rt.Norm.1SE) # visualize cross-validation results
# par(mfrow=c(1,1)) # reset plots


heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(tree, branch.col=cols, box.col=cols, ...)
}

heat.tree.lm1 <- (heat.tree(rt.Norm, gap = 0, space = 0, type = 2, extra = 101, 
                            varlen = 15, faclen = 0, tweak = 3.0, compress = FALSE, 
                            ycompress = FALSE
                            )
                  )

heat.tree.lm2 <- (heat.tree(rt.Norm, gap = 0, space = 0, type = 2, extra = 101, 
                            varlen = 15, faclen = 0, tweak = 3.0, compress = TRUE, 
                            ycompress = FALSE, fallen.leaves = TRUE
                            )
                  )

heat.tree.lm1
heat.tree.lm2


##### LOG data
##### LOG data
##### LOG data
str(TrainingData2_LOG)

##### Minimize _____
rt.LOG <- rpart(ridesLOG ~ . - rides, data = TrainingData2_LOG)
rt.LOG
save(rt.LOG, file = "rt.LOG.Rdata")
load("rt.LOG.Rdata")

prettyTree(rt.LOG)
printcp(rt.LOG)
plotcp(rt.LOG) # visualize cross-validation results
summary(rt.LOG) # detailed summary of splits
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(rt.LOG) # visualize cross-validation results
par(mfrow=c(1,1)) # reset plots


##### Predictions TRAINING
rt.LOG.predictions <- predict(rt.LOG, TrainingData2_LOG)
save(rt.LOG.predictions, file = "rt.LOG.predictions.Rdata")
load("rt.LOG.predictions.Rdata")


##### Predictions TEST
rt.LOG.predictions.Test <- predict(rt.LOG, TestData2_LOG)
save(rt.LOG.predictions.Test, file = "rt.LOG.predictions.Test.Rdata")
load("rt.LOG.predictions.Test.Rdata")


heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(tree, branch.col=cols, box.col=cols, ...)
}

heat.tree.rt1 <- (heat.tree(rt.LOG, gap = 0, space = 0, type = 2, extra = 101, varlen = 15, 
                         faclen = 0, tweak = 3.0, compress = FALSE, ycompress = FALSE
                         )
                  )
heat.tree.rt2 <- (heat.tree(rt.LOG, gap = 0, space = 0, type = 2, extra = 101, varlen = 15, 
                         faclen = 0, tweak = 3.0, compress = TRUE, ycompress = FALSE, 
                         fallen.leaves = TRUE
                         )
                  )
heat.tree.rt1
heat.tree.rt2



#######################
##   Random Forest   ##
#######################

##### Random Forest can only handle Factor variables with up to 53 choices
#####     so...find the 50 stations with the highest daily ridership...
DailyStationRides_Avg <- TrainingData2 %>%
  group_by(stationname) %>%
  summarize(
    rides_avg = mean(rides)
  )

top_stations <- arrange(DailyStationRides_Avg, desc(rides_avg))
str(top_stations)

top_stations_50 <- top_stations[1:50, ]
str(top_stations_50)
top_stations_50

save(top_stations_50, file = "top_stations_50.Rdata")
load("top_stations_50.Rdata")


##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)


##### Subset the Training Data to Top 50 Stations
TrainingData2_Top50Stations <- subset(TrainingData2, 
                                     stationname %in% top_stations_50$stationname
                                     )

str(TrainingData2_Top50Stations)
head(TrainingData2_Top50Stations)
summary(TrainingData2_Top50Stations)

TrainingData2_Top50Stations$stationname <- factor(TrainingData2_Top50Stations$stationname)

str(TrainingData2_Top50Stations)

##### Verify number of unique stations
distinct(select(TrainingData2_Top50Stations, stationname))
nrow(distinct(select(TrainingData2_Top50Stations, stationname)))

save(TrainingData2_Top50Stations, file = "TrainingData2_Top50Stations.Rdata")
load("TrainingData2_Top50Stations.Rdata")


##### 10% Sample
TrainingData2_Top50_Samp <- TrainingData2_Top50Stations[sample(nrow(TrainingData2_Top50Stations), 
                                                             0.10*(nrow(TrainingData2_Top50Stations)), 
                                                             replace = FALSE), ]

str(TrainingData2_Top50_Samp)

save(TrainingData2_Top50_Samp, file = "TrainingData2_Top50_Samp.Rdata")
load("TrainingData2_Top50_Samp.Rdata")


##### Subset the Test Data to Top 50 Stations
TestData2_Top50Stations <- subset(TestData2, 
                                  stationname %in% top_stations_50$stationname
                                  )

str(TestData2_Top50Stations)
head(TestData2_Top50Stations)
summary(TestData2_Top50Stations)

TestData2_Top50Stations$stationname <- factor(TestData2_Top50Stations$stationname)

str(TestData2_Top50Stations)

##### Verify number of unique stations
distinct(select(TestData2_Top50Stations, stationname))
nrow(distinct(select(TestData2_Top50Stations, stationname)))

save(TestData2_Top50Stations, file = "TestData2_Top50Stations.Rdata")
load("TestData2_Top50Stations.Rdata")


# ##### 30% Sample
# TestData2_Top50_Samp <- TestData2_Top50Stations[sample(nrow(TestData2_Top50Stations), 
#                                                        0.30*(nrow(TestData2_Top50Stations)), 
#                                                        replace = FALSE), ]
# 
# str(TestData2_Top50_Samp)
# 
# save(TestData2_Top50_Samp, file = "TestData2_Top50_Samp.Rdata")
# load("TestData2_Top50_Samp.Rdata")




##### Run the randomForest function
rf.Norm <- randomForest(rides ~ ., data = TrainingData2_Top50_Samp, ntree = 200, 
                        keep.forest = TRUE, importance = TRUE
                        )

rf.Norm
summary(rf.Norm)
save(rf.Norm, file = "rf.Norm.Rdata")
load("rf.Norm.Rdata")

##### Plotting Variable Importance
##### Plotting Variable Importance
##### Plotting Variable Importance
rf.Imp.Norm <- as.data.frame(importance(rf.Norm, scale = TRUE))
colnames(rf.Imp.Norm)[1] <- "PctIncMSE"
rf.Imp.Norm$Variable <- rownames(rf.Imp.Norm)
rf.Imp.Norm <- arrange(rf.Imp.Norm, desc(PctIncMSE))
rf.Imp.Norm

varImpPlot.MSE <- varImpPlot(rf.Norm, sort = TRUE, scale = TRUE, type = 1)
varImpPlot.MSE
varImpPlot.Impurity <- varImpPlot(rf.Norm, sort = TRUE, scale = TRUE, type = 2)
varImpPlot.Impurity

varImpPlot.MSE.Color <- ggplot(data = rf.Imp.Norm, aes(y = PctIncMSE, 
                                                      x = factor(Variable), 
                                                      fill = PctIncMSE
                                                      )
                               ) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = rf.Imp.Norm$Variable) + 
  xlab("Variable") + 
  ylab("Pct Increase in MSE if Variable Removed") + 
  theme(legend.position="none", axis.text.x = element_text(angle=30)) + 
  theme(legend.position="none") + 
  ggtitle("Variable Importance for Predicting L Ridership") + 
  scale_fill_gradient2(limits = c(0, 300), mid = "red", high = "blue")

varImpPlot.MSE.Color

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_RF_VarImp_MSE.jpg", scale=1.5)


varImpPlot.Node.Color <- ggplot(data = rf.Imp.Norm, 
                                aes(y = IncNodePurity, x = reorder(Variable, 
                                                                   -IncNodePurity
                                                                   ),
                                    fill = IncNodePurity
                                    )
                                ) + 
  geom_bar(stat = "identity") + 
  #scale_x_discrete(limits = rf.Imp.Norm$Variable) + 
  xlab("Variable") + 
  ylab("Decrease in Node Impurity (SSE) from Removing Variable") + 
  theme(legend.position="none", axis.text.x = element_text(angle=30)) + 
  theme(legend.position="none") + 
  ggtitle("Variable Importance for Predicting L Ridership") + 
  scale_fill_gradient2(limits = c(0, 125000000000), mid = "red", high = "blue")

varImpPlot.Node.Color

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_RF_VarImp_Node.jpg", scale=1.5)


##### Predictions TRAINING
rf.Norm.predictions <- predict(rf.Norm, TrainingData2_Top50_Samp)
save(rf.Norm.predictions, file = "rf.Norm.predictions.Rdata")
load("rf.Norm.predictions.Rdata")


##### Predictions TEST
rf.Norm.predictions.Test <- predict(rf.Norm, TestData2_Top50Stations)
save(rf.Norm.predictions.Test, file = "rf.Norm.predictions.Test.Rdata")
load("rf.Norm.predictions.Test.Rdata")



##### LOG data
##### LOG data
##### LOG data
str(TrainingData2_LOG)


##### Subset the Training Data to Top 50 Stations
TrainingData2_LOG_Top50Stations <- subset(TrainingData2_LOG, 
                                         stationname %in% top_stations_50$stationname
                                         )

str(TrainingData2_LOG_Top50Stations)
head(TrainingData2_LOG_Top50Stations)
summary(TrainingData2_LOG_Top50Stations)

TrainingData2_LOG_Top50Stations$stationname <- factor(TrainingData2_LOG_Top50Stations$stationname)

str(TrainingData2_LOG_Top50Stations)

##### Verify number of unique stations
distinct(select(TrainingData2_LOG_Top50Stations, stationname))
nrow(distinct(select(TrainingData2_LOG_Top50Stations, stationname)))

save(TrainingData2_LOG_Top50Stations, file = "TrainingData2_LOG_Top50Stations.Rdata")
load("TrainingData2_LOG_Top50Stations.Rdata")


##### 10% Sample
TrainingData2_LOG_Top50_Samp <- TrainingData2_LOG_Top50Stations[sample(nrow(TrainingData2_LOG_Top50Stations), 
                                                                     0.10*(nrow(TrainingData2_LOG_Top50Stations)), 
                                                                     replace = FALSE), ]

str(TrainingData2_LOG_Top50_Samp)

save(TrainingData2_LOG_Top50_Samp, file = "TrainingData2_LOG_Top50_Samp.Rdata")
load("TrainingData2_LOG_Top50_Samp.Rdata")



##### Subset the Test Data to Top 50 Stations
TestData2_LOG_Top50Stations <- subset(TestData2_LOG, 
                                      stationname %in% top_stations_50$stationname
                                      )

str(TestData2_LOG_Top50Stations)
head(TestData2_LOG_Top50Stations)
summary(TestData2_LOG_Top50Stations)

TestData2_LOG_Top50Stations$stationname <- factor(TestData2_LOG_Top50Stations$stationname)

str(TestData2_LOG_Top50Stations)

##### Verify number of unique stations
distinct(select(TestData2_LOG_Top50Stations, stationname))
nrow(distinct(select(TestData2_LOG_Top50Stations, stationname)))

save(TestData2_LOG_Top50Stations, file = "TestData2_LOG_Top50Stations.Rdata")
load("TestData2_LOG_Top50Stations.Rdata")


# ##### 30% Sample
# TestData2_LOG_Top50_Samp <- TestData2_LOG_Top50Stations[sample(nrow(TestData2_LOG_Top50Stations), 
#                                                                0.30*(nrow(TestData2_LOG_Top50Stations)), 
#                                                                replace = FALSE), ]
# 
# str(TestData2_LOG_Top50_Samp)
# 
# save(TestData2_LOG_Top50_Samp, file = "TestData2_LOG_Top50_Samp.Rdata")
# load("TestData2_LOG_Top50_Samp.Rdata")


##### Run the randomForest function
rf.LOG <- randomForest(ridesLOG ~ . - rides, data = TrainingData2_LOG_Top50_Samp, 
                       ntree = 200, keep.forest = TRUE, importance = TRUE
                       )

rf.LOG
save(rf.LOG, file = "rf.LOG.Rdata")
load("rf.LOG.Rdata")


##### Predictions
rf.LOG.predictions <- predict(rf.LOG, TrainingData2_LOG_Top50_Samp)
save(rf.LOG.predictions, file = "rf.LOG.predictions.Rdata")
load("rf.LOG.predictions.Rdata")


##### Predictions TEST
rf.LOG.predictions.Test <- predict(rf.LOG, TestData2_LOG_Top50Stations)
save(rf.LOG.predictions.Test, file = "rf.LOG.predictions.Test.Rdata")
load("rf.LOG.predictions.Test.Rdata")


####################
##   Neural Net   ##
####################

##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)


##### 5% Sample
##### 5% Sample
##### 5% Sample
TrainingData2_Samp <- TrainingData2[sample(nrow(TrainingData2), 
                                           0.05*(nrow(TrainingData2)), 
                                           replace = FALSE
                                           ), 
                                    ]

str(TrainingData2_Samp)
head(TrainingData2_Samp)
summary(TrainingData2_Samp)

save(TrainingData2_Samp, file = "TrainingData2_Samp.Rdata")
load("TrainingData2_Samp.Rdata")


##### Scaling Normal Data
##### Scaling Normal Data
##### Scaling Normal Data
TrainingData2_Samp_Scaled <- TrainingData2_Samp

TrainingData2_Samp_Scaled[ , c( "rides"
                         ,"AvgDayTempMIN_F"
                         ,"AvgDayTempMAX_F"
                         ,"AvgDayTempOBS_F"
                         ,"AvgDayPRCP_Inch"
                         ,"AvgDaySNOW_Inch"
                         ,"AvgDaySNWD_Inch"
                         )
                    ] <- scale(TrainingData2_Samp_Scaled[ , c( "rides"
                                                        ,"AvgDayTempMIN_F"
                                                        ,"AvgDayTempMAX_F"
                                                        ,"AvgDayTempOBS_F"
                                                        ,"AvgDayPRCP_Inch"
                                                        ,"AvgDaySNOW_Inch"
                                                        ,"AvgDaySNWD_Inch"
                                                        )
                                                   ]
                               )

str(TrainingData2_Samp_Scaled)
head(TrainingData2_Samp_Scaled)
summary(TrainingData2_Samp_Scaled)

save(TrainingData2_Samp_Scaled, file = "TrainingData2_Samp_Scaled.Rdata")
load("TrainingData2_Samp_Scaled.Rdata")


##### Scaling Normal Data TEST
##### Scaling Normal Data TEST
##### Scaling Normal Data TEST
TestData2_Scaled <- TestData2

TestData2_Scaled[ , c( "rides"
                      ,"AvgDayTempMIN_F"
                      ,"AvgDayTempMAX_F"
                      ,"AvgDayTempOBS_F"
                      ,"AvgDayPRCP_Inch"
                      ,"AvgDaySNOW_Inch"
                      ,"AvgDaySNWD_Inch"
                      )
                 ] <- scale(TestData2_Scaled[ , c( "rides"
                                                  ,"AvgDayTempMIN_F"
                                                  ,"AvgDayTempMAX_F"
                                                  ,"AvgDayTempOBS_F"
                                                  ,"AvgDayPRCP_Inch"
                                                  ,"AvgDaySNOW_Inch"
                                                  ,"AvgDaySNWD_Inch"
                                                  )
                                             ]
                            )

str(TestData2_Scaled)
head(TestData2_Scaled)
summary(TestData2_Scaled)

save(TestData2_Scaled, file = "TestData2_Scaled.Rdata")
load("TestData2_Scaled.Rdata")


##### LOG Data
##### LOG Data
##### LOG Data
str(TrainingData2_LOG)


##### 5% Sample
##### 5% Sample
##### 5% Sample
TrainingData2_LOG_Samp <- TrainingData2_LOG[sample(nrow(TrainingData2_LOG), 
                                                   0.05*(nrow(TrainingData2_LOG)), 
                                                   replace = FALSE
                                                   ), 
                                            ]

str(TrainingData2_LOG_Samp)
head(TrainingData2_LOG_Samp)
summary(TrainingData2_LOG_Samp)

save(TrainingData2_LOG_Samp, file = "TrainingData2_LOG_Samp.Rdata")
load("TrainingData2_LOG_Samp.Rdata")


##### Scaling LOG Data
##### Scaling LOG Data
##### Scaling LOG Data
TrainingData2_LOG_Samp_Scaled <- TrainingData2_LOG_Samp

TrainingData2_LOG_Samp_Scaled[ , c( "ridesLOG"
                                   ,"AvgDayTempMIN_F"
                                   ,"AvgDayTempMAX_F"
                                   ,"AvgDayTempOBS_F"
                                   ,"AvgDayPRCP_Inch"
                                   ,"AvgDaySNOW_Inch"
                                   ,"AvgDaySNWD_Inch"
                                   )
                        ] <- scale(TrainingData2_LOG_Samp_Scaled[ , c( "ridesLOG"
                                                                      ,"AvgDayTempMIN_F"
                                                                      ,"AvgDayTempMAX_F"
                                                                      ,"AvgDayTempOBS_F"
                                                                      ,"AvgDayPRCP_Inch"
                                                                      ,"AvgDaySNOW_Inch"
                                                                      ,"AvgDaySNWD_Inch"
                                                                      )
                                                                 ]
                                   )

str(TrainingData2_LOG_Samp_Scaled)
head(TrainingData2_LOG_Samp_Scaled)
summary(TrainingData2_LOG_Samp_Scaled)

save(TrainingData2_LOG_Samp_Scaled, file = "TrainingData2_LOG_Samp_Scaled.Rdata")
load("TrainingData2_LOG_Samp_Scaled.Rdata")


##### Scaling LOG Data TEST
##### Scaling LOG Data TEST
##### Scaling LOG Data TEST
TestData2_LOG_Scaled <- TestData2_LOG

TestData2_LOG_Scaled[ , c( "ridesLOG"
                          ,"AvgDayTempMIN_F"
                          ,"AvgDayTempMAX_F"
                          ,"AvgDayTempOBS_F"
                          ,"AvgDayPRCP_Inch"
                          ,"AvgDaySNOW_Inch"
                          ,"AvgDaySNWD_Inch"
                          )
                     ] <- scale(TestData2_LOG_Scaled[ , c( "ridesLOG"
                                                          ,"AvgDayTempMIN_F"
                                                          ,"AvgDayTempMAX_F"
                                                          ,"AvgDayTempOBS_F"
                                                          ,"AvgDayPRCP_Inch"
                                                          ,"AvgDaySNOW_Inch"
                                                          ,"AvgDaySNWD_Inch"
                                                          )
                                                     ]
                                )

str(TestData2_LOG_Scaled)
head(TestData2_LOG_Scaled)
summary(TestData2_LOG_Scaled)

save(TestData2_LOG_Scaled, file = "TestData2_LOG_Scaled.Rdata")
load("TestData2_LOG_Scaled.Rdata")


##### Neural Network Algorithm
##### Neural Network Algorithm
##### Neural Network Algorithm

##### Normal Rides
##### Normal Rides
##### Normal Rides
str(TrainingData2_Samp_Scaled)

set.seed(1234)

nn.Norm <- nnet(rides ~ ., TrainingData2_Samp_Scaled, size = 10, decay = 0.01, 
                maxit = 100, linout = TRUE, trace = FALSE, MaxNWts = 2000
                )

save(nn.Norm, file = "nn.Norm.Rdata")
load("nn.Norm.Rdata")

##### Training Predictions
scaled.predictions.Norm <- predict(nn.Norm, TrainingData2_Samp_Scaled)
str(scaled.predictions.Norm)
class(scaled.predictions.Norm)
head(scaled.predictions.Norm)

save(scaled.predictions.Norm, file = "scaled.predictions.Norm.Rdata")
load("scaled.predictions.Norm.Rdata")

nn_NORM.predictions <- unscale(scaled.predictions.Norm, 
                               scale(TrainingData2_Samp[ , c( "rides"
                                                             ,"AvgDayTempMIN_F"
                                                             ,"AvgDayTempMAX_F"
                                                             ,"AvgDayTempOBS_F"
                                                             ,"AvgDayPRCP_Inch"
                                                             ,"AvgDaySNOW_Inch"
                                                             ,"AvgDaySNWD_Inch"
                                                             )
                                                        ]
                                     )
                               )

str(nn_NORM.predictions)
head(nn_NORM.predictions)

save(nn_NORM.predictions, file = "nn_NORM.predictions.Rdata")
load("nn_NORM.predictions.Rdata")


##### TESTING Predictions
scaled.predictions.Norm.Test <- predict(nn.Norm, TestData2_Scaled)
str(scaled.predictions.Norm.Test)
class(scaled.predictions.Norm.Test)
head(scaled.predictions.Norm.Test)

save(scaled.predictions.Norm.Test, file = "scaled.predictions.Norm.Test.Rdata")
load("scaled.predictions.Norm.Test.Rdata")

nn_NORM.predictions.Test <- unscale(scaled.predictions.Norm.Test, 
                                    scale(TestData2[ , c( "rides"
                                                         ,"AvgDayTempMIN_F"
                                                         ,"AvgDayTempMAX_F"
                                                         ,"AvgDayTempOBS_F"
                                                         ,"AvgDayPRCP_Inch"
                                                         ,"AvgDaySNOW_Inch"
                                                         ,"AvgDaySNWD_Inch"
                                                         )
                                                    ]
                                          )
                                    )

str(nn_NORM.predictions.Test)
head(nn_NORM.predictions.Test)

save(nn_NORM.predictions.Test, file = "nn_NORM.predictions.Test.Rdata")
load("nn_NORM.predictions.Test.Rdata")


##### LOG Rides
##### LOG Rides
##### LOG Rides
str(TrainingData2_LOG_Samp_Scaled)

set.seed(1234)

nn.LOG <- nnet(ridesLOG ~ . - rides, TrainingData2_LOG_Samp_Scaled, size = 10, 
                decay = 0.01, maxit = 100, linout = TRUE, trace = FALSE, MaxNWts = 2000
                )

save(nn.LOG, file = "nn.LOG.Rdata")
load("nn.LOG.Rdata")

##### Training Predictions
scaled.predictions.LOG <- predict(nn.LOG, TrainingData2_LOG_Samp_Scaled)
str(scaled.predictions.LOG)
class(scaled.predictions.LOG)
head(scaled.predictions.LOG)

save(scaled.predictions.LOG, file = "scaled.predictions.LOG.Rdata")
load("scaled.predictions.LOG.Rdata")

nn_LOG.predictions <- unscale(scaled.predictions.LOG, 
                              scale(TrainingData2_LOG_Samp[ , c( "rides"
                                                                ,"AvgDayTempMIN_F"
                                                                ,"AvgDayTempMAX_F"
                                                                ,"AvgDayTempOBS_F"
                                                                ,"AvgDayPRCP_Inch"
                                                                ,"AvgDaySNOW_Inch"
                                                                ,"AvgDaySNWD_Inch"
                                                                )
                                                           ]
                                    )
                              )

str(nn_LOG.predictions)
head(nn_LOG.predictions)

save(nn_LOG.predictions, file = "nn_LOG.predictions.Rdata")
load("nn_LOG.predictions.Rdata")


##### TESTING Predictions
scaled.predictions.LOG.Test <- predict(nn.LOG, TestData2_LOG_Scaled)
str(scaled.predictions.LOG.Test)
class(scaled.predictions.LOG.Test)
head(scaled.predictions.LOG.Test)

save(scaled.predictions.LOG.Test, file = "scaled.predictions.LOG.Test.Rdata")
load("scaled.predictions.LOG.Test.Rdata")

nn_LOG.predictions.Test <- unscale(scaled.predictions.LOG.Test, 
                                   scale(TestData2_LOG[ , c( "rides"
                                                            ,"AvgDayTempMIN_F"
                                                            ,"AvgDayTempMAX_F"
                                                            ,"AvgDayTempOBS_F"
                                                            ,"AvgDayPRCP_Inch"
                                                            ,"AvgDaySNOW_Inch"
                                                            ,"AvgDaySNWD_Inch"
                                                            )
                                                       ]
                                         )
                                   )

str(nn_LOG.predictions.Test)
head(nn_LOG.predictions.Test)

save(nn_LOG.predictions.Test, file = "nn_LOG.predictions.Test.Rdata")
load("nn_LOG.predictions.Test.Rdata")



####################
##   Evaluation   ##
##   Statistics   ##
####################

##### Train to Train
##### Train to Train
##### Train to Train
lm.NORM.eval <- regr.eval(TrainingData2[ , "rides"], step_NORM.predictions, 
                          train.y = TrainingData2[ , "rides"]
                          )
lm.NORM.eval


lm.LOG.eval <- regr.eval(TrainingData2_LOG[ , "ridesLOG"], step_LOG.predictions, 
                         train.y = TrainingData2_LOG[ , "ridesLOG"]
                         )
lm.LOG.eval


rt.NORM.eval <- regr.eval(TrainingData2[ , "rides"], rt.Norm.predictions, 
                          train.y = TrainingData2[ , "rides"]
                          )
rt.NORM.eval


rt.LOG.eval <- regr.eval(TrainingData2_LOG[ , "ridesLOG"], rt.LOG.predictions, 
                         train.y = TrainingData2_LOG[ , "ridesLOG"]
                         )
rt.LOG.eval


rf.NORM.eval <- regr.eval(TrainingData2_Top50_Samp[ , "rides"], rf.Norm.predictions, 
                          train.y = TrainingData2_Top50_Samp[ , "rides"]
                          )
rf.NORM.eval


rf.LOG.eval <- regr.eval(TrainingData2_LOG_Top50_Samp[ , "ridesLOG"], rf.LOG.predictions, 
                         train.y = TrainingData2_LOG_Top50_Samp[ , "ridesLOG"]
                         )
rf.LOG.eval


nn.NORM.eval <- regr.eval(TrainingData2_Samp[ , "rides"], nn_NORM.predictions, 
                         train.y = TrainingData2_Samp_Scaled[ , "rides"]
                         )
nn.NORM.eval


nn.LOG.eval <- regr.eval(TrainingData2_LOG_Samp[ , "ridesLOG"], nn_LOG.predictions, 
                          train.y = TrainingData2_LOG_Samp_Scaled[ , "ridesLOG"]
                         )
nn.LOG.eval



EvalStats_TrToTr <- as.data.frame(t(cbind(lm.NORM.eval, lm.LOG.eval, rt.NORM.eval, 
                                          rt.LOG.eval, rf.NORM.eval, rf.LOG.eval, 
                                          nn.NORM.eval, nn.LOG.eval
                                          )
                                    )
                                  )
EvalStats_TrToTr
save(EvalStats_TrToTr, file = "EvalStats_TrToTr.Rdata")
load("EvalStats_TrToTr.Rdata")



##### Train to Test
##### Train to Test
##### Train to Test
lm.NORM.eval.TrToTe <- regr.eval(TestData2[ , "rides"], step_NORM.predictions.Test, 
                                 train.y = TrainingData2[ , "rides"]
                                 )
lm.NORM.eval.TrToTe


lm.LOG.eval.TrToTe <- regr.eval(TestData2_LOG[ , "ridesLOG"], step_LOG.predictions.Test, 
                                train.y = TrainingData2_LOG[ , "ridesLOG"]
                                )
lm.LOG.eval.TrToTe


rt.NORM.eval.TrToTe <- regr.eval(TestData2[ , "rides"], rt.Norm.predictions.Test,
                                 train.y = TrainingData2[ , "rides"]
                                 )
rt.NORM.eval.TrToTe


rt.LOG.eval.TrToTe <- regr.eval(TestData2_LOG[ , "ridesLOG"], rt.LOG.predictions.Test, 
                                train.y = TrainingData2_LOG[ , "ridesLOG"]
                                )
rt.LOG.eval.TrToTe


rf.NORM.eval.TrToTe <- regr.eval(TestData2_Top50Stations[ , "rides"], rf.Norm.predictions.Test, 
                                 train.y = TrainingData2_Top50_Samp[ , "rides"]
                                 )
rf.NORM.eval.TrToTe


rf.LOG.eval.TrToTe <- regr.eval(TestData2_LOG_Top50Stations[ , "ridesLOG"], rf.LOG.predictions.Test, 
                                train.y = TrainingData2_LOG_Top50_Samp[ , "ridesLOG"]
                                )
rf.LOG.eval.TrToTe


nn.NORM.eval.TrToTe <- regr.eval(TestData2[ , "rides"], nn_NORM.predictions.Test, 
                                 train.y = TrainingData2_Samp_Scaled[ , "rides"]
                                 )
nn.NORM.eval.TrToTe


nn.LOG.eval.TrToTe <- regr.eval(TestData2_LOG[ , "ridesLOG"], nn_LOG.predictions.Test, 
                                train.y = TrainingData2_LOG_Samp_Scaled[ , "ridesLOG"]
                                )
nn.LOG.eval.TrToTe


EvalStats_TrToTe <- as.data.frame(t(cbind(lm.NORM.eval.TrToTe, lm.LOG.eval.TrToTe, 
                                          rt.NORM.eval.TrToTe, rt.LOG.eval.TrToTe, 
                                          rf.NORM.eval.TrToTe, rf.LOG.eval.TrToTe, 
                                          nn.NORM.eval.TrToTe, nn.LOG.eval.TrToTe
                                          )
                                    )
                                  )


str_sub(rownames(EvalStats_TrToTe), start = -12, end = -1) <- ""
EvalStats_TrToTe$Algorithm <- rownames(EvalStats_TrToTe)
str(EvalStats_TrToTe)
EvalStats_TrToTe
save(EvalStats_TrToTe, file = "EvalStats_TrToTe.Rdata")
load("EvalStats_TrToTe.Rdata")

EvalStats_TrToTe_Plot <- ggplot(data = EvalStats_TrToTe, aes(y = nmse, 
                                                             x = factor(Algorithm), 
                                                             fill = nmse
                                                             )
                                ) + 
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(limits = rf.Imp.Norm$Algorithm) + 
  xlab("Algorithm") + 
  ylab("Normalized Mean Squared Error (NMSE)") + 
  theme(legend.position="none", axis.text.x = element_text(angle=0)) + 
  theme(legend.position="none") + 
  ggtitle("Algorithm Comparison") + 
  scale_fill_gradient2(limits = c(0, 1), mid = "blue", low = "red")

EvalStats_TrToTe_Plot

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_EvalStats_TrToTe.jpg", scale=1.5)


######################
##   Experimental   ##
##    Comparison    ##
######################

str(TrainingData2_Top50_Samp)

cv.lm <- function(form, train, test, ...) {
  require(DMwR)
  m <- lm(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test)
               )^2
              )
  c(nmse = mse / 
           mean((mean(resp(form, train)) - resp(form, test)
                 )^2
                )
    )
}


cv.rpart <- function(form, train, test, ...) {
  require(DMwR)
  require(rpart)
  m <- rpart(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test)
               )^2
              )
  c(nmse = mse / 
           mean((mean(resp(form,train)) - resp(form, test)
                 )^2
                )
    )
}


cv.rf <- function(form, train, test, ...) {
  require(DMwR)
  require(randomForest)
  m <- randomForest(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test)
               )^2
              )
  c(nmse = mse / 
           mean((mean(resp(form, train)) - resp(form, test)
                 )^2
                )
    )
}


cv.nn <- function(form, train, test, ...) {
  require(DMwR)
  require(nnet)
  m <- nnet(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test)
               )^2
              )
  c(nmse = mse / 
           mean((mean(resp(form, train)) - resp(form, test)
                 )^2
                )
    )
}


ExpComp.Results <- experimentalComparison(
  c(dataset( rides ~ .
            ,data = TrainingData2_Top50_Samp
            #,"rides"
            )
    ),
  c( variants("cv.lm")
    ,variants("cv.rpart", rpart.control(cp = c(0.05, 0.10, 0.15)
                                        )
              )
    ,variants("cv.rf", ntree = c(50, 100, 200)
              )
    ,variants("cv.nn", size = 10, decay = 0.01, maxit = 100, linout = TRUE, trace = FALSE, 
              MaxNWts = 2000
              )
    ), 
  cvSettings(3, 10, 1234)
  )


ExpComp.Results
str(ExpComp.Results)
save(ExpComp.Results, file = "ExpComp.Results.Rdata")
load("ExpComp.Results.Rdata")

summary(ExpComp.Results)
plot(ExpComp.Results)


getVariant("cv.rf.v3", ExpComp.Results)
getVariant("cv.rf.v2", ExpComp.Results)
getVariant("cv.rf.v1", ExpComp.Results)
getVariant("cv.nn.v1", ExpComp.Results)
getVariant("cv.lm.v1", ExpComp.Results)
getVariant("20.v1", ExpComp.Results)


Best <- bestScores(ExpComp.Results)
Best
save(Best, file = "Best.Rdata")
load("Best.Rdata")


Comparison <- compAnalysis(ExpComp.Results, against = "cv.rf.v3")
Comparison
save(Comparison, file = "Comparison.Rdata")
load("Comparison.Rdata")



##########################
##    Predictions vs    ##
##   Actuals (Visuals)  ##
##   "Clark/Lake" Only  ##
##########################

##### Predictions TEST
##### Predictions TEST
##### Predictions TEST
str(TestData2_Top50Stations)

TestData2_Top50Stations_PREDICT <- TestData2_Top50Stations

TestData2_Top50Stations_PREDICT$ridesPREDICT <- predict(rf.Norm, 
                                                        TestData2_Top50Stations_PREDICT
                                                        )

str(TestData2_Top50Stations_PREDICT)

save(TestData2_Top50Stations_PREDICT, file = "TestData2_Top50Stations_PREDICT.Rdata")
load("TestData2_Top50Stations_PREDICT.Rdata")


##### Limit data to station with largest daily average rides (Clark/Lake)
##### Limit data to station with largest daily average rides (Clark/Lake)
##### Limit data to station with largest daily average rides (Clark/Lake)
top_stations_50
str(TestData2_Top50Stations_ClarkLake)

TestData2_Top50Stations_PREDICT_ClarkLake <- filter(TestData2_Top50Stations_PREDICT, 
                                            stationname == "Clark/Lake"
                                            )

TestData2_Top50Stations_PREDICT_ClarkLake$stationname <- factor(TestData2_Top50Stations_PREDICT_ClarkLake$stationname)
str(TestData2_Top50Stations_PREDICT_ClarkLake)

save(TestData2_Top50Stations_PREDICT_ClarkLake, file = "TestData2_Top50Stations_PREDICT_ClarkLake.Rdata")
load("TestData2_Top50Stations_PREDICT_ClarkLake.Rdata")


##### Prediction vs Actual Line Graph (All Dates)
##### Prediction vs Actual Line Graph (All Dates)
##### Prediction vs Actual Line Graph (All Dates)
str(TestData2_Top50Stations_PREDICT_ClarkLake)

PredictVsActual <- melt(TestData2_Top50Stations_PREDICT_ClarkLake[ , c("date", "rides", "ridesPREDICT")], 
                        id="date"
                        )  # convert to long format
str(PredictVsActual)
head(PredictVsActual)

save(PredictVsActual, file = "PredictVsActual.Rdata")
load("PredictVsActual.Rdata")

AllDates <- ggplot(data=PredictVsActual, aes(x=date, y=value, color=variable)
                   ) +
  geom_line() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="bottom", legend.title=element_blank(), 
        axis.text.x = element_text(angle=20)
  ) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("")

AllDates

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_PredictVsActual_AllDates.jpg", 
       scale=1.5
       )



##### Prediction vs Actual Line Graph (Holidays)
##### Prediction vs Actual Line Graph (Holidays)
##### Prediction vs Actual Line Graph (Holidays)
Nov2013 <- ggplot(data=filter(PredictVsActual, date > "2013-11-19", date < "2013-12-04"),
                  aes(x=date, y=value, color=variable)
                  ) +
  geom_line() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="bottom", legend.title=element_blank(), 
        axis.text.x = element_text(angle=20)
        ) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("")

Nov2013


Dec2013 <- ggplot(data=filter(PredictVsActual, date > "2013-12-18", date < "2014-01-03"),
                  aes(x=date, y=value, color=variable)
                  ) +
  geom_line() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="bottom", legend.title=element_blank(), 
        axis.text.x = element_text(angle=20)
        ) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("")

Dec2013


Feb2014 <- ggplot(data=filter(PredictVsActual, date > "2014-02-12", date < "2014-02-26"),
                  aes(x=date, y=value, color=variable)
                  ) +
  geom_line() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="bottom", legend.title=element_blank(), 
        axis.text.x = element_text(angle=20)
        ) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("")

Feb2014


July2014 <- ggplot(data=filter(PredictVsActual, date > "2014-06-30", date < "2014-07-15"),
                   aes(x=date, y=value, color=variable)
                   ) +
  geom_line() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="bottom", legend.title=element_blank(), 
        axis.text.x = element_text(angle=25)
        ) + 
  #theme(legend.position="right", axis.text.x = element_blank()) + 
  ggtitle("")

July2014

PredictVsActual_Graph <- multiplot(Nov2013, Dec2013, Feb2014, July2014, cols=2)
PredictVsActual_Graph

ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_PredictVsActual_Holidays.jpg", 
       scale=1.5
       )



#####################
##   Time Series   ##
##     Analyses    ##
#####################

str(TrainingData2_Top50Stations)

##### Subset data to only the Clark/Lake Station
##### Subset data to only the Clark/Lake Station
##### Subset data to only the Clark/Lake Station
TrainingData2_Top50Stations_ClarkLake <- filter(TrainingData2_Top50Stations, 
                                                stationname == "Clark/Lake"
                                                )
TrainingData2_Top50Stations_ClarkLake$stationname <- factor(TrainingData2_Top50Stations_ClarkLake$stationname)
str(TrainingData2_Top50Stations_ClarkLake)

save(TrainingData2_Top50Stations_ClarkLake, file = "TrainingData2_Top50Stations_ClarkLake.Rdata")
load("TrainingData2_Top50Stations_ClarkLake.Rdata")

##### Create a Time-Series Dataset (Train)
##### Create a Time-Series Dataset (Train)
##### Create a Time-Series Dataset (Train)
str(TrainingData2_Top50Stations_ClarkLake)
summary(TrainingData2_Top50Stations_ClarkLake)
yday(min(TrainingData2_Top50Stations_ClarkLake$date))
yday(max(TrainingData2_Top50Stations_ClarkLake$date))

TSeriesTrain <- msts(TrainingData2_Top50Stations_ClarkLake[ , "rides"], 
                     start = c(2001, 1), end = c(2010, 365), 
                     seasonal.periods = c(7, 365.25)
                     )
str(TSeriesTrain)
head(TSeries)

save(TSeriesTrain, file = "TSeriesTrain.Rdata")
load("TSeriesTrain.Rdata")


##### Testing Time-Series Forms
##### Testing Time-Series Forms
##### Testing Time-Series Forms
TSeriesTrain_Diff <- diff(TSeriesTrain)
TSeriesTrain_Diff7 <- diff(TSeriesTrain, lag = 7)

TSeriesTrain_LOG <- log(TSeriesTrain)
TSeriesTrain_LOG_Diff <- diff(TSeriesTrain_LOG)
TSeriesTrain_LOG_Diff7 <- diff(TSeriesTrain_LOG, lag = 7)


plot(TSeriesTrain)
plot(TSeriesTrain_Diff)
plot(TSeriesTrain_Diff7)
plot(diff(TSeriesTrain_Diff7))

plot(TSeriesTrain_LOG)
plot(TSeriesTrain_LOG_Diff)
plot(TSeriesTrain_LOG_Diff7)
plot(diff(TSeriesTrain_LOG_Diff7))


plot(stl(TSeriesTrain, s.window = "periodic"
         )
     )
plot(stl(TSeriesTrain, s.window = 7
         )
     )
#?stl
monthplot(stl(TSeriesTrain, s.window = "periodic"
              )
          )

# library("forecast")
# Ar <- arima(TSeriesTrain, order = c(1, 0, 0))
# accuracy(Ar)



Acf(TSeriesTrain)
Acf(TSeriesTrain_Diff)
Acf(diff(TSeriesTrain_Diff7))

Acf(TSeriesTrain_LOG)
Acf(TSeriesTrain_LOG_Diff)
Acf(diff(TSeriesTrain_LOG_Diff7))

tsdisplay(TSeriesTrain_Diff7)
tsdisplay(diff(TSeriesTrain_Diff7))
tsdisplay(TSeriesTrain_LOG_Diff7)


adf.test(TSeriesTrain_LOG_Diff7, alternative = "stationary")
kpss.test(TSeriesTrain_LOG_Diff7)

Pacf(TSeriesTrain)
Pacf(TSeriesTrain_Diff)
Pacf(TSeriesTrain_Diff7)

Pacf(TSeriesTrain_LOG)
Pacf(TSeriesTrain_LOG_Diff)
Pacf(TSeriesTrain_LOG_Diff7)


##### Estimating the number of differences in the time series
##### Estimating the number of differences in the time series
##### Estimating the number of differences in the time series
ns <- nsdiffs(TSeriesTrain)
str(ns)
head(ns)

if(ns > 0) {
  xstar <- diff(TSeriesTrain,lag=frequency(TSeriesTrain),differences=ns)
} else {
  xstar <- TSeriesTrain
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}
str(xstar)
head(xstar)
head(TSeriesTrain)


##### Manually trying to estimate the number of differences in the time series
##### Manually trying to estimate the number of differences in the time series
##### Manually trying to estimate the number of differences in the time series
fit <- auto.arima(TSeriesTrain, max.p = 366, max.q = 366, stepwise = TRUE, trace = TRUE)
str(fit)
fit
summary(fit)
plot(forecast(fit, h = 365))

save(fit, file = "fit.Rdata")
load("fit.Rdata")

Acf(residuals(fit))
Box.test(residuals(fit), type="Ljung")

fit2 <- auto.arima(TSeriesTrain, max.p = 366, max.q = 366, stepwise = FALSE, trace = TRUE, 
                   approximation = FALSE
                   )
str(fit2)
fit2
summary(fit2)
plot(forecast(fit2, h = 365))

save(fit2, file = "fit2.Rdata")
load("fit2.Rdata")

Acf(residuals(fit2))
Box.test(residuals(fit2), type="Ljung")


fit3 <- auto.arima(TSeriesTrain, max.p = 366, max.q = 366, max.d = 366, max.P = 366, 
                   max.Q = 366, max.D = 366, stepwise = TRUE, trace = TRUE
                   )
str(fit3)
fit3
summary(fit3)
plot(forecast(fit3, h = 365))

save(fit3, file = "fit3.Rdata")
load("fit3.Rdata")

Acf(residuals(fit3))
Box.test(residuals(fit3), type="Ljung")

fit4 <- auto.arima(TSeriesTrain, max.p = 366, max.q = 366, max.d = 366, max.P = 366, 
                   max.Q = 366, max.D = 366, stepwise = FALSE, trace = TRUE, 
                   approximation = FALSE
                   )
str(fit4)
fit4
summary(fit4)
plot(forecast(fit4, h = 365))

save(fit4, file = "fit4.Rdata")
load("fit4.Rdata")

Acf(residuals(fit4))
Box.test(residuals(fit4), type="Ljung")



fit_LOG <- auto.arima(TSeriesTrain_LOG, max.p = 366, max.q = 366, stepwise = TRUE, 
                      trace = TRUE
                      )
str(fit_LOG)
fit_LOG
summary(fit_LOG)
plot(forecast(fit_LOG, h = 365))

save(fit_LOG, file = "fit_LOG.Rdata")
load("fit_LOG.Rdata")

Acf(residuals(fit_LOG))
Box.test(residuals(fit_LOG), type="Ljung")

fit_LOG2 <- auto.arima(TSeriesTrain_LOG, max.p = 366, max.q = 366, stepwise = FALSE, 
                       trace = TRUE, approximation = FALSE
                       )
str(fit_LOG2)
fit_LOG2
summary(fit_LOG2)
plot(forecast(fit_LOG2, h = 365))

save(fit_LOG2, file = "fit_LOG2.Rdata")
load("fit_LOG2.Rdata")

Acf(residuals(fit_LOG2))
Box.test(residuals(fit_LOG2), type="Ljung")


fit_LOG3 <- auto.arima(TSeriesTrain_LOG, max.p = 366, max.q = 366, max.d = 366, max.P = 366, 
                   max.Q = 366, max.D = 366, stepwise = TRUE, trace = TRUE
                   )
str(fit_LOG3)
fit_LOG3
summary(fit_LOG3)
plot(forecast(fit_LOG3, h = 365))

save(fit_LOG3, file = "fit_LOG3.Rdata")
load("fit_LOG3.Rdata")

Acf(residuals(fit_LOG3))
Box.test(residuals(fit_LOG3), type="Ljung")

fit_LOG4 <- auto.arima(TSeriesTrain_LOG, max.p = 366, max.q = 366, max.d = 366, max.P = 366, 
                   max.Q = 366, max.D = 366, stepwise = FALSE, trace = TRUE, 
                   approximation = FALSE
                   )
str(fit_LOG4)
fit_LOG4
summary(fit_LOG4)
plot(forecast(fit_LOG4, h = 365))

save(fit_LOG4, file = "fit_LOG4.Rdata")
load("fit_LOG4.Rdata")

Acf(residuals(fit_LOG4))
Box.test(residuals(fit_LOG4), type="Ljung")


##### Automating the estimation of the model's form
##### Automating the estimation of the model's form
##### Automating the estimation of the model's form
fitTBATS <- tbats(TSeriesTrain)
fitTBATS
summary(fitTBATS)

save(fitTBATS, file = "fitTBATS.Rdata")
load("fitTBATS.Rdata")

components <- tbats.components(fitTBATS)
plot(components)

save(components, file = "components.Rdata")
load("components.Rdata")

fc <- forecast(fitTBATS)
summary(fc)
plot(fc)


##### Create a Time-Series Dataset (Test)
##### Create a Time-Series Dataset (Test)
##### Create a Time-Series Dataset (Test)

str(TestData2_Top50Stations_PREDICT_ClarkLake)
yday(min(TestData2_Top50Stations_PREDICT_ClarkLake$date))
yday(max(TestData2_Top50Stations_PREDICT_ClarkLake$date))

TSeriesTest <- msts(TestData2_Top50Stations_PREDICT_ClarkLake[ , "rides"], 
                start = c(2011, 1), end = c(2014, 273), seasonal.periods = c(7, 365.25)
                )
str(TSeriesTest)
head(TSeriesTest)

save(TSeriesTest, file = "TSeriesTest.Rdata")
load("TSeriesTest.Rdata")




#########################
##    Linear Model     ##
##  Limited Variables  ##
#########################

##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)

fit_NORM_lmtd <- lm(rides ~ stationname + dayname + Holiday + date + date_year + 
                            date_month + AvgDayTempOBS_F, data = TrainingData2
                    )
step_NORM_lmtd <- stepAIC(fit_NORM_lmtd, direction = "both")

save(step_NORM_lmtd, file = "step_NORM_lmtd.Rdata")
load("step_NORM_lmtd.Rdata")

summary(step_NORM_lmtd)
step_NORM_lmtd$anova


##### Predictions TRAIN
step_NORM_lmtd.predictions <- predict(step_NORM_lmtd, TrainingData2)
save(step_NORM_lmtd.predictions, file = "step_NORM_lmtd.predictions.Rdata")
load("step_NORM_lmtd.predictions.Rdata")


##### Predictions TEST
step_NORM_lmtd.predictions.Test <- predict(step_NORM_lmtd, TestData2)
save(step_NORM_lmtd.predictions.Test, file = "step_NORM_lmtd.predictions.Test.Rdata")
load("step_NORM_lmtd.predictions.Test.Rdata")


##### Testing Residuals Against Independent Varialbes
plot(TrainingData2$stationname, residuals(fit_NORM_lmtd), xlab="StationName")
plot(TrainingData2$dayname, residuals(fit_NORM_lmtd), xlab="DayName")
plot(TrainingData2$Holiday, residuals(fit_NORM_lmtd), xlab="Holiday")
plot(TrainingData2$date, residuals(fit_NORM_lmtd), xlab="Date")
plot(TrainingData2$date_year, residuals(fit_NORM_lmtd), xlab="DateYear")
plot(TrainingData2$date_month, residuals(fit_NORM_lmtd), xlab="DateMonth")
plot(TrainingData2$AvgDayTempOBS_F, residuals(fit_NORM_lmtd), xlab="AvgDayTempOBS_F")


##### Testing Residuals Against Fitted Values
plot(fitted(fit_NORM_lmtd), residuals(fit_NORM_lmtd), xlab="Predicted scores", 
     ylab="Residuals"
     )


##### Testing Normality of Residuals
qqnorm(residuals(fit_NORM_lmtd)
       )
qqline(residuals(fit_NORM_lmtd)
       )



#########################
##    Linear Model     ##
##  Limited Variables  ##
##      No Date        ##
#########################

##### Normal data
##### Normal data
##### Normal data
str(TrainingData2)

fit_NORM_lmtd_NoDate <- lm(rides ~ stationname + dayname + Holiday + date_year + 
                                   date_month + AvgDayTempOBS_F, data = TrainingData2
                           )
step_NORM_lmtd_NoDate <- stepAIC(fit_NORM_lmtd_NoDate, direction = "both")

save(step_NORM_lmtd_NoDate, file = "step_NORM_lmtd_NoDate.Rdata")
load("step_NORM_lmtd_NoDate.Rdata")

summary(step_NORM_lmtd_NoDate)
step_NORM_lmtd_NoDate$anova


##### Predictions TRAIN
step_NORM_lmtd_NoDate.predictions <- predict(step_NORM_lmtd_NoDate, TrainingData2)
save(step_NORM_lmtd_NoDate.predictions, file = "step_NORM_lmtd_NoDate.predictions.Rdata")
load("step_NORM_lmtd_NoDate.predictions.Rdata")


##### Predictions TEST
step_NORM_lmtd_NoDate.predictions.Test <- predict(step_NORM_lmtd_NoDate, TestData2)
save(step_NORM_lmtd_NoDate.predictions.Test, 
     file = "step_NORM_lmtd_NoDate.predictions.Test.Rdata"
     )
load("step_NORM_lmtd_NoDate.predictions.Test.Rdata")


##### Testing Residuals Against Independent Varialbes
plot(TrainingData2$stationname, residuals(fit_NORM_lmtd_NoDate), xlab="StationName")
plot(TrainingData2$dayname, residuals(fit_NORM_lmtd_NoDate), xlab="DayName")
plot(TrainingData2$Holiday, residuals(fit_NORM_lmtd_NoDate), xlab="Holiday")
plot(TrainingData2$date_year, residuals(fit_NORM_lmtd_NoDate), xlab="DateYear")
plot(TrainingData2$date_month, residuals(fit_NORM_lmtd_NoDate), xlab="DateMonth")
plot(TrainingData2$AvgDayTempOBS_F, residuals(fit_NORM_lmtd_NoDate), 
     xlab="AvgDayTempOBS_F"
     )


##### Testing Residuals Against Fitted Values
plot(fitted(fit_NORM_lmtd_NoDate), residuals(fit_NORM_lmtd_NoDate), 
     xlab="Predicted scores", ylab="Residuals"
     )


##### Testing Normality of Residuals
qqnorm(residuals(fit_NORM_lmtd_NoDate)
       )
qqline(residuals(fit_NORM_lmtd_NoDate)
       )



#########################
##     Evaluation      ##
##     Statistics      ##
##  Linear Regression  ##
##        ONLY         ##
#########################

##### Train to Train
##### Train to Train
##### Train to Train
lm.eval <- regr.eval(TrainingData2[ , "rides"], step_NORM.predictions,
                     train.y = TrainingData2[ , "rides"]
                     )
lm.eval

lm.eval.lmtd <- regr.eval(TrainingData2[ , "rides"], step_NORM_lmtd.predictions, 
                          train.y = TrainingData2[ , "rides"]
                          )
lm.eval.lmtd

lm.eval.lmtd_NoDate <- regr.eval(TrainingData2[ , "rides"], 
                                 step_NORM_lmtd_NoDate.predictions,
                                 train.y = TrainingData2[ , "rides"]
                                 )
lm.eval.lmtd_NoDate



EvalStats_TrToTr2 <- as.data.frame(t(cbind(lm.eval, lm.eval.lmtd, lm.eval.lmtd_NoDate
                                           )
                                     )
                                   )
EvalStats_TrToTr2
save(EvalStats_TrToTr2, file = "EvalStats_TrToTr2.Rdata")
load("EvalStats_TrToTr2.Rdata")


##### Train to Test
##### Train to Test
##### Train to Test
lm.eval.TrToTe <- regr.eval(TestData2[ , "rides"], step_NORM.predictions.Test,
                            train.y = TrainingData2[ , "rides"]
                            )
lm.eval.TrToTe

lm.eval.lmtd.TrToTe <- regr.eval(TestData2[ , "rides"], step_NORM_lmtd.predictions.Test,
                                 train.y = TrainingData2[ , "rides"]
                                 )
lm.eval.lmtd.TrToTe

lm.eval.lmtd_NoDate.TrToTe <- regr.eval(TestData2[ , "rides"],
                                        step_NORM_lmtd_NoDate.predictions.Test,
                                        train.y = TrainingData2[ , "rides"]
                                        )
lm.eval.lmtd_NoDate.TrToTe


EvalStats_TrToTe2 <- as.data.frame(t(cbind(lm.eval.TrToTe, lm.eval.lmtd.TrToTe,
                                           lm.eval.lmtd_NoDate.TrToTe
                                           )
                                     )
                                   )
EvalStats_TrToTe2
save(EvalStats_TrToTe2, file = "EvalStats_TrToTe2.Rdata")
load("EvalStats_TrToTe2.Rdata")


# str_sub(rownames(EvalStats_TrToTe2), start = 1, end = 3) <- ""
# str_sub(rownames(EvalStats_TrToTe2), start = -1, end = -7) <- ""
# EvalStats_TrToTe2$Algorithm <- rownames(EvalStats_TrToTe2)
# str(EvalStats_TrToTe2)
# EvalStats_TrToTe2
# save(EvalStats_TrToTe2, file = "EvalStats_TrToTe2.Rdata")
# load("EvalStats_TrToTe2.Rdata")
# 
# EvalStats_TrToTe2_Plot <- ggplot(data = EvalStats_TrToTe2, aes(y = nmse,
#                                                                x = factor(Algorithm),
#                                                                fill = nmse
#                                                                )
#                                  ) + 
#   geom_bar(stat = "identity") + 
#   coord_cartesian(ylim = c(0, 1)) + 
#   scale_x_discrete(limits = rf.Imp.Norm$Algorithm) + 
#   xlab("Algorithm") + 
#   ylab("Normalized Mean Squared Error (NMSE)") + 
#   theme(legend.position="none", axis.text.x = element_text(angle=0)) + 
#   theme(legend.position="none") + 
#   ggtitle("Algorithm Comparison") + 
#   scale_fill_gradient2(limits = c(0, 1), mid = "blue", low = "red")
# 
# EvalStats_TrToTe2_Plot
# 
# ggsave("/Users/mdturse/Google Drive/TimeSeries/Plot_EvalStats_TrToTe2.jpg", scale=1.5)










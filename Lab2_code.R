###########################################################################
### Lab 2-Precipitation Analysis
###
### Each line of comments includes some directions which you must 
### turn into some code below each comment. Sometimes started code
### will be provided and you have to finish it, or insert arguments.
### Other times only directions will be provided.
### When finished, paste your code below and figures into a document 
### and submit as a your lab report as .doc or .pdf on Canvas.
############################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)

# read in daily precip data from Pittsburgh from file.
# check your working directory and place the data there
getwd()

# to see documentation about what a function does and how to use it
# type a questions mark and the name of the function(), and look at the help 
# window in lower right hand corner
?read_csv()

# and can load using
p <- read_csv("C:/Users/zucco/Desktop/Academics/Pitt/GEOL_SurfaceWaterHydrology/Lab/Lab2/daily.csv")

# convert Date to date, R thought it was a "character"
p$DATE <- as.Date(p$DATE, format="%m/%d/%Y")

# Another method to do this would be using the mutate function and "pipes"
# A pipe is %>% and is a way of chaining functions together.
# For example, 
# new_data <- function3(function2(function1(data)))
# is the SAME as
# new_data <- data %>%
#   function1() %>%
#   function2() %>%
#   function3()

p <- p %>%
  mutate(DATE =as.Date(p$DATE, format="%m/%d/%Y"))

# First make a plot of all the data using base R
plot(p$DATE, p$PRCP, type="l")

# FINISH THIS CODE: now in ggplot
ggplot(p)+
  geom_line(aes(x=DATE, y=PRCP ))

# You can make ggplots INTERACTIVE! Run this and explore the interactive graph
ggplotly(ggplot(p)+
           geom_line(aes(x=DATE, y= PRCP)))

# Lets add a month, year, and day of year (DOY) columns using functions from the 
# lubridate package to make future calculations easier. We sometimes have to tell R
# which package a function comes from by adding package::function when there are 
# multiple R functions with the same name.

# FINISH THIS CODE: You will have to find the function that estimates DOY or year day
# perhaps on the internet
p <- p %>%
  mutate(year = lubridate::year(DATE),
         month = lubridate::month(DATE),
         doy =  lubridate::yday(DATE)                   )
 
# Calculate the annual maximum, mean precipitation, and annual total precipitation
# the group_by function makes this easy by making "groups" out of a variable/column
# and then applying the same function to all using summarise function

# FINISH THIS CODE: What column do you have to group_by to get ANNUAL summary stats
p_annual <- p %>%
  group_by(year) %>%
  summarise(annual_mean = mean(PRCP), 
            annual_total = sum(PRCP),
            annual_max =  max(PRCP))

# inspect the p_annual dataframe by clicking on the object in the environment
# does it look like what you expected? Notice the NAs, data is messy and looks like
# not every where had complete dataset. We could remove that by adding na.rm= TRUE
# to mean(PRCP, na.rm=TRUE), but we do not want incomplete datasets here

# first lets drop rows with pesky NA data
p_annual <- p_annual %>%
  filter(!is.na(annual_max))
  
####################################################################################  

# QUESTION : Why would incomplete daily rainfall data be a problem for calculating
# statistics such total and max precip? And why is the mean not a good statistic annual 
# rainfall?

# ANSWER: Having N/A values in your daily rainfall data would result in an underrepresentation of total and max precipitation. 
# Mean is not a good statistic for annual rainfall because extreme events, such as one day with incredibly high rainfall, can skew the mean. 
# This is why meteorologists tend to use median values to signify “typical” conditions.

####################################################################################

# ANALYSIS 1: Calculate the return period and exceedence probability of the annual
# maximum rainfall from this dataset. Sort the data from highest to lowest
# value using the arrange() function.
# Then add columns of rank, return period (Tr), and probability using the mutate()function
# and equations from class. 
# FINISH THIS CODE in style of tidyverse OR re-code the same 
# analysis using whatever way of coding you prefer.P

p_annual <- p_annual %>%
  arrange(desc(annual_max)) %>%
  mutate(rank =1:nrow(p_annual)) %>%
  mutate(Tr = 18/rank) %>%
  mutate(prob = 1/Tr)

# MAKE FIGURE 1: plot precipitation magnitude vs return period and add this plot
# to your lab report. you can save figures as image files by clicking EXPORT
# in the plot window or using ggsave (if its a ggplot) which will write to file
# the last figure that was plotted
# Example
# ggsave("D:/documents/myfolder/image.png", width = 4, height=4, units="in", dpi=300)

ggplotly(ggplot(p_annual)+
           geom_line(aes(x=annual_max, y=Tr)))

###################################################################################

#ANALYSIS 2: Calculating monthly climate normals. Using a similar approach above
# group by year and month...group_by(year, month)... to calculate the sum or total
# for each month in each year. THEN group_by month to calculate the mean monthly
# climate normal across all years.
# FINISH THIS CODE in style of tidyverse OR do the same 
# analysis using whatever way of coding you prefer.

p_month <- p %>%
  # lets first filter to only years with full dataset from the p_annual summary
  filter(year %in% p_annual$year) %>%
  group_by(year, month) %>%
  summarise(YM_Sum = sum(PRCP)) %>%
  ungroup(year) %>%
  group_by(month) %>%
  summarize(month_avg_PRCP = mean(YM_Sum))


# MAKE FIGURE 2: PLOT the mean monthly precipitation vs. month. Submit it in your report.

ggplotly(ggplot(p_month)+
           geom_line(aes(x=month, y=month_avg_PRCP)))

####################################################################################

# QUESTION: What is the rainiest month on average. Is it what you expected, why or why not?

# ANSWER: The rainiest month on average is June. 
# This is what I expected for the Pittsburgh area as we get a lot of summertime rain in June and July.

####################################################################################

# ANALYSIS 3: Trends in precipitation. Fit a linear regression to the annual
# precipitation calculated in p_annual. 

# FINISH THIS code to fit a linear regression using the lm() function. 
# look at the arguments by typing ?lm() in the console
model <- lm(annual_total ~ year, data = p_annual)

# extract the summary of the fitted model
summary(model)

# MAKE FIGURE 3: plot the annual total precip vs year.

ggplotly(ggplot(p_annual)+
           geom_point(aes(x=year, y=annual_total)))

#####################################################################################

# QUESTION: Is there a significant trend? If so, which way was it trending?
# Hypothesize about the trend or lack of trend and why or why not 
# this data is appropriate for this statistical test.

# ANSWER: There is no significant positive or negative trend observed in the data. 
# This is not an appropriate statistical test for this data because the variables, year and annual total precipitation 
# do not have a linear positive or negative relationship. Over longer time frames, 
# it is possible that increases and decreases in precipitation could be witnessed from factors such as climate change. 
# However, the approximately 20 years of data that are analyzed in this problem have an R2 value of 0.2, 
# indicating that there is little correlation between these variables.

#####################################################################################

# ANALYSIS 4: Lets have fun making more plots to compare inter-annual variation in precip

# Run this code to add another dimension in color to a plot
ggplot(p) +
  geom_line(aes(x=doy, y=PRCP, color=year)) +
  scale_color_viridis_c()

# hmmm...its hard to see differences by year. Lets try plotting it in different plot windows
ggplot(p) +
  geom_line(aes(x=doy, y=PRCP)) +
  facet_wrap(~year)

####################################################################################

# QUESTION: By looking at this plot, what 2 years have the biggest storms?
# And provide 2 potential causes of big rains occurring at the time of year they occurred?

# ANSWER: The biggest storms occurred in 2000 and 2004. 
# Potential causes of large rain events during these times of the year (June – November) include the Atlantic hurricane season
# (Hurricane Ivan in 2004) and by movement of the jet stream and moisture from the Great Lakes.

####################################################################################

# Lets try plotting cumulative precip so we can better see inter-annual differences
p <- p %>%
  group_by(year) %>%
  mutate(cumulative = cumsum(PRCP)) %>%
  ungroup()

ggplot(p) +
  geom_line(aes(x=doy, y=cumulative, group=year, color=year)) +
  scale_color_viridis_c() +
  theme_bw()

####################################################################################

# QUESTION: What season or time of year has the greatest inter-annual variability in 
# precipitation in Pittsburgh?

# ANSWER: The end of the year (fall and winter) has the greatest inter-annual variability in precipitation.

####################################################################################



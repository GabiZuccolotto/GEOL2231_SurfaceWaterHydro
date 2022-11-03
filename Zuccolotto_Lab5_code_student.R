#####################################################################
### Lab 5: Streamflow Analysis
### We will do flow-duration analysis, low flow analysis,
### and gain practice interpreting streamflow regimes
#####################################################################

### Install any of the below packages that are not installed using
### install.packages("package_name")

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dataRetrieval")
install.packages("zoo")
install.packages("viridis")

# load libraries into R 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dataRetrieval)
library(zoo)
library(viridis)

#####################################################################
### LOAD DATA
#####################################################################

### Load in daily river discharge data from the internet using the dataRetrieval package with the function
### readNWISdv. This function takes arguments of a USGS sitenumber, the parameter Code
### (here we use "00060" which is the code for discharge) and it loads all available 
### daily discharge values over the period of record. The column with discharge (ft3/s) data
### will be named  X_00060_00003. These are the same sites from Lab#1, plus we added the rio grande.
### RUN THIS  and finish the other two.
# Load french creek near Wattsburgh PA
fr <- readNWISdv(siteNumbers = "03021350", parameterCd = "00060")

# Load allegheny river near Natrona PA
al <- readNWISdv(siteNumbers = "03049500", parameterCd = "00060") 

# Load data for the Rio Grande River
rio <- readNWISdv("08330000", parameterCd = "00060") 

### Change the name of column X_00060_00003 to "Q", and add a column called "name"
### of the river name to identify rather than using USGS site number. Do this for each site.
### (HINT: you could USE rename() function and  mutate() to add a column)
### FINISH THIS CODE
fr <- fr %>%
  rename(Q=X_00060_00003)%>%
  mutate("Name"= "French Creek")

al <- al %>%
  rename(Q=X_00060_00003)%>%
  mutate("Name"="Allegheny River")

rio <- rio %>%
  rename(Q=X_00060_00003)%>%
  mutate("Name"="Rio Grande")


### Now we can merge the three sites and do all analyses on them at once.
### ADD a column for day of year (doy) AND a column for year.
### lets also make sure there are no NAs in the Q data. The "!" means "is not" so
### !is.na() means is not NA. We are filtering to only rows that have no NA in the Q column.
### FINISH code:
data <- bind_rows(fr, al, rio) %>%
  mutate(doy = lubridate::yday(Date))%>%
  mutate(year = lubridate::year(Date)) %>%
  filter(!is.na(Q))

####################################################################################
### ANALYSIS 1: Flow-Duration Analysis
####################################################################################

### Plot 1a: Plot the entire time series of each river using ggplot and facet_wrap
ggplot(data = data, aes(x=Date, y=Q))+
  labs(title = "Streamflow Comparisons",
       y="Q (cfs)", x="Date") +
  geom_line() +
  facet_wrap(~ Name)

### QUESTION 1a: Make a few brief observations about the time series. Notice any patterns or trends?
### in high flow, low flow, or anything else. If so, make a hypothesis why?

  # A large river in the southwest United States has a similar discharge profile to a small creek in 
  # the northeast.. even though Rio Grande is a more comparable size to the Allegheny River... This 
  # speaks to differences in climate/precipitation trends between the two regions (PRCP > northeast US)

### Calculate the return period and exceedence probability for all daily discharge data
### for all 3 sites. Use whatever approach you want, could use code from Lab 2
### HINT: Use the n() function to find the length of the data for each group
### FINISH THIS CODE
data_fdc <- data %>%
  group_by(Name) %>%
  arrange(desc(Q)) %>%
  mutate(rank=1:n())%>%
  mutate(Tr=(n()+1)/rank) %>%
  mutate(prob=1/Tr)%>%
  ungroup()

### PLOT 1b: Make a Flow-Duration Curve of Q vs. exceedence probability for all 3 sites. 
### You could use ggplot and facet_wrap to do both at the same time.
### Add nice x and y axis titles and put the y axis (e.g. Discharge) in log10 which is typical. 
### but also look at the graph without a log10 transformation

ggplot(data = data_fdc, aes(x=prob, y=log10(Q)))+
  labs(title = "Flow Duration Curves",
       y="Discharge(cfs)", x="Exceedence Probability") +
  geom_point() +
  facet_wrap(~ Name)

#### QUESTION 1b: Interpret any differences in the FDC curves between three different rivers.
### e.g. What could you infer just by looking at the shape of this graph?


########################################################################
### ANALYSIS 2: Low Flow Analysis
#######################################################################
###
### The 1Q10 and 7Q10 are both hydrologically based design flows. 
### The 1Q10 is the lowest 1-day average flow that occurs (on average) once every 10 years
### The 7Q10 is the lowest 7-day average flow that occurs (on average) once every 10 years."
### EPA https://www.epa.gov/ceam/definition-and-characteristics-low-flows#1Q10
### The 10 in 7Q10 means there is a 10 percent chance that the associated 7-day average flow 
### or below will occur in any given year.
### To calculate the 7Q10 we need to calculate the 7 day (rolling) mean Q, and then
### find the minimum 7 day rolling mean Q that occurs within each year.
### So this is essentialy the same as doing flow frequency analysis on peak flows 
### to estimate floods, but instead we are looking and flow droughts. 

### Fill in the missing arguments to calculate the 7 day rolling  mean Q for each site
### look up the function by typing ?rollmean() into the console or searching it on the web
### FINISH THIS CODE
data_low <- data %>% 
  group_by(Name) %>%
  mutate(xdaymean = rollmean(x =  Q  ,
                            k = 7    , 
                            fill = NA, 
                            na.rm = F, 
                            align = "right")) %>%
  ungroup()

### Lets make a quick plot for 1 year to see what a rolling mean looks like
### RUN THIS CODE
ggplot(data_low %>% 
           filter(Date >= mdy("01-01-2018") & Date <= mdy("12-31'2018"))) +
  geom_line(aes(x=Date, y= Q), color="black") +
  geom_line(aes(x=Date, y= xdaymean), color="red") +
  facet_wrap(~Name, scales="free")
  
### CALCULATE the yearly minimum flow for each site and year. FILL IN the missing 
### arguments
### FINISH THIS CODE
yearlyMin <- data_low %>%
  mutate(year=year(Date)) %>%
  group_by(Name, year) %>%
  summarize(minQ = min(xdaymean, na.rm=T), 
            lenDat = length(Q),
            lenNAs = sum(is.na(xdaymean))) %>%
  filter(lenDat > 328 & lenNAs / lenDat < 0.1) %>%
  ungroup()

### CALCULATE the rank, return interval, and exceedence probability for the 7Q10
### for each site by grouping by name or site_no and using same calculations 
### as before.
### FINISH AND ADD NEW CODE
yearlyMin <- yearlyMin %>% 
  group_by(Name) %>%
  mutate(rank=rank(minQ, ties.method="first")) %>%
  mutate(Tr=(length(rank)+1)/rank)%>%
  mutate(prob=1/Tr) %>%
  ungroup()

### PLOT 2: Plot the minimum Q vs. return interval for all three sites
ggplot(data = yearlyMin, aes(x=Tr, y=minQ))+
  labs(y="Discharge(cfs)", x="Return Interval") +
  geom_point() +
  facet_wrap(~ Name, scales="free")

### QUESTION 2a: Approximately what is the 7Q10 from looking at the graph of each river?


### QUESTION 2b: A company wants to build a brewery near the Allegheny River 
### and it will discharge about 1000 ft3/s of waste water into the river at all times. 
### The EPA rules state that this 
### brewery discharge cannot make up more than 50% of the total river discharge at any given 
### time. Looking at the plot of minimum 7 day Q vs. return interval, approximately 
### what is the return interval for a river flow that is composed of 50% 
### wasterwater discharge? What is the probability the brewery will violate the rule 
### during any given year? Would you give them the the permit to discharge? 
### Why or why not? 



######################################################################
### ANALYSIS 3: Flow regimes
######################################################################

### LOAD in some watershed characteristics data for the three sites
### click on the sites object and look at it.
### the drainage_area_va column is the watershed size in square miles (mi2).
### RUN THIS CODE
sites <- readNWISsite(siteNumbers = unique(data$site_no))

### PLOT 3: Make a plot of the annual flow hydrograph (Q vs day of year)
### where each year is color coded. Do this for each site. 
### You could use ggplot and facet_wrap or whatever plotting method you prefer.
### Make any other plots or inspect the data that may help you intepretation
### WRITE NEW CODE

fun_color_range <- colorRampPalette(c("red", "yellow"))  # Create color generating function
colorscheme <- fun_color_range(80)


ggp <- ggplot(data = data, aes(x=doy, y=Q, group_by=year, color=year))+
  labs(y="Discharge", x="Day of the Year") +
  geom_line() +
  facet_wrap(~ Name, scales="free")

ggp + scale_colour_gradientn(colors = colorscheme)

### QUESTION 3: MAKE some interpretations about the the flow regimes for each river
### (e.g. is flow have a distinct seasonality or not? why? When does the highest and lowest
### flows occur (if there is a norm), and why? How "flashy" are the flow regimes? 
### Why is one river more flashy than another? etc. etc.)
### Consider looking at these annual hydrographs and the watershed size in the sites dataframe
### to help your answers.

# Allegheny: Flashy discharge events (rain driven) throughout much of teh year. 


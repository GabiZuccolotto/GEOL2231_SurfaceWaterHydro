#####################################################################################
### Geol 1050/2150 Lab 1: Introduction to R (load and plotting discharge data)
#####################################################################################
### Practice some basic R coding following the steps below.
### Each line of comments includes some directions. Sometimes code will be started but
### you have to finish it, or insert arguments.
### Other times only directions will be provided.
### When finished, paste figures into a document 
### and submit as a your lab report as .doc or .pdf on Canvas.
#####################################################################################

library("tidyverse")
library("dataRetrieval")
library("ggplot2")

### Load in daily river discharge data using the dataRetrieval packes with the function
### readNWISdv. This function takes arguments of a USGS sitenumber, the parameter Code
### (here we use "00060" which is the code for discharge) and it loads all available 
### daily discharge values over the period of record. The column with discharge (ft3/s) data
### will be named  X_00060_00003.

fr<- readNWISdv(siteNumbers = "03021350", parameterCd = "00060")

al<- readNWISdv(siteNumbers = "03049500", parameterCd = "00060") 

######################################################################################

###QUESTION: What is oldest date on record for each dataset?

###ANSWER: French Creek = 10/01/1974, Allegheny River = 10/01/1938

######################################################################################

### Make new objects that are a subset of the Fr and Al dataframe to just data in 2015.
### There are many ways to do this using base R or tidyverse functions. We will try two ways.

# tidyverse filtering by date
fr_2015 <- fr %>%
  filter(Date >= "2015-01-01" & Date <= "2015-12-31")

# base R subsetting by data, filters by rows and then columns, since there is nothing after comma, its all columns.
al_2015 <- al[al$Date >= "2015-01-01" & al$Date <= "2015-12-31" , ]

### Make an annual hydrograph (Discharge over time or "Date") for French Creek and Allegheny River for the year 2015.
### Finish this code below putting adding to the data argument and indicating 
### what columns in the Fr and Al data frames are the x and y axes

ggplot(data= al_2015 ) +
  geom_line(aes(x = Date, y= X_00060_00003))


ggplot(data= fr_2015) +
  geom_line(aes(x = Date, y= X_00060_00003))

########################################################################################

### QUESTION: What is one thing you think is different about these hydrographs (1-2 sentences) and hypothesize why (1-2 sentences)?

### ANSWER: Both hydrographs exhibit similar temporal patterns for when high discharge events occur throughout the year.
### However, the Allegheny River has much larger variations in discharge and a higher baseflow compared French Creek. 
### This makes sense as French Creek is a small tributary of the mighty Allegheny. 
### French Creek also appears to be more prone to flash flooding as the hydrograph exhibits many smaller discharge events that peak quickly and recede quickly.

########################################################################################

### Paste code, the two figures, and answers to the questions in a doc or pdf and submit to canvas.



### Author: Gabi Zuccolotto
### Surface Water Hydrology
### Problem Set 2
### Question 5

library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

# Replicate the data table in a data frame called "data". 
# Make sure to convert cm -> m
data <- data.frame(
  Test = c(1:10),
  Sample = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
  Q_m3s = c(0.000025, 0.00004, 0.000052, 0.00008, 0.0001, 0.0000025, 0.000004, 0.0000052, 0.000008, 0.00001),
  h1_m = c(0.01, 0.02, 0.025, 0.034, 0.05, 0.005, 0.012, 0.032, 0.053, 0.060),
  h2_m = c(0.045, 0.060, 0.085, 0.116, 0.155, 0.135, 0.272, 0.312, 0.463, 0.710)
)

# Add columns for dh (change in head), dl (change in length) and a third column for the hydraulic gradient (dh/dl)
data$dh_m = data$h2_m - data$h1_m
data$dl_m = 0.10 
data$dhdl = data$dh_m/data$dl_m

# Add another column for hydraulic conductivity. 
# Calculate the hydraulic conductivity (K) using the equation... K = Q / A
# The area of the cylinder (A) can be calculated using the equation... A = π x r^2 where r = 0.1 m.
# The unit of K will be in m/s.
data$K = data$Q_m3s / (3.14*(0.1^2))

# Add another column for specific discharge. 
# Calculate the specific discharge (q) for each test using the equation... q = hydraulic conductivity (K) x hydraulic gradient (dh/dl)
# Hydraulic conductivity is in the unit of m/s and hydraulic gradient is a unitless ratio, so units for specific discharge will be m/s
data$spec_q = data$K*data$dhdl 

# Make two scatter plots; one for each sample, where dh is on the x-axis and specific discharge is on the y-axis. 
# Fit two lines through the data on each plot: one for sample A and one for sample B. 
plot <- ggplot(data) +
  geom_point(aes(dh_m, spec_q, color=Sample), size=3) +
  labs(title = "Hydraulic Conductivity (K)") +
  ylab("Specific discharge (m/s)") +
  xlab("Δh (m)")+
  geom_smooth(aes(dh_m, spec_q, color=Sample), method=lm, formula = y~x) 
print(plot)

# Write summary statistics to a new list called data_summary
# Note that the first two intercept and slope rows will be for Sed Samp A and the following two intercept and slope rows will be Sed Samp A + Sed Samp B
data_summary <- summary(lm(data=data, spec_q~dh_m*Sample))

# Write just the coefficient (slope and intercept) statistics to a new data frame called coeff
coeff <- as.data.frame(data_summary[["coefficients"]])

# Note that the first two intercept and slope rows will be for Sed Samp A and the following two intercept and slope rows will be Sed Samp A + Sed Samp B
# Create a new data frame with slope values for A and B called conductivity
conductivity <- data.frame (
  Sample_A = c(coeff$Estimate[2]),
  # To find the slope value for B, take slope of A + Slope of A+B
  Sample_B = c((coeff$Estimate[2])+(coeff$Estimate[4]))
)

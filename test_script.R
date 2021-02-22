# Initial Data Manipulation and Test Plots 
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

## Library ----
library(tidyverse)

fulldata <- read.csv("np_dr_averages.csv", header = TRUE)
str(fulldata)

np_only <- fulldata[fulldata$type == "NP", ]
dr_only <- fulldata[fulldata$type == "DR", ]
              

# initial plots
(all_plot <- ggplot(fulldata, aes(x = temp, y = avgDW)) +
                geom_point(aes(color = treatment_type)))

(np_plot <- ggplot(np_only, aes(x = temp, y = avgDW)) +
                geom_line(aes(color = treatment_type)))

(dr_plot <- ggplot(dr_only, aes(x = temp, y = avgDW)) +
                geom_line(aes(color = treatment_type)))
# respiration has acclimated to remain exactly the same as the control!! 


# using chlorophyll data instead
(np_chl_plot <- ggplot(np_only, aes(x = temp, y = avgChl)) +
    geom_line(aes(color = treatment_type)))

(dr_chl_plot <- ggplot(dr_only, aes(x = temp, y = avgChl)) +
    geom_line(aes(color = treatment_type)))

# basically the same thing, which is good! 
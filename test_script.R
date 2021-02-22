# Initial Data Manipulation and Test Plots 
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

## Library ----
library(tidyverse)

## Temperature Response Curves ----
fulldata <- read.csv("np_dr_averages.csv", header = TRUE)
str(fulldata)

np_only <- fulldata[fulldata$type == "NP", ]
dr_only <- fulldata[fulldata$type == "DR", ]
              

# initial temperature curve plots
(all_plot <- ggplot(fulldata, aes(x = temp, y = avgDW)) +
                geom_point(aes(color = treatment_type)))

(np_plot <- ggplot(np_only, aes(x = temp, y = avgDW)) +
                geom_line(aes(color = treatment_type)) +
                geom_point(aes(color = treatment_type)))

(dr_plot <- ggplot(dr_only, aes(x = temp, y = avgDW)) +
                geom_line(aes(color = treatment_type))+
                geom_point(aes(color = treatment_type)))
# respiration has acclimated to remain exactly the same as the control!! 


# using chlorophyll data instead
(np_chl_plot <- ggplot(np_only, aes(x = temp, y = avgChl)) +
    geom_line(aes(color = treatment_type)) +
    geom_point(aes(color = treatment_type)))

(dr_chl_plot <- ggplot(dr_only, aes(x = temp, y = avgChl)) +
    geom_line(aes(color = treatment_type)) +
    geom_point(aes(color = treatment_type)))

# basically the same thing, which is good! 


## Carbon Gain Efficiency ----
cgain <- read.csv("c_gain_efficiency.csv")

dw_cgain <- cgain %>% 
              dplyr::select(c("temp", "DRperc_DW", "NPperc_DW", "treatment_type"))

control_dw <- dw_cgain[dw_cgain$treatment_type == "control", ]
treatment_dw <- dw_cgain[dw_cgain$treatment_type == "treatment", ]

control_dw_long <- control_dw %>% 
                      gather(DWtype, percent, 2:3)
treatment_dw_long <- treatment_dw %>% 
                      gather(DWtype, percent, 2:3)

# making a stacked plot for control 
(stacked_control <- ggplot(control_dw_long, aes(x = temp, y = percent, fill = DWtype)) +
                      geom_bar(position = "fill", stat = "identity"))

(stacked_treatment <- ggplot(treatment_dw_long, aes(x = temp, y = percent, fill = DWtype)) +
                        geom_bar(position = "fill", stat = "identity"))

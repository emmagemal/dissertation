# Data Analysis
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

### Library ----
library(tidyverse)
library(retistruct)

### Data Manipulation ----
fulldata <- read.csv("Data/np_dr_averages.csv", header = TRUE)

fulldata <- fulldata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))

np_only <- fulldata %>% 
              filter(type == "NP")

np_control <- np_only %>% 
                  filter(treatment_type == "control")
np_treatment <- np_only %>% 
                    filter(treatment_type == "treatment")

### Calculating Optimum Temperature Ranges ----
summary(full_control)  # max NP = 1.3321
summary(full_treatment)  # max NP = 2.5546

# calculating 90% of the maximum net photosynthesis 
0.9*1.3321  # control = 1.19889
0.9*2.5546  # treatment = 2.29914

# visualizing the intersection points 
(np_plot <- ggplot(np_only, aes(x = temp, y = avgDW)) +
              geom_point(aes(color = treatment_type, shape = treatment_type), 
                             size = 2.5, alpha = 0.85) +
              geom_line(aes(color = treatment_type)) +
              facet_wrap(~treatment_type))

hline <- data.frame(z = c(1.19889, 2.29914), treatment_type = factor(c("control", "treatment")))
(np_plot <- np_plot + geom_hline(data = hline, aes(yintercept = z)))

## Determining the intersection points
# control intersection points
c1 <- c(5, 0.9957210)
c2 <- c(10, 1.3320533)
c3 <- c(10, 1.3320533)
c4 <- c(15, 0.4879867)
control_y <- c(0, 1.19889)
control_y2 <- c(20, 1.19889)

line.line.intersection(c1, c2, control_y, control_y2, interior.only = FALSE)  # x = 8.0204
line.line.intersection(c3, c4, control_y, control_y2, interior.only = FALSE)  # x = 10.7888

# treatment intersection points 
t1 <- c(5, 1.294007)
t2 <- c(10, 2.510977)
t3 <- c(15, 2.554641)
t4 <- c(20, 1.261462)
treatment_y <- c(0, 2.29914)
treatment_y2 <- c(30, 2.29914)

line.line.intersection(t1, t2, treatment_y, treatment_y2, interior.only = FALSE)  # x = 9.1297
line.line.intersection(t3, t4, treatment_y, treatment_y2, interior.only = FALSE)  # x = 15.9879


### Determining Negative NP Thresholds ----

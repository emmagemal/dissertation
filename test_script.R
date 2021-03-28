# Test Plots (PLOT SCRIPT ONLY, NOT STATS)
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

## Library ----
library(tidyverse)

## Temperature Response Curves ----
fulldata <- read.csv("Data/np_dr_averages.csv", header = TRUE)
str(fulldata)

fulldata <- fulldata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))
str(fulldata)

# temperature curve plots
(all_plot <- ggplot(fulldata, aes(x = temp, y = avgDW)) +
                geom_point(aes(color = treatment_type, shape = type), 
                           size = 2.5, alpha = 0.85))

(facet_plot <- ggplot(fulldata, aes(x = temp, y = avgDW)) +
                  geom_hline(yintercept = 0, color = "white", size = 1.5) +  # optional to keep              
                  geom_point(aes(shape = treatment_type, color = treatment_type),
                             size = 2) +
                  geom_line(aes(color = treatment_type)) +
                  facet_wrap(~type, nrow = 1) +
                  geom_errorbar(aes(ymin = avgDW-se_DW, ymax = avgDW+se_DW,
                                    color = treatment_type),
                                width = 0.5))  # USE THIS ONE I THINK! 
# respiration has acclimated to remain exactly the same as the control!! 

# using chlorophyll data instead
(facet_plot_chl <- ggplot(fulldata, aes(x = temp, y = avgChl)) +
                      geom_hline(yintercept = 0, color = "white", size = 1.5) +  # optional to keep              
                      geom_point(aes(shape = treatment_type, color = treatment_type),
                                 size = 2) +
                      geom_line(aes(color = treatment_type)) +
                      facet_wrap(~type, nrow = 1))
# basically the same relationship, which is good! Slightly different values of course 
# use to show that the relationship is the same, and explain that DW is used throughout
  # in order to allow scaling and comparison with other vegetation types 


## Carbon Gain Efficiency ----
cgain <- read.csv("Data/c_gain_efficiency.csv")  

# separating the control and treatment data 
control_dw <- cgain[cgain$treatment_type == "control", ]
treatment_dw <- cgain[cgain$treatment_type == "treatment", ]

# putting dark respiration and net photosynthesis values into a long column 
control_perc_long <- control_dw %>% 
                        gather(type, percent, 5:6) %>% 
                        mutate(type = case_when(type == "DRperc_DW" ~ "DR",
                                                type == "NPperc_DW" ~ "NP"))
control_ratio_long <- control_dw %>% 
                        gather(type, ratio, 3:4) %>% 
                        mutate(type = case_when(type == "DRratio_DW" ~ "DR",
                                                type == "NPratio_DW" ~ "NP"))
control_long_combo <- full_join(control_perc_long, control_ratio_long) %>% 
                        dplyr::select(c("temp", "GP_DW", "treatment_type",
                                        "type", "percent", "ratio"))
                      
treatment_perc_long <- treatment_dw %>% 
                          gather(type, percent, 5:6) %>% 
                          mutate(type = case_when(type == "DRperc_DW" ~ "DR",
                                                  type == "NPperc_DW" ~ "NP"))
treatment_ratio_long <- treatment_dw %>% 
                          gather(type, ratio, 3:4) %>% 
                          mutate(type = case_when(type == "DRratio_DW" ~ "DR",
                                                  type == "NPratio_DW" ~ "NP"))
treatment_long_combo <- full_join(treatment_perc_long, treatment_ratio_long) %>% 
                          dplyr::select(c("temp", "GP_DW", "treatment_type",
                                          "type", "percent", "ratio"))

# combining treatments for plotting
cgain_combo <- full_join(treatment_long_combo, control_long_combo)
write.csv(cgain_combo, "Data/c_gain_long.csv", row.names = FALSE) 

# making stacked plots 
(stacked_DWc <- ggplot(control_long_combo, aes(x = temp, y = percent, fill = type)) +
                    geom_bar(position = "fill", stat = "identity"))
# greatest proportion as DR/ smallest as NP at 20 degrees (similar to proportion at 15 though)

(stacked_DWt <- ggplot(treatment_long_combo, aes(x = temp, y = percent, fill = type)) +
                      geom_bar(position = "fill", stat = "identity"))
# greatest proportion as DR/smallest as NP at 25

(stacked_both <- ggplot(cgain_combo, aes(x = temp, y = percent, fill = type)) +
                    geom_bar(position = "fill", stat = "identity") +
                    facet_wrap(~treatment_type, nrow = 1))  # USE THIS I THINK!

# change in ratio between respiration and (gross) photosynthesis 
(dr_gp_plot <- ggplot(cgain, aes(x = temp, y = DRratio_DW)) +
                  geom_point(aes(color = treatment_type)) +
                  geom_line(aes(color = treatment_type, linetype = treatment_type)))
# treatment appears to have acclimated, has lower ratios = has smaller R 
 # or larger GP at higher temperatures 

## could potentially use a beta distribution for a glm model, as it uses values BETWEEN 0 and 1


## Acclimation Ratios ----
ratios <- read.csv("Data/acclim_ratios.csv")

(ratio_plot <- ggplot(ratios, aes(x = temp, y = DWc.t)) +
                  geom_point(aes(color = type, shape = type), size = 2.5, alpha = 0.9) +
                  geom_hline(yintercept = 1, linetype = "dotted"))

# DR = 10-30 degrees the dark respiration ratios are nearly identical/ very similar
  # could imply Q10 is the same for those temperatures, so >5 degrees 
# closer to 1 would imply acclimation to the same rates as the control at optimal temp
  # >1 would imply increased rates compared to the control 
  # closer to 0 would imply less acclimation 
# DR 2 and 5 degrees > 1, so higher respiration rates
# NP 25 > 1, so higher net photosynthesis rates (>2x) and thus increased productivity 


## Light Response Curves ----
light <- read.csv("Data/light_responses.csv")
str(light)
summary(light)  # max control = 2.881, max treatment = 14.420 

0.9*2.881  # 90% max control = 2.593
0.9*14.420  # 90% max treatment = 12.978

light <- light %>% 
            pivot_longer(cols = 2:3,
                         names_to = "treatment_type",
                         names_prefix = "avgCO2_",
                         values_to = "avgCO2")

light_control <- light %>% 
                  filter(treatment_type == "control")
light_treatment <- light %>% 
                      filter(treatment_type == "treatment")

# plotting the light response curves
(light_plots <- ggplot(light, aes(x = Lcuv, y = avgCO2)) +
                  geom_point() +
                  geom_line() +
                  facet_wrap(~treatment_type))

## OR do (probably best not to include the lines tbh... shows inaccuracy in estimates)
(light_plots_c <- ggplot(light_control, aes(x = Lcuv, y = avgCO2)) +
                    geom_vline(xintercept = 780, size = 20, alpha = 0.3) +                
                    geom_point() +
                    geom_line()) 
(light_plots_t <- ggplot(light_treatment, aes(x = Lcuv, y = avgCO2)) +
                    geom_vline(xintercept = 400, size = 20, alpha = 0.3) +                
                    geom_point() +
                    geom_line()) 
  
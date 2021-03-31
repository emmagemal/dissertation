# Plots for dissertation 
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

## Library ----
library(tidyverse)

## Temperature Response Curves ----
avgdata <- read.csv("Data/np_dr_averages.csv", header = TRUE)
str(avgdata)

avgdata <- avgdata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))
str(avgdata)

# temperature curve plots
(all_plot <- ggplot(avgdata, aes(x = temp, y = avgDW)) +
                geom_point(aes(color = treatment_type, shape = type), 
                           size = 2.5, alpha = 0.85))

# FOR DISSERTATION 
(facet_plot <- ggplot(avgdata, aes(x = temp, y = avgDW)) +
                  geom_hline(yintercept = 0, color = "grey", size = 1) +  # optional to keep              
                  geom_point(aes(shape = treatment_type, color = treatment_type),
                             size = 2.2) +
                  geom_line(aes(color = treatment_type)) +
                  facet_wrap(~type, nrow = 1) +
                  geom_errorbar(aes(ymin = avgDW-se_DW, ymax = avgDW+se_DW,
                                    color = treatment_type),
                                width = 0.5) +
                  scale_color_manual(values = c("#12A7B8", "#004452")) +
                  theme_bw())  
# respiration has acclimated to remain exactly the same as the control!! 

# using chlorophyll data instead
(facet_plot_chl <- ggplot(avgdata, aes(x = temp, y = avgChl)) +
                      geom_hline(yintercept = 0, color = "grey", size = 1) +  # optional to keep              
                      geom_point(aes(shape = treatment_type, color = treatment_type),
                                 size = 2.2) +
                      geom_line(aes(color = treatment_type)) +
                      facet_wrap(~type, nrow = 1) +
                      scale_color_manual(values = c("#12A7B8", "#004452")) +
                      theme_bw())
# basically the same relationship, which is good! Slightly different values of course 
# use to show that the relationship is the same, and explain that DW is used throughout
  # in order to allow scaling and comparison with other vegetation types 


## Carbon Gain Efficiency ----
cgain <- read.csv("Data/c_gain_long.csv") 
cgain_wide <- read.csv("Data/c_gain_efficiency.csv")  # if keeping the 2nd plot 

# making stacked plot
# FOR DISSERTATION
(stacked_both <- ggplot(cgain, aes(x = temp, y = percent, fill = type)) +
                    geom_bar(position = "fill", stat = "identity") +
                    facet_wrap(~treatment_type, nrow = 1) +
                    scale_fill_manual(values = c("#FF6D33", "#7A292A")) +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +  # OR
               #    scale_y_continuous(expand = c(0,0)) +
                    theme_bw())
# idk if I need this one: 
# change in ratio between respiration and (gross) photosynthesis 
(dr_gp_plot <- ggplot(cgain_wide, aes(x = temp, y = DRratio_DW)) +
                  geom_point(aes(color = treatment_type), size = 2) +
                  geom_line(aes(color = treatment_type, linetype = treatment_type)) +
                  scale_color_manual(values = c("#12A7B8", "#004452")))
# treatment appears to have acclimated, has lower ratios = has smaller R 
 # or larger GP at higher temperatures 


## Acclimation Ratios ----
ratios <- read.csv("Data/acclim_ratios.csv")

(ratio_plot <- ggplot(ratios, aes(x = temp, y = DWc.t)) +
                  geom_point(aes(color = type, shape = type), size = 2.5, alpha = 0.9) +
               #  geom_line(aes(color = type)) +  # don't know if I need to connect them
                  geom_hline(yintercept = 1, linetype = "dotted") +
                  theme_bw() +
                  scale_color_manual(values = c("#FF6D33", "#7A292A")))

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
vline <- data.frame(z = c(750, 300), treatment_type = factor(c("control", "treatment")))

(light_plots <- ggplot(light, aes(x = Lcuv, y = avgCO2)) +
                  geom_point(aes(color = treatment_type), size = 2.2) +
                  geom_line(aes(color = treatment_type)) +
                  facet_wrap(~treatment_type) +
             #     geom_vline(data = vline, aes(xintercept = z), size = 10, alpha = 0.3) +              
                  theme_bw() +
                  scale_color_manual(values = c("#12A7B8", "#004452")))





#### creating c_gain_long.csv
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
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

# plotting temperature response curves using dry weight  
(dw_plot <- ggplot(avgdata, aes(x = temp, y = avgDW, color = treatment_type)) +
              geom_hline(yintercept = 0, color = "grey", size = 0.8) +  # optional to keep              
              geom_point(aes(shape = type), size = 2.2) +
              geom_line(aes(linetype = type)) +
              ylab(label = expression(Average~NP~per~dry~weight~(nmol~g^-1~s^-1))) +
              xlab(label = "Temperature (˚C)") +              
              geom_errorbar(aes(ymin = avgDW-se_DW, ymax = avgDW+se_DW), width = 0.5) +
              theme_bw() +
              theme(axis.title.x = 
                      element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                    axis.title.y = 
                      element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    panel.grid.minor = element_blank()) +
              theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
              scale_color_manual(values = c("#12A7B8", "#004452"),
                                 name = c("Treatment Type", "Process"),
                                 labels = c("Control", "Treatment")) +
              scale_linetype_discrete(name = c("Process", "Treatment Type")) +
              scale_shape_discrete(name = c("Process", "Treatment Type")))  

ggsave("Figures/diss_figures/t_response_DW.png", plot = dw_plot, 
       width = 6.5, height = 5.5, units = "in")

# plotting curves using chlorophyll content 
(chl_plot <- ggplot(avgdata, aes(x = temp, y = avgChl, color = treatment_type)) +
                geom_hline(yintercept = 0, color = "grey", size = 0.8) +  # optional to keep              
                geom_point(aes(shape = type), size = 2.2) +
                geom_line(aes(linetype = type)) +
                geom_errorbar(aes(ymin = avgChl-se_Chl, ymax = avgChl+se_Chl), width = 0.5) +
                ylab(label = 
                       expression(Average~NP~per~chlorophyll~content~(nmol~mg^-1~s^-1))) +
                xlab(label = "Temperature (˚C)") +              
                theme_bw() +
                theme(axis.title.x = 
                        element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                      axis.title.y = 
                        element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)), 
                        panel.grid.minor = element_blank()) +
                theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
                scale_color_manual(values = c("#12A7B8", "#004452"),
                                   name = c("Treatment Type", "Process"),
                                   labels = c("Control", "Treatment")) +
                scale_linetype_discrete(name = c("Process", "Treatment Type")) +
                scale_shape_discrete(name = c("Process", "Treatment Type")) +
                scale_y_continuous(breaks = seq(-20, 10, 5)))

ggsave("Figures/diss_figures/t_response_chl.png", plot = chl_plot, 
      width = 6.5, height = 5.5, units = "in")


## Carbon Gain Efficiency ----
cgain <- read.csv("Data/c_gain_long.csv") 

str(cgain)

# making stacked plots of DR:NP 
(stacked_both <- ggplot(cgain, aes(x = temp, y = percent, fill = type)) +
                    geom_bar(position = "fill", stat = "identity") +
                    ylab(label = "Percentage of gross photosynthesis (%)") +
                    xlab(label = "Temperature (˚C)") +              
                    facet_wrap(~treatment_type, nrow = 1) +
                    theme_bw() +
                    theme(axis.title.x = 
                            element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                          axis.title.y = 
                            element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
                    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
                          panel.spacing = unit(1, "cm")) + 
                    scale_fill_manual(values = c("#FF6D33", "#7A292A"),
                                      name = "Process") +  # could change the name 
                    scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                                       labels = scales::percent_format(suffix = "")))  # OR
               #    scale_y_continuous(expand = c(0,0)))

ggsave("Figures/diss_figures/c_gain_stacked2.png", plot = stacked_both, 
       width = 8, height = 5.5, units = "in")


## Acclimation Ratios ----
ratios <- read.csv("Data/acclimation_ratios.csv")

(ratio_plot <- ggplot(ratios, aes(x = temp, y = DWt.c)) +
                  geom_point(aes(color = type, shape = type), size = 2.5, alpha = 0.9) +
                  geom_hline(yintercept = 1, linetype = "dotted") +
                  ylab(label = "Acclimation ratio") +
                  xlab(label = "Temperature (˚C)") +
                  theme_bw() +
                  theme(axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = 
                          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                        panel.grid.minor = element_blank()) +
                  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                  scale_color_manual(values = c("#FF6D33", "#7A292A"),
                                     name = "Process") +
                  scale_shape_discrete(name = "Process") + # can change the name
                  scale_y_continuous(limits = c(0, 6)))

ggsave("Figures/diss_figures/acclim_ratio_plot.png", plot = ratio_plot, 
       width = 6, height = 5.5, units = "in")


## Light Response Curves ----
light <- read.csv("Data/full_light_responses.csv") 
str(light)

# calculating averages, standard deviation and standard error
light_sum <- light %>% 
                group_by(Lcuv, treatment_type) %>% 
                summarise(avgCO2 = mean(CO2),
                          sdCO2 = sd(CO2)) %>% 
                mutate(seCO2 = sdCO2/sqrt(3)) %>% 
                na.omit()

# plotting the light response curves
(light_plots <- ggplot(light_sum, aes(x = Lcuv, y = avgCO2)) +
                  geom_hline(yintercept = 0, size = 0.5, linetype = "dotted") +               
                  geom_point(aes(color = treatment_type), size = 2.2) +
                  geom_line(aes(color = treatment_type)) +
                  geom_errorbar(aes(ymin = avgCO2-seCO2, ymax = avgCO2+seCO2, 
                                    color = treatment_type, width = 20), alpha = 0.8) + 
                  ylab(label = expression(paste(
                       "Average ", "\u0394", "CO"[2], " (rel. ppm)"))) +  # check units 
                  xlab(label = expression(paste(
                       "PPFD ", "(µmol ", "m"^-2, " s"^-1, ")"))) +
                  theme_bw() +
                  theme(panel.grid.minor = element_blank(),
                        axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
                  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
                  scale_color_manual(values = c("#12A7B8", "#004452"),
                                     name = "Treatment Type",
                                     labels = c("Control", "Treatment"))) 

ggsave("Figures/diss_figures/light_response.png", plot = light_plots, 
       width = 7, height = 5.5, units = "in")





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
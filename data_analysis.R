# Data Analysis and Calculations
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

### Library ----
library(tidyverse)
library(retistruct)
library(lmtest)
library(lme4)
library(car)

### Data Manipulation ----
fulldata <- read.csv("Data/raw_np_dr_data.csv", header = TRUE)
avgdata <- read.csv("Data/np_dr_averages.csv", header = TRUE)

str(fulldata)
str(avgdata)

fulldata <- fulldata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type),
                     sample = as.factor(sample))

avgdata <- avgdata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))

# subsetting average NP and DR for calculations
np_only <- avgdata %>% 
              filter(type == "NP")
dr_only <- avgdata %>% 
              filter(type == "DR")

np_control <- np_only %>% 
                filter(treatment_type == "control")
np_treatment <- np_only %>% 
                  filter(treatment_type == "treatment")
dr_control <- dr_only %>% 
                filter(treatment_type == "control")
dr_treatment <- dr_only %>% 
                  filter(treatment_type == "treatment")

# subsetting NP and DR (full) for models 
np_full <- fulldata %>% 
              filter(type == "NP")
dr_full <- fulldata %>% 
              filter(type == "DR")


### Calculating Optimum Temperature Ranges ----
summary(np_control)  # max control NP = 1.3321
summary(np_treatment)  # max treatment NP = 2.5546

summary(dr_control)  # max control DR (min. number) = -6.1712
summary(dr_treatment)  # max treatment DR = -6.7696

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

line.line.intersection(c1, c2, control_y, control_y2, 
                       interior.only = FALSE)               # x = 8.0204˚C
line.line.intersection(c3, c4, control_y, control_y2, 
                       interior.only = FALSE)               # x = 10.7888˚C

# treatment intersection points 
t1 <- c(5, 1.294007)
t2 <- c(10, 2.510977)
t3 <- c(15, 2.554641)
t4 <- c(20, 1.261462)
treatment_y <- c(0, 2.29914)
treatment_y2 <- c(30, 2.29914)

line.line.intersection(t1, t2, treatment_y, treatment_y2, 
                       interior.only = FALSE)               # x = 9.1297˚C
line.line.intersection(t3, t4, treatment_y, treatment_y2, 
                       interior.only = FALSE)               # x = 15.9879˚C


### Determining Negative NP Thresholds ----
(neg_np_plot <- ggplot(np_only, aes(x = temp, y = avgDW)) +
                   geom_point(aes(color = treatment_type, shape = treatment_type), 
                              size = 2.5, alpha = 0.85) +
                   geom_line(aes(color = treatment_type)) +
                   geom_hline(aes(yintercept = 0)) +  # plotting a line at 0 
                   facet_wrap(~treatment_type))

## Determining the intersection points
# control intersection points
c1_neg <- c(15, 0.4879867)
c2_neg <- c(20, -0.6376730)
control_y_neg <- c(10, 0)
control_y2_neg <- c(25, 0)

line.line.intersection(c1_neg, c2_neg, control_y_neg, 
                       control_y2_neg, interior.only = FALSE)      # x = 17.1676˚C 

# treatment intersection points 
t1_neg <- c(25, 0.4504340)
t2_neg <- c(30, -1.3601202)
treatment_y_neg <- c(20, 0)
treatment_y2_neg <- c(35, 0)

line.line.intersection(t1_neg, t2_neg, treatment_y_neg, 
                       treatment_y2_neg, interior.only = FALSE)    # x = 26.2439˚C 


### Models for NP ----
# checking for normality of the data 
(hist <- ggplot(fulldata, aes(x = np_DW)) +
           geom_histogram(color = "black") +
           theme_classic() +
           scale_y_continuous(expand = c(0,0)))

## Mixed effects models 
mixed_null_np <- lmer(np_DW ~ 1 + (1|sample), data = np_full, REML = F)
mixed_sample_np <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = np_full, REML = T)
mixed_int_sample_np <- lmer(np_DW ~ temp*treatment_type + (1|sample), data = np_full, REML = T)
# sample as a random effect because sample needs to be controlled for, but I am not
# interested in the direct relationship of it with np_DW 

# comparing the models 
#### to compare models you need REML = F, but to do an F-test rather than chi squared test
  # you need to REML = T 
AIC(mixed_null_np, mixed_sample_np, mixed_int_sample_np)  # lowest AIC = mixed_sample_np
  # also has fewer degrees of freedom than mixed_int_sample_np

anova(mixed_null_np, mixed_sample_np)  # mixed_sample_np is better than the null model
anova(mixed_null_np, mixed_int_sample_np)  # mixed_int_sample_np is better than null model
anova(mixed_sample_np, mixed_int_sample_np)  # interaction is NOT significantly better

# model outputs
confint(mixed_sample_np)  # temp: -0.146 to -0.0817
                          # treatment_type: 0.253 to 2.665

summary(mixed_sample_np)  # sample explains quite a bit of the excess variation 
                          # temp: -0.114 ± 0.0163 (within the confidence intervals)
                          # treatment_type: 1.457 ± 0.621 (within the confidence intervals)
# treatment_type and temp significantly effect NP
# they're significantly different from 0 according to the CI's
0.9157/(0.9157+1.4169)    # sample explains 39.3% of the residual variation 
                          ##### (or however you explain it)

Anova(mixed_sample_np, type = "III", test = "F")  # temp: F = 49.05, p = 02.66e-9
                                                  # treatment_type: F = 5.50, p = 0.041
# temp and treatment_type significantly effect NP after accounting for between sample variation

# type 3 = how much variability in NP can be attributed to be temp after accounting 
  # for everything else. then how much can be attributed to treatment after accounting for 
  # everything else, etc. 
# type 1 would be how much is attributed to temp, then how much of the leftover variability
  # is explained by treatment, then how much that's left is explained by the rest, etc. 
  # (order matters for type 1)


### Models for DR ----
## Mixed effect models 
mixed_null_dr <- lmer(np_DW ~ 1 + (1|sample), data = dr_full, REML = F)
mixed_sample_dr <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = dr_full, REML = T)
mixed_int_sample_dr <- lmer(np_DW ~ temp*treatment_type + (1|sample), 
                            data = dr_full, REML = F)

# comparing the models 
AIC(mixed_null_dr, mixed_sample_dr, mixed_int_sample_dr)  # mixed_int_sample_dr = lowest AIC

anova(mixed_sample_dr, mixed_int_sample_dr)  # interaction is NOT significantly better
  # extra coefficients make it not worth it 

# model outputs 
confint(mixed_sample_dr)  # temp: -0.243 to -0.192 
                          # treatment_type: -1.492 to 1.425 

summary(mixed_sample_dr)  # sample explains most of the variance 
                          # temp: -0.217 ± 0.0130 
                          # treatment_type: -0.0338 ± 0.751
# treatment std. error is >> treatment estimate 
  # implies treatment_type does not have a large effect on DR 
# temp is significant, treatment_type is not 
# from the CI's, temp is significantly different from 0 but treatment_type isn't (CI's include 0)
  # treatment_type could include 0 = has no effect on DR, so therefore it's not significant 
# basically means the difference between treatments could be 0 
1.5371/(1.5371+0.9043)  # sample explains 63.0% of the residual variation

Anova(mixed_sample_dr, type = "III", test = "F")  # temp: F = 280.39, p = <2e-16 
                                                  # treatment_type: F = 0.0020, p = 0.965
# treatment_type is not significant, temp is significant 
# no difference between control and treatment = good, implies acclimation of DR

# a large F ratio = means are not equal, variabiltiy between group means is larger than within
  # = means it's significant

### Carbon Gain Models ----
cgain <- read.csv("Data/c_gain_long.csv")

# extracting only NP data (just need one of them really because I want to see if the ratio
# between NP and DR changes, and the percentage/ratio is already calculated)
cgain_np <- cgain %>% 
              filter(type == "NP")

# creating models 
null_c <- lm(percent ~ 1, data = cgain_np)
c_temp <- lm(percent ~ temp, data = cgain_np)
c_ttype <- lm(percent ~ temp + treatment_type, data = cgain_np)
c_int <- lm(percent ~ temp*treatment_type, data = cgain_np)

c_mixed <- lmer(percent ~ temp + (1|treatment_type), data = cgain_np)  # treatment_type
                                                            # explains little variation
summary(c_ttype)

AIC(null_c, c_temp, c_ttype, c_int)  # all better than null model, others aren't very different
aov_c <- aov(c_ttype)  # there's no significant difference between treatment types it seems 


c_ttest <- t.test(ratio ~ temp, data = cgain)
# to do t test, the grouping factor (predictor) can only have 2 levels 


### Average Light Response Curve Calculations ----
light <- read.csv("Data/avg_light_responses.csv")

summary(light)  # max control = 2.881, max treatment = 14.420 

0.9*2.881  # 90% max control = 2.593
0.9*14.420  # 90% max treatment = 12.978

## Determining the intersection points for 90% LSP
# visualizing the 90% light saturation point 
(light_plot <- ggplot(light, aes(x = Lcuv, y = avgCO2)) +
    geom_point(aes(color = treatment_type)) +
    geom_line(aes(color = treatment_type)) +
    facet_wrap(~treatment_type))

hline_light <- data.frame(z = c(2.593, 12.978), treatment_type = factor(c("control", "treatment")))

(lightsat_plot <- light_plot + 
                    geom_hline(data = hline_light, aes(yintercept = z)))

# control intersection point
c1_l <- c(500, 1.069)
c2_l <- c(1000, 2.881)
control_y_l <- c(500, 2.593)
control_y2_l <- c(1000, 2.593)

line.line.intersection(c1_l, c2_l, control_y_l, control_y2_l, 
                       interior.only = FALSE)               # x = 920.5 µE m^-2 s^-1

# treatment intersection point
t1_l <- c(200, 9.200)
t2_l <- c(500, 14.420)
treatment_y_l <- c(200, 12.978)
treatment_y2_l <- c(500, 12.978)

line.line.intersection(t1_l, t2_l, treatment_y_l, treatment_y2_l, 
                       interior.only = FALSE)               # x = 417 µE m^-2 s^-1

## Determining the intersection points for LCP
# visualizing the light compensation point  
(lightcomp_plot <- light_plot + 
                      geom_hline(yintercept = 0))

# control intersection point
c1_lc <- c(200, -1.809)
c2_lc <- c(500, 1.069)
control_y_lc <- c(200, 0)
control_y2_lc <- c(500, 0)

line.line.intersection(c1_lc, c2_lc, control_y_lc, control_y2_lc, 
                       interior.only = FALSE)               # x = 388.6 µE m^-2 s^-1

# treatment intersection point
t1_lc <- c(50, -1.620)
t2_lc <- c(100, 3.600)
treatment_y_lc <- c(50, 0)
treatment_y2_lc <- c(100, 0)

line.line.intersection(t1_lc, t2_lc, treatment_y_lc, treatment_y2_lc, 
                       interior.only = FALSE)               # x = 65.5 µE m^-2 s^-1


### Light Response Curve Statistics ----
full_light <- read.csv("Data/full_light_responses.csv")

light_sum <- full_light %>% 
                group_by(sample) %>% 
                summarize(maxCO2 = max(CO2)) %>% 
                mutate(LSPx90 = 0.9*maxCO2)

# fixing negative values so 90% of the LSP is correct
-9.70*1.1   # -10.67 
-15.00*1.1  # -16.5

light_sum$LSPx90[light_sum$maxCO2 == -9.70] <- -10.67
light_sum$LSPx90[light_sum$maxCO2 == -15.00] <- -16.5

## Determining the intersection points for 90% LSP
# visualizing the 90% light saturation points for all samples
(light_plot_facet <- ggplot(full_light, aes(x = Lcuv, y = CO2)) +
                        geom_point(aes(color = treatment_type)) +
                        geom_line(aes(color = treatment_type)) +
                        facet_wrap(~sample) +
                        geom_hline(data = light_sum, aes(yintercept = LSPx90)))

# C1 intersection point
c1_1 <- c(500, -11.07)
c1_2 <- c(1000, -10.54)
control_y_c1 <- c(500, -10.670)
control_y2_c1 <- c(1100, -10.670)

line.line.intersection(c1_1, c1_2, control_y_c1, control_y2_c1, 
                       interior.only = FALSE)               # x = 877.4 µE m^-2 s^-1

# C2 intersection point
c2_1 <- c(500, 4.33)
c2_2 <- c(1000, 5.36)
control_y_c2 <- c(500, 4.824)
control_y2_c2 <- c(1000, 4.824)

line.line.intersection(c2_1, c2_2, control_y_c2, control_y2_c2, 
                       interior.only = FALSE)               # x = 739.8 µE m^-2 s^-1

# C3 intersection point
c3_1 <- c(500, 9.95)
c3_2 <- c(1000, 13.83)
control_y_c3 <- c(500, 12.960)
control_y2_c3 <- c(1000, 12.960)

line.line.intersection(c3_1, c3_2, control_y_c3, control_y2_c3, 
                       interior.only = FALSE)               # x = 887.9 µE m^-2 s^-1

# T1 intersection point
t1_1 <- c(50, -16.90)
t1_2 <- c(100, -15.90)
treatment_y_t1 <- c(50, -16.500)
treatment_y2_t1 <- c(100, -16.500)

line.line.intersection(t1_1, t1_2, treatment_y_t1, treatment_y2_t1, 
                       interior.only = FALSE)               # x = 70.0 µE m^-2 s^-1

# T2 intersection point
t2_1 <- c(200, 17.54)
t2_2 <- c(500, 23.57)
treatment_y_t2 <- c(200, 21.213)
treatment_y2_t2 <- c(500, 21.213)

line.line.intersection(t2_1, t2_2, treatment_y_t2, treatment_y2_t2, 
                       interior.only = FALSE)               # x = 382.7 µE m^-2 s^-1

# T3 intersection point
t3_1 <- c(200, 25.05)
t3_2 <- c(500, 35.00)
treatment_y_t3 <- c(200, 34.920)
treatment_y2_t3 <- c(500, 34.920)

line.line.intersection(t3_1, t3_2, treatment_y_t3, treatment_y2_t3, 
                       interior.only = FALSE)               # x = 497.6 µE m^-2 s^-1

light_sum$LSPcuv <- c(877.4, 739.8, 887.9, 70.0, 382.7, 497.6)
light_sum$treatment_type <- c("control", "control", "control", 
                              "treatment", "treatment", "treatment")              

## T-Test for LSP
t.test(LSPcuv ~ treatment_type, data = light_sum)  # p = 0.042 (significant difference!)
# using the means of the calculated LSPcuv, control = 835 and treatment = 316 
  # (different to pre-averaged data)



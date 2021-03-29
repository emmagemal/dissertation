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
avgdata <- read.csv("Data/np_dr_averages.csv", header = TRUE)
fulldata <- read.csv("Data/raw_np_dr_data.csv", header = TRUE)

str(fulldata)
str(avgdata)

fulldata <- fulldata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type),
                     sample = as.factor(sample))

avgdata <- avgdata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))

# subsetting NP and DR  
np_only <- avgdata %>% 
              filter(type == "NP")

np_only_full <- fulldata %>% filter(type == "NP")

dr_only <- avgdata %>% 
              filter(type == "DR")

np_control <- np_only %>% 
                  filter(treatment_type == "control")
np_treatment <- np_only %>% 
                    filter(treatment_type == "treatment")

### Calculating Optimum Temperature Ranges ----
summary(np_control)  # max NP = 1.3321
summary(np_treatment)  # max NP = 2.5546

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


### Modelling ----
# checking for normality of the data 
(hist <- ggplot(fulldata, aes(x = np_DW)) +
             geom_histogram(color = "black") +
             theme_classic() +
             scale_y_continuous(expand = c(0,0)))

## Simple linear models 
null <- lm(np_DW ~ 1, data = fulldata)
temp <- lm(np_DW ~ temp, data = fulldata)
temp_ttype <- lm(np_DW ~ temp + treatment_type, data = fulldata)
temp_type <- lm(np_DW ~ temp + type, data = fulldata)
temp_ttype_type <- lm(np_DW ~ temp + treatment_type + type, data = fulldata)
temp_ttype_int <- lm(np_DW ~ temp*treatment_type, data = fulldata)
temp_type_int <- lm(np_DW ~ temp*type, data = fulldata)
temp_ttype_type_int <- lm(np_DW ~ temp + treatment_type*type, data = fulldata)
temp_ttype_int_type <- lm(np_DW ~ temp*treatment_type + type, data = fulldata)
all_int <- lm(np_DW ~ temp*treatment_type*type, data = fulldata)

AIC(null, temp, temp_ttype, temp_type, temp_ttype_type, temp_ttype_int, 
    temp_type_int, temp_ttype_type_int, temp_ttype_int_type, all_int)
# all_int is the best model of these 

# including sample to see its effect
int_sample <- lm(np_DW ~ temp*treatment_type*type + sample, data = fulldata)
all_int2 <- lm(np_DW ~ temp*treatment_type*type*sample, data = fulldata)

AIC(null, all_int, int_sample, all_int2)  # all_int2 is best, but there's 49 parameters...

# checking model assumptions 
hist(residuals(all_int2))   
shapiro.test(residuals(all_int2))  # p < 0.05, residuals are not normally distributed 

plot(all_int2)    # some possible outliers, rows 29, 20, 39
bptest(all_int2)  # p < 0.05, there is heteroskedasticity in the model 

# attempting to transform the data 
fulldata_cut <- fulldata[-c(29, 30, 39), ]
all_int3 <- lm(np_DW ~ temp*treatment_type*type*sample, data = fulldata_cut)

plot(all_int3)   # outliers: row 18, 37, 111
bptest(all_int3) # no heteroskedasticity anymore 
shapiro.test(resid(all_int3))  # still no normally distributed residuals 

# results
summary(all_int3)   # temp has a significant effect on avgDW (p = 1.92e-8)
                   # type has significant effect on avgDW (NP groups are significantly 
                      # different to each other it seems to say) (p = 0.0233)
                   # temp*type interaction is significant, so the effect on temperature on 
                      # avgDW depends on the type (makes sense)
# adjusted R^2 = 0.9451 

# using type 3 errors because 
Anova(all_int2, type = "III")   # significant interactions = temp*type 
                               # means the first depends on the second 
# effect of temperature on avgDW depends on type
# temp is significant after controlling for treatment type and type 
# type has a significant effect on avgDW 


## Creating mixed effects models 
mixed_type <- lmer(np_DW ~ temp + treatment_type + (1|type), data = fulldata, REML = F)
  # type as a random effect because DR and NP need to be controlled for, but I am not
  # interested in the direct relationship of it with avgDW 

# type only has 2 levels and it would be good to be able to make predictions about
  # differences between type = probably better not to have it as a random effect at all 

# alternative mixed model
mixed_sample <- lmer(np_DW ~ temp + treatment_type + type + (1|sample), 
                     data = fulldata, REML = F)
mixed_int_sample <- lmer(np_DW ~ temp*treatment_type*type + (1|sample), 
                      data = fulldata, REML = F)

anova(mixed_type, all_int)  # all_int is better at explaining the relationship
anova(mixed_type, mixed_sample)
anova(mixed_sample, all_int)
anova(mixed_int_sample, all_int)

Anova(mixed_int_sample, type = "III")
plot(mixed_int_sample)

## Separate ANCOVAs for NP and DR 
# net photosynthesis (NP)
np_temp <- lm(avgDW ~ temp, data = np_only)
np_treatment_m <- lm(avgDW ~ treatment_type, data = np_only)
np_both <- lm(avgDW ~ temp + treatment_type, data = np_only)
np_int <- lm(avgDW ~ temp*treatment_type, data = np_only)

AIC(np_temp, np_treatment_m, np_both, np_int)  # np_both is the best 

# checking model assumptions
plot(np_both)
shapiro.test(resid(np_both))  # residuals are normally distributed 
bptest(np_both)   # no heteroskedasticity 

Anova(np_both, type = "III")  # treatmemt_type is significant (p = 0.0057765, F = 11.661) 
                              # = there's a difference between control and treatment
# temp has a significant effect on avgDW (p = 0.0001229, F = 33.404)
summary(np_both)  # adjusted R^2 = 0.7681 

# dark respiration (DR)
dr_temp <- lm(avgDW ~ temp, data = dr_only)
dr_treatment_m <- lm(avgDW ~ treatment_type, data = dr_only)
dr_both <- lm(avgDW ~ temp + treatment_type, data = dr_only)
dr_int <- lm(avgDW ~ temp*treatment_type, data = dr_only)

AIC(dr_temp, dr_treatment_m, dr_both, dr_int)   # dr_temp and dr_both are very similar 

# checking model assumptions
plot(dr_both)  # some potential outliers 
shapiro.test(resid(dr_both))  # residuals are NOT normally distributed 
bptest(dr_both)   # no heteroskedasticity 

Anova(dr_both, type = "III")  # temp has a significant effect (p = 2.24e-10, F = 469.556)
# treatment_type is non-significant (p = 0.611, F = 0.274)
  # no difference between control and treatment = good, implies acclimation of DR
summary(dr_both)   # adjusted R^2 = 0.973 

## Differences in carbon gain
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
summary(c_mixed)

AIC(null_c, c_temp, c_ttype, c_int)  # all better than null model, others aren't very different
aov_c <- aov(c_ttype)  # there's no significant difference between treatment types it seems 



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
fulldata <- read.csv("Data/np_dr_averages.csv", header = TRUE)

fulldata <- fulldata %>% 
              mutate(treatment_type = as.factor(treatment_type),
                     type = as.factor(type))

np_only <- fulldata %>% 
              filter(type == "NP")
dr_only <- fulldata %>% 
              filter(type == "DR")

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
(hist <- ggplot(fulldata, aes(x = avgDW)) +
             geom_histogram(color = "black") +
             theme_classic() +
             scale_y_continuous(expand = c(0,0)))

shapiro.test(fulldata$avgDW)   # p > 0.05, it IS normally distributed 

## Simple linear models 
null <- lm(avgDW ~ 1, data = fulldata)
temp <- lm(avgDW ~ temp, data = fulldata)
treatment <- lm(avgDW ~ treatment_type, data = fulldata)
temp_ttype <- lm(avgDW ~ temp + treatment_type, data = fulldata)
temp_type <- lm(avgDW ~ temp + type, data = fulldata)
temp_ttype_type <- lm(avgDW ~ temp + treatment_type + type, data = fulldata)
temp_ttype_int <- lm(avgDW ~ temp*treatment_type, data = fulldata)
temp_type_int <- lm(avgDW ~ temp*type, data = fulldata)
temp_ttype_type_int <- lm(avgDW ~ temp + treatment_type*type, data = fulldata)
temp_ttype_int_type <- lm(avgDW ~ temp*treatment_type + type, data = fulldata)
all_int <- lm(avgDW ~ temp*treatment_type*type, data = fulldata)

AIC(null, treatment, temp, temp_ttype, temp_type, temp_ttype_type, temp_ttype_int, 
    temp_type_int, temp_ttype_type_int, temp_ttype_int_type, all_int)
# all_int is the best model of these 

# checking model assumptions 
hist(residuals(all_int))   
shapiro.test(residuals(all_int))  # residuals are normally distributed 

plot(all_int)    # potential non-linear relationship present 
bptest(all_int)  # no heteroskedasticity in the model 


# results
summary(all_int)   # temp has a significant effect on avgDW (p = 1.92e-8)
                   # type has significant effect on avgDW (NP groups are significantly 
                      # different to each other it seems to say) (p = 0.0233)
                   # temp*type interaction is significant, so the effect on temperature on 
                      # avgDW depends on the type (makes sense)
# adjusted R^2 = 0.9451 

# using type 3 errors because 
Anova(all_int, type = "III")   # significant interactions = temp*type 
                               # means the first depends on the second 
# effect of temperature on avgDW depends on type
# temp is significant after controlling for treatment type and type 
# type has a significant effect on avgDW 


# model interpretation:
# am seeing how average NP changes with temperature and treatment type 
# I am allowing the relationship to vary with type (NP vs DR)
# intercept of the model = average starting NP
# each coefficient = how much change there is in NP with each temp step 
# adding random effect (1|type) = how much variation in avgDW there is in the initial (low T) 
  # NP between types (I'm allowing for variation by type)

# the model is just telling me if the trends seen are significant and what the trend is 
# I am looking at the difference in avgDW vs temp for control and treatment 
  # and controlling for variation in avgDW with type (DR and NP)
# HOWEVER, type only has 2 levels and it would be good to be able to make predictions about
  # differences between type = probably better not to have it as a random effect at all 
# including type as a random effect would be determining the relationship of temp and 
  # treatment type AFTER controlling for variation in type

## Creating mixed effects models 
mixed_type <- lmer(avgDW ~ temp + treatment_type + (1|type), data = fulldata, REML = F)
  # type as a random effect because DR and NP need to be controlled for, but I am not
  # interested in the direct relationship of it with avgDW 

# checking model assumptions 
plot(mixed_type)  
qqnorm(resid(mixed_type))
qqline(resid(mixed_type)) 

summary(mixed_type)     # temp: -0.1628
                        # treatment_type: 0.6213
confint(mixed_type, level = 0.95)     # temp: -0.2011 to -0.1355 
                                      # treatment_type: -0.007811 to 1.2503

anova(mixed_type, all_int)  # all_int is better at explaining the relationship


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



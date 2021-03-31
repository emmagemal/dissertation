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

## General linear models 
null_np <- lm(np_DW ~ 1, data = np_full)
temp_np <- lm(np_DW ~ temp, data = np_full)
temp_ttype_np <- lm(np_DW ~ temp + treatment_type, data = np_full)
temp_sample_np <- lm(np_DW ~ temp + sample, data = np_full)
temp_ttype_sample_np <- lm(np_DW ~ temp + treatment_type + sample, data = np_full)
temp_ttype_int_np <- lm(np_DW ~ temp*treatment_type, data = np_full)
temp_sample_int_np <- lm(np_DW ~ temp*sample, data = np_full)
temp_ttype_int_sample_np <- lm(np_DW ~ temp + treatment_type*sample, data = np_full)
temp_int_ttype_sample_np <- lm(np_DW ~ temp*treatment_type + sample, data = np_full)
all_int_np <- lm(np_DW ~ temp*treatment_type*sample, data = np_full)

AIC(null_np, temp_np, temp_ttype_np, temp_sample_np, temp_ttype_sample_np, temp_ttype_int_np, 
    temp_sample_int_np, temp_ttype_int_sample_np, temp_int_ttype_sample_np, all_int_np)
# all_int_np is the best model of these 

# checking model assumptions 
plot(all_int_np)   # outliers: row 14, 15, 18
shapiro.test(resid(all_int_np))   # p << 0.05, residuals are not normally distributed 
bptest(all_int_np)   #  p > 0.05 (just), no heteroskedasticity 

# removing outliers
np_full_edit <- np_full[-c(14, 15, 18), ]
all_int_np_edit <- lm(np_DW ~ temp*treatment_type*sample, data = np_full_edit)

plot(all_int_np_edit)
shapiro.test(resid(all_int_np_edit))  # residuals still not normally distributed 
bptest(all_int_np_edit)  # p >> 0.05, no heteroskasticity 

## Mixed effects models 
mixed_null_np <- lmer(np_DW ~ 1 + (1|sample), data = np_full, REML = F)
mixed_sample_np <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = np_full, REML = T)
  # sample as a random effect because sample needs to be controlled for, but I am not
  # interested in the direct relationship of it with np_DW 

mixed_int_sample_np <- lmer(np_DW ~ temp*treatment_type + (1|sample), data = np_full, REML = T)

#### to compare models you need REML = F, but to do an F-test rather than chi squared test
  # you need to REML = T 
anova(mixed_null_np, mixed_sample_np)  # mixed_sample_np is better than the null model
anova(mixed_null_np, mixed_int_sample_np)  # mixed_int_sample_np is better than null model
anova(mixed_sample_np, mixed_int_sample_np)  # interaction is NOT significantly better

## Comparing mixed and general linear models 
anova(mixed_sample_np, all_int_np)  # all_int_np is better at explaining the relationship

summary(all_int_np_edit)  # adjusted R^2 = 0.7594
# idk what it means exactly, but the interactions between temp and T1-T6 and C6 = significant
# so significant interaction terms, but the samples in themselves do not significantly 
  # impact np_DW 
anova(all_int_np_edit)  # all are significant

summary(mixed_sample_np)  # sample quite a bit of the excess variation 
Anova(mixed_sample_np, type = "III", test = "F")  # both temp and treatment_type significant effect NP
# type 3 = how much variability in NP can be attributed to be temp after accounting 
  # for everything else. then how much can be attributed to treatment after accounting for 
  # everything else, etc. 
# type 1 would be how much is attributed to temp, then how much of the leftover variability
  # is explained by treatment, then how much that's left is explained by the rest, etc. 
  # (order matters for type 1)


### Models for DR ----
## General linear models 
null_dr <- lm(np_DW ~ 1, data = dr_full)
temp_dr <- lm(np_DW ~ temp, data = dr_full)
temp_ttype_dr <- lm(np_DW ~ temp + treatment_type, data = dr_full)
temp_sample_dr <- lm(np_DW ~ temp + sample, data = dr_full)
temp_ttype_sample_dr <- lm(np_DW ~ temp + treatment_type + sample, data = dr_full)
temp_ttype_int_dr <- lm(np_DW ~ temp*treatment_type, data = dr_full)
temp_sample_int_dr <- lm(np_DW ~ temp*sample, data = dr_full)
temp_ttype_int_sample_dr <- lm(np_DW ~ temp + treatment_type*sample, data = dr_full)
temp_int_ttype_sample_dr <- lm(np_DW ~ temp*treatment_type + sample, data = dr_full)
all_int_dr <- lm(np_DW ~ temp*treatment_type*sample, data = dr_full)

AIC(null_dr, temp_dr, temp_ttype_dr, temp_sample_dr, temp_ttype_sample_dr, temp_ttype_int_dr, 
    temp_sample_int_dr, temp_ttype_int_sample_dr, temp_int_ttype_sample_dr, all_int_dr)
# all_int_dr and temp_sample_int_dr have the same AIC value and same degrees of freedom
# use temp_sample_int_dr 

## Mixed effect models 
mixed_null_dr <- lmer(np_DW ~ 1 + (1|sample), data = dr_full, REML = F)
mixed_sample_dr <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = dr_full, REML = T)
mixed_int_sample_dr <- lmer(np_DW ~ temp*treatment_type + (1|sample), 
                            data = dr_full, REML = F)

anova(mixed_null_dr, mixed_sample_dr)  # mixed_sample_dr is better than the null model
anova(mixed_null_dr, mixed_int_sample_dr)  # mixed_int_sample_dr is better than null model
anova(mixed_sample_dr, mixed_int_sample_dr)  # interaction is NOT significantly better

## Comparing mixed and general linear models 
anova(mixed_sample_dr, all_int_dr)  # all_int_dr is better at explaining the relationship

summary(all_int_dr)  # adjusted R^2 = 0.9591
# only temp and a few temp*sample interactions (2 control, 3 treatment) are significant 
anova(all_int_dr)  # treatment_type and temp*treatment_type are NOT significant 
# sample and temp*sample are significant (between sample variation is more important than
  # treatment type)
# the significant interaction (temp*sample) = the response of np_DW to temp depends on sample

summary(mixed_sample_dr)  # sample explains most of the variance 
# std. error is > treatment estimate 
Anova(mixed_sample_dr, type = "III", test = "F")  # treatment_type is not significant 
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
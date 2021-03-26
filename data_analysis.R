# Data Analysis and Calculations
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

### Library ----
library(tidyverse)
library(retistruct)
library(lmtest)
library(lme4)

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


# creating some linear models from initial data
null <- lm(avgDW ~ 1, data = fulldata)
temp_ttype <- lm(avgDW ~ temp + treatment_type, data = fulldata)
temp_ttype_type <- lm(avgDW ~ temp + treatment_type + type, data = fulldata)
temp_ttype_int <- lm(avgDW ~ temp*treatment_type, data = fulldata)
temp_ttype_type_int <- lm(avgDW ~ temp + treatment_type*type, data = fulldata)
temp_ttype_int_type <- lm(avgDW ~ temp*treatment_type + type, data = fulldata)
all_int <- lm(avgDW ~ temp*treatment_type*type, data = fulldata)

AIC(null, temp_ttype, temp_ttype_type, temp_ttype_int, temp_ttype_type_int, 
    temp_ttype_int_type, all_int)
# all_int is the best model of these 

# checking model assumptions
anova(all_int)   # some significant interaction terms = violates assumptions, can't use 

hist(residuals(all_int))   
shapiro.test(residuals(all_int))  # residuals are normally distributed 

plot(all_int)    # non-linear relationship present 
bptest(all_int)  # no heteroskedasticity in the model 


# running basic ANCOVAs on NP and DR separately 
np_m <- lm(avgDW ~ temp + treatment_type, data = np_only)
anova(np_m)  # significant difference between control and treatment!

dr_m <- lm(avgDW ~ temp + treatment_type, data = dr_only)
anova(dr_m)  # no difference between control and treatment = good for DR!! acclimation!!


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
mixed_type <- lmer(avgDW ~ temp + treatment_type + (1|type), data = fulldata)
  # type as a random effect because DR and NP need to be controlled for, but I am not
  # interested in the direct relationship of it with avgDW 

summary(mixed_type)     # temp: -0.1628
                        # treatment_type: 0.6213
confint(mixed_type, level = 0.95)     # temp: -0.2011 to -0.1355 
                                      # treatment_type: -0.007811 to 1.2503
plot(mixed_type)  # checking model assumption 
qqnorm(resid(mixed_type))
qqline(resid(mixed_type)) 

## Attempting to add other variables 
cgain <- read.csv("Data/c_gain_long.csv")

# combining full data and carbon gain efficiency data
combo <- full_join(fulldata, cgain)
combo <- combo %>% 
          rename(cgain_perc = percent) %>% 
          rename(cgain_ratio = ratio)

combo_m <- lmer(avgDW ~ temp + treatment_type + cgain_ratio + (1|type), data = combo)
# I don't really see the point in adding it as a variable? Like what does it add???? 

AIC(mixed_type, combo_m)  # combo_m is better 
plot(combo_m)  # checking model assumption 
qqnorm(resid(combo_m))
qqline(resid(combo_m)) 

summary(combo_m)    # temp: -0.1683
                    # treatment_type: 0.6213
                    # cgain_ratio: -1.5668
confint(combo_m, level = 0.95)     # temp: -0.1965 to -0.1400
                                   # treatment_type: 0.07927 to 1.1632
                                   # cgain_ratio: -2.6337 to -0.5288

# combo_m = parameters are all significant (lay within the CI's)
  # temp has significant effect on avgDW, there's significant difference in avgDW between 
  # treatments and there's significant relationship between c gain ratios and avgDW?? I think

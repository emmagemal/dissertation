# Data Analysis and Calculations
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh 

### Library ----
library(tidyverse)
library(retistruct)
library(lmerTest)
library(lme4)
library(car)
library(sjPlot)
library(MuMIn)

### NP/DR Data Manipulation ----
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

# subsetting full NP and DR for models and calculations  
np_full <- fulldata %>% 
              filter(type == "NP")
dr_full <- fulldata %>% 
              filter(type == "DR")

np_full_control <- np_full %>% 
                      filter(treatment_type == "control")
np_full_treatment <- np_full %>% 
                        filter(treatment_type == "treatment")

### Calculating Average Optimum Temperature Ranges ----
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

### Optimum Temperature Range Statistics ----
## Control samples optimum temperature calculations 
# maximum net photosynthesis for each sample 
summary(np_full_control$np_DW[np_full_control$sample == "C1"])  # max C1 = 2.150
summary(np_full_control$np_DW[np_full_control$sample == "C2"])  # max C2 = 0.452
summary(np_full_control$np_DW[np_full_control$sample == "C3"])  # max C3 = 2.073
summary(np_full_control$np_DW[np_full_control$sample == "C4"])  # max C4 = 3.095
summary(np_full_control$np_DW[np_full_control$sample == "C5"])  # max C5 = 0.914
summary(np_full_control$np_DW[np_full_control$sample == "C6"])  # max C6 = 1.530

# calculating 90% of the maximum net photosynthesis 
0.9*2.150  # C1 = 1.935
0.9*0.452  # C2 = 0.407
0.9*2.073  # C3 = 1.866
0.9*3.095  # C4 = 2.786
0.9*0.914  # C5 = 0.823
0.9*1.530  # C6 = 1.377

# visualizing the intersection points 
hline_c <- data.frame(z = c(1.935, 0.407, 1.866, 2.786, 0.823, 1.377), 
                      sample = factor(c("C1", "C2", "C3", "C4", "C5", "C6")))

(np_control_plot <- ggplot(np_full_control, aes(x = temp, y = np_DW)) +
                      geom_point(aes(color = sample), 
                                 size = 2, alpha = 0.85) +
                      geom_line(aes(color = sample)) +
                      facet_wrap(~sample) +
                      geom_hline(data = hline_c, aes(yintercept = z)))

## Determining the control intersection points
# C1 intersection points
c1a <- c(10, 1.40250649)
c1b <- c(15, 2.15039655)
c1c <- c(20, 0.87415111)
control_y_c1 <- c(5, 1.935)
control_y2_c1 <- c(25, 1.935)

line.line.intersection(c1a, c1b, control_y_c1, control_y2_c1, 
                       interior.only = FALSE)               # x = 13.560˚C
line.line.intersection(c1b, c1c, control_y_c1, control_y2_c1, 
                       interior.only = FALSE)               # x = 15.844˚C

# C2 intersection points
c2a <- c(2, 0.45202245)
c2b <- c(5, 0.30479208)
control_y_c2 <- c(0, 0.407)
control_y2_c2 <- c(10, 0.407)

line.line.intersection(c2a, c2b, control_y_c2, control_y2_c2, 
                       interior.only = FALSE)               # x = 2.917˚C

# C3 intersection points
c3a <- c(2, 2.07331979)
c3b <- c(5, 1.37018400)
control_y_c3 <- c(0, 1.866)
control_y2_c3 <- c(10, 1.866)

line.line.intersection(c3a, c3b, control_y_c3, control_y2_c3, 
                       interior.only = FALSE)               # x = 2.885˚C

# C4 intersection points
c4a <- c(5, 1.85969117)
c4b <- c(10, 3.09514203)
c4c <- c(15, 1.76274162)
control_y_c4 <- c(2, 2.786)
control_y2_c4 <- c(20, 2.786)

line.line.intersection(c4a, c4b, control_y_c4, control_y2_c4, 
                       interior.only = FALSE)               # x = 8.749˚C
line.line.intersection(c4b, c4c, control_y_c4, control_y2_c4, 
                       interior.only = FALSE)               # x = 11.160˚C

# C5 intersection points
c5a <- c(5, 0.15343868)
c5b <- c(10, 0.91347719)
c5c <- c(15, -1.71686016)
control_y_c5 <- c(2, 0.823)
control_y2_c5 <- c(20, 0.823)

line.line.intersection(c5a, c5b, control_y_c5, control_y2_c5, 
                       interior.only = FALSE)               # x = 9.405˚C
line.line.intersection(c5b, c5c, control_y_c5, control_y2_c5, 
                       interior.only = FALSE)               # x = 10.172˚C

# C6 intersection points
c6a <- c(5, 1.03596630)
c6b <- c(10, 1.52977948)
c6c <- c(15, -1.46892583)
control_y_c6 <- c(2, 1.377)
control_y2_c6 <- c(20, 1.377)

line.line.intersection(c6a, c6b, control_y_c6, control_y2_c6, 
                       interior.only = FALSE)               # x = 8.453˚C
line.line.intersection(c6b, c6c, control_y_c6, control_y2_c6, 
                       interior.only = FALSE)               # x = 10.255˚C

## Treatment samples optimum temperature calculations
# maximum net photosynthesis for each sample 
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T1"])  # max T1 = 2.0939
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T2"])  # max T2 = 2.483
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T3"])  # max T3 = 4.573
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T4"])  # max T4 = 0.998
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T5"])  # max T5 = 4.432
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T6"])  # max T6 = 4.938

# calculating 90% of the maximum net photosynthesis 
0.9*2.0939  # T1 = 1.885
0.9*2.483  # T2 = 2.235
0.9*4.573  # T3 = 4.116
0.9*0.998  # T4 = 0.898
0.9*4.432  # T5 = 3.989
0.9*4.938  # T6 = 4.444

# visualizing the intersection points 
hline_t <- data.frame(z = c(1.885, 2.235, 4.116, 0.898, 3.989, 4.444), 
                      sample = factor(c("T1", "T2", "T3", "T4", "T5", "T6")))

(np_treatment_plot <- ggplot(np_full_treatment, aes(x = temp, y = np_DW)) +
                        geom_point(aes(color = sample), 
                                   size = 2, alpha = 0.85) +
                        geom_line(aes(color = sample)) +
                        facet_wrap(~sample) +
                        geom_hline(data = hline_t, aes(yintercept = z)))

## Determining the treatment intersection points
# T1 intersection points
t1a <- c(2, 2.09390796)
t1b <- c(5, 1.53332246)
treatment_y_t1 <- c(0, 1.885)
treatment_y2_t1 <- c(10, 1.885)

line.line.intersection(t1a, t1b, treatment_y_t1, treatment_y2_t1, 
                       interior.only = FALSE)               # x = 3.118˚C

# T2 intersection points
t2a <- c(10, 0.63100582)
t2b <- c(15, 2.48289801)
t2c <- c(20, 2.19181633)
treatment_y_t2 <- c(5, 2.235)
treatment_y2_t2 <- c(25, 2.235)

line.line.intersection(t2a, t2b, treatment_y_t2, treatment_y2_t2, 
                       interior.only = FALSE)               # x = 14.331˚C
line.line.intersection(t2b, t2c, treatment_y_t2, treatment_y2_t2, 
                       interior.only = FALSE)               # x = 19.258˚C

# T3 intersection points
t3a <- c(10, 2.96954361)
t3b <- c(15, 4.57265541)
t3c <- c(20, 1.38623779)
treatment_y_t3 <- c(5, 4.116)
treatment_y2_t3 <- c(25, 4.116)

line.line.intersection(t3a, t3b, treatment_y_t3, treatment_y2_t3, 
                       interior.only = FALSE)               # x = 13.576˚C
line.line.intersection(t3b, t3c, treatment_y_t3, treatment_y2_t3, 
                       interior.only = FALSE)               # x = 15.717˚C

# T4 intersection points
t4a <- c(5, 0.69462551)
t4b <- c(10, 0.99809032)
t4c <- c(15, 0.68706063)
treatment_y_t4 <- c(2, 0.898)
treatment_y2_t4 <- c(20, 0.898)

line.line.intersection(t4a, t4b, treatment_y_t4, treatment_y2_t4, 
                       interior.only = FALSE)               # x = 8.351˚C
line.line.intersection(t4b, t4c, treatment_y_t4, treatment_y2_t4, 
                       interior.only = FALSE)               # x = 11.609˚C

# T5 intersection points
t5a <- c(5, 0.27651295)
t5b <- c(10, 4.43202224)
t5c <- c(15, 1.99593504)
treatment_y_t5 <- c(2, 3.989)
treatment_y2_t5 <- c(20, 3.989)

line.line.intersection(t5a, t5b, treatment_y_t5, treatment_y2_t5, 
                       interior.only = FALSE)               # x = 9.467˚C
line.line.intersection(t5b, t5c, treatment_y_t5, treatment_y2_t5, 
                       interior.only = FALSE)               # x = 10.909˚C

# T6 intersection points
t6a <- c(5, 0.31124528)
t6b <- c(10, 4.93776185)
t6c <- c(15, 3.77278049)
treatment_y_t6 <- c(2, 4.444)
treatment_y2_t6 <- c(20, 4.444)

line.line.intersection(t6a, t6b, treatment_y_t6, treatment_y2_t6, 
                       interior.only = FALSE)               # x = 9.466˚C
line.line.intersection(t6b, t6c, treatment_y_t6, treatment_y2_t6, 
                       interior.only = FALSE)               # x = 12.119˚C


## Creating a combined dataframe for analysis 
opt_temp_max <- c(15.844, 2.917, 2.885, 11.160, 10.172, 10.255,
                  3.118, 19.258, 15.717, 11.609, 10.909, 12.119)
opt_temp_min <- c(13.560, 2.917, 2.885, 8.749, 9.405, 8.453, 
                  3.118, 14.331, 13.576, 8.351, 9.467, 9.466)
sample_max <- c("C1", "C2", "C3", "C4", "C5", "C6", "T1", "T2", "T3", "T4", "T5", "T6")
sample_min <- c("C1", "C2", "C3", "C4", "C5", "C6", "T1", "T2", "T3", "T4", "T5", "T6")

opt_stats <- data.frame(opt_temp_max, opt_temp_min, sample_max, sample_min)
str(opt_stats)

opt_stats <- opt_stats %>% 
                mutate(treatment_type = case_when(grepl("C", sample_max) ~ "control",
                                                  grepl("T", sample_max) ~ "treatment"))

## Testing significance of optimum temperature ranges 
t.test(opt_temp_max ~ treatment_type, data = opt_stats)
t.test(opt_temp_min ~ treatment_type, data = opt_stats)


### Negative NP Statistics ----
## Determining the control intersection points
# visualizing the intersection points
(negnp_control_plot <- ggplot(np_full_control, aes(x = temp, y = np_DW)) +
                         geom_point(aes(color = sample), 
                                    size = 2, alpha = 0.85) +
                         geom_line(aes(color = sample)) +
                         facet_wrap(~sample) +
                         geom_hline(yintercept = 0))

# C1 intersection points
c1aneg <- c(25, 0.03516769)
c1bneg <- c(30, -1.04820980)
control_y_c1neg <- c(20, 0)
control_y2_c1neg <- c(30, 0)

line.line.intersection(c1aneg, c1bneg, control_y_c1neg, control_y2_c1neg, 
                       interior.only = FALSE)               # x = 25.162˚C

# C2 intersection points
c2aneg <- c(15, 0.29194808)
c2bneg <- c(20, -1.19222116)
control_y_c2neg <- c(10, 0)
control_y2_c2neg <- c(25, 0)

line.line.intersection(c2aneg, c2bneg, control_y_c2neg, control_y2_c2neg, 
                       interior.only = FALSE)               # x = 15.984˚C

# C3 intersection points
c3aneg <- c(15, 1.90861965)
c3bneg <- c(20, -1.54370263)
control_y_c3neg <- c(15, 0)
control_y2_c3neg <- c(20, 0)

line.line.intersection(c3aneg, c3bneg, control_y_c3neg, control_y2_c3neg, 
                       interior.only = FALSE)               # x = 17.764˚C

# C4 intersection points
c4aneg <- c(20, 1.81622713)
c4bneg <- c(25, -0.19804641)
control_y_c4neg <- c(20, 0)
control_y2_c4neg <- c(30, 0)

line.line.intersection(c4aneg, c4bneg, control_y_c4neg, control_y2_c4neg, 
                       interior.only = FALSE)               # x = 24.508˚C

# C5 intersection points
c5aneg <- c(10, 0.91347719)
c5bneg <- c(15, -1.71686016)
control_y_c5neg <- c(10, 0)
control_y2_c5neg <- c(20, 0)

line.line.intersection(c5aneg, c5bneg, control_y_c5neg, control_y2_c5neg, 
                       interior.only = FALSE)               # x = 11.736˚C
# C6 intersection points
c6aneg <- c(10, 1.52977948)
c6bneg <- c(15, -1.46892583)
control_y_c6neg <- c(10, 0)
control_y2_c6neg <- c(15, 0)

line.line.intersection(c6aneg, c6bneg, control_y_c6neg, control_y2_c6neg, 
                       interior.only = FALSE)               # x = 12.551˚C

## Determining the treatment intersection points
# visualizing the intersection points
(negnp_treatment_plot <- ggplot(np_full_treatment, aes(x = temp, y = np_DW)) +
                            geom_point(aes(color = sample), 
                                       size = 2, alpha = 0.85) +
                            geom_line(aes(color = sample)) +
                            facet_wrap(~sample) +
                            geom_hline(yintercept = 0))

# T1 intersection points
t1aneg <- c(15, 1.81651836)
t1bneg <- c(20, -0.40554268)
treatment_y_t1neg <- c(15, 0)
treatment_y2_t1neg <- c(25, 0)

line.line.intersection(t1aneg, t1bneg, treatment_y_t1neg, treatment_y2_t1neg, 
                       interior.only = FALSE)               # x = 19.087˚C

# T2 intersection points
t2aneg <- c(20, 2.19181633)
t2bneg <- c(25, -0.48649322)
treatment_y_t2neg <- c(20, 0)
treatment_y2_t2neg <- c(30, 0)

line.line.intersection(t2aneg, t2bneg, treatment_y_t2neg, treatment_y2_t2neg, 
                       interior.only = FALSE)               # x = 24.092˚C

# T3 intersection points
t3aneg <- c(25, 1.02732157)
t3bneg <- c(30, -0.53525911)
treatment_y_t3neg <- c(20, 0)
treatment_y2_t3neg <- c(30, 0)

line.line.intersection(t3aneg, t3bneg, treatment_y_t3neg, treatment_y2_t3neg, 
                       interior.only = FALSE)               # x = 28.287˚C

## Creating a combined dataframe for analysis 
negNP <- c(25.162, 15.984, 17.764, 24.508, 11.736, 12.551, 19.087, 24.092, 28.287)

sample <- c("C1", "C2", "C3", "C4", "C5", "C6", "T1", "T2", "T3")

negNP_stats <- data.frame(negNP, sample)
str(negNP_stats)

negNP_stats <- negNP_stats %>% 
                  mutate(treatment_type = case_when(grepl("C", sample) ~ "control",
                                                    grepl("T", sample) ~ "treatment"))

## Testing significance of optimum temperature ranges 
t.test(negNP ~ treatment_type, data = negNP_stats)

### Maximum NP Statistics ----
# maximum net photosynthesis for each control sample 
summary(np_full_control$np_DW[np_full_control$sample == "C1"])  # max C1 = 2.150
summary(np_full_control$np_DW[np_full_control$sample == "C2"])  # max C2 = 0.452
summary(np_full_control$np_DW[np_full_control$sample == "C3"])  # max C3 = 2.073
summary(np_full_control$np_DW[np_full_control$sample == "C4"])  # max C4 = 3.095
summary(np_full_control$np_DW[np_full_control$sample == "C5"])  # max C5 = 0.914
summary(np_full_control$np_DW[np_full_control$sample == "C6"])  # max C6 = 1.530

# maximum net photosynthesis for each treatment sample 
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T1"])  # max T1 = 2.0939
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T2"])  # max T2 = 2.483
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T3"])  # max T3 = 4.573
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T4"])  # max T4 = 0.998
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T5"])  # max T5 = 4.432
summary(np_full_treatment$np_DW[np_full_treatment$sample == "T6"])  # max T6 = 4.938

maxNP <- c(2.150, 0.452, 2.073, 3.095, 0.914, 1.530, 2.0939, 2.483, 4.573, 0.998, 4.432, 4.938)
sample <- c("C1", "C2", "C3", "C4", "C5", "C6", "T1", "T2", "T3", "T4", "T5", "T6")

maxNP_stats <- data.frame(maxNP, sample)
str(maxNP_stats)

maxNP_stats <- maxNP_stats %>% 
                  mutate(treatment_type = case_when(grepl("C", sample) ~ "control",
                                                    grepl("T", sample) ~ "treatment"))

## Testing significance of maximum NP rates 
t.test(maxNP ~ treatment_type, data = maxNP_stats)

### Models for NP ----
## Checking data assumptions  
# normally distributed response variable 
(hist <- ggplot(np_full, aes(x = np_DW)) +
           geom_histogram(color = "black") +
           theme_classic() +
           scale_y_continuous(expand = c(0,0)))
shapiro.test(np_full$np_DW)   # is normally distributed, p > 0.05

# homogeneity of regression slopes 
anova(lm(np_DW ~ temp*treatment_type, data = np_full))  # interaction not significant

## Models
lm_treat_temp <- lm(np_DW ~ temp + treatment_type, data = np_full)
mixed_null_np <- lmer(np_DW ~ 1 + (1|sample), data = np_full, REML = F)
mixed_notemp <- lmer(np_DW ~ treatment_type + (1|sample), data = np_full, REML = F)
mixed_sample_np <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = np_full, REML = F)
# sample as a random effect because sample needs to be controlled for, but I am not
# interested in the direct relationship of it with np_DW 

# comparing the models 
AIC(mixed_null_np, mixed_notemp, mixed_sample_np)  # mixed: lowest AIC = mixed_sample_np

anova(mixed_null_np, mixed_sample_np)  # mixed_sample_np is better than the null model
anova(mixed_sample_np, lm_treat_temp)  # accounting for sample variation is best (mixed_sample_np)

# checking model assumptions 
plot(mixed_sample_np)

par(mfrow = c(1,2))
qqnorm(ranef(mixed_sample_np)$sample[, 1], main = "Random effects of sample")
qqnorm(resid(mixed_sample_np), main = "Residuals")

# model outputs
confint(mixed_sample_np)  # temp: -0.146 to -0.0817
                          # treatment_type: 0.253 to 2.665

summary(mixed_sample_np)  # sample explains quite a bit of the residual variance 
                          # temp: -0.114 ± 0.0163 (within the confidence intervals)
                          # treatment_type: 1.457 ± 0.621 (within the confidence intervals)
# treatment_type and temp significantly effect NP
# they're significantly different from 0 according to the CI's
0.9157/(0.9157+1.4169)    # sample explains 39.3% of the residual variation 

anova(mixed_sample_np)   # temp: F = 49.05, p = 2.71e-9
                         # treatment_type: F = 5.51, p = 0.041

### Models for DR ----
## Checking data assumptions  
# normally distributed response variable 
(hist <- ggplot(dr_full, aes(x = np_DW)) +
           geom_histogram(color = "black") +
           theme_classic() +
           scale_y_continuous(expand = c(0,0)))
shapiro.test(dr_full$np_DW)   # is NOT normally distributed, p << 0.05

# homogeneity of regression slopes 
anova(lm(np_DW ~ temp*treatment_type, data = dr_full))  # interaction not significant

## Models
lm_treat_temp_dr <- lm(np_DW ~ temp + treatment_type, data = dr_full)
mixed_null_dr <- lmer(np_DW ~ 1 + (1|sample), data = dr_full, REML = F)
mixed_notemp_dr <- lmer(np_DW ~ treatment_type + (1|sample), data = dr_full, REML = F)
mixed_sample_dr <- lmer(np_DW ~ temp + treatment_type + (1|sample), data = dr_full, REML = F)

# comparing the models 
AIC(mixed_null_dr, mixed_notemp_dr, mixed_sample_dr)  # mixed_sample_dr = lowest AIC

anova(mixed_sample_dr, mixed_null_dr)  # mixed_sample_dr is better than the null model 
anova(mixed_sample_dr, lm_treat_temp_dr)  # better to include the random effect 

# checking model assumptions 
plot(mixed_sample_dr)   # potential violation of linearity 

par(mfrow = c(1,2))
qqnorm(ranef(mixed_sample_dr)$sample[, 1], main = "Random effects of sample")
qqnorm(resid(mixed_sample_dr), main = "Residuals")

# model outputs 
confint(mixed_sample_dr)  # temp: -0.243 to -0.192 
                          # treatment_type: -1.492 to 1.425 

summary(mixed_sample_dr)  # sample explains a lot of the residual variance 
                          # temp: -0.217 ± 0.0130 
                          # treatment_type: -0.0338 ± 0.751
# treatment std. error is >> treatment estimate 
  # implies treatment_type does not have a large effect on DR 
# temp is significant, treatment_type is not 
1.5371/(1.5371+0.9043)  # sample explains 63.0% of the residual variation

anova(mixed_sample_dr)   # temp: F = 280.40, p = <2e-16
                         # treatment_type: F = 0.0020, p = 0.965 
# treatment_type is not significant, temp is significant 

### Average Light Response Curve Calculations ----
light <- read.csv("Data/full_light_responses.csv")

light_avg <- light %>% 
                group_by(Lcuv, treatment_type) %>% 
                summarise(avgCO2 = mean(CO2)) %>% 
                filter(Lcuv <= 1000)

summary(light_avg$avgCO2[light_avg$treatment_type == "control"])  # max control = 2.883
summary(light_avg$avgCO2[light_avg$treatment_type == "treatment"])  # max treatment = 14.423 

0.9*2.883  # 90% max control = 2.595
0.9*14.423  # 90% max treatment = 12.981

## Determining the intersection points for 90% LSP
# visualizing the 90% light saturation point 
(light_plot <- ggplot(light_avg, aes(x = Lcuv, y = avgCO2)) +
                  geom_point(aes(color = treatment_type)) +
                  geom_line(aes(color = treatment_type)) +
                  facet_wrap(~treatment_type))

hline_light <- data.frame(z = c(2.595, 12.981), treatment_type = factor(c("control", "treatment")))

(lightsat_plot <- light_plot + 
                    geom_hline(data = hline_light, aes(yintercept = z)))

# control intersection point
c1_l <- c(500, 1.069)
c2_l <- c(1000, 2.883)
control_y_l <- c(500, 2.595)
control_y2_l <- c(1000, 2.595)

line.line.intersection(c1_l, c2_l, control_y_l, control_y2_l, 
                       interior.only = FALSE)               # x = 920.6 µE m^-2 s^-1

# treatment intersection point
t1_l <- c(200, 9.200)
t2_l <- c(500, 14.423)
treatment_y_l <- c(200, 12.981)
treatment_y2_l <- c(500, 12.981)

line.line.intersection(t1_l, t2_l, treatment_y_l, treatment_y2_l, 
                       interior.only = FALSE)               # x = 417.2 µE m^-2 s^-1

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


### Light Response Curve LSP Statistics ----
light_sum <- light %>% 
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
(light_plot_facet <- ggplot(light, aes(x = Lcuv, y = CO2)) +
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


### Light Response Curve LCP Statistics ----
# visualizing the light compensation point for all samples
(lightcomp_plot_facet <- ggplot(light, aes(x = Lcuv, y = CO2)) +
                           geom_point(aes(color = treatment_type)) +
                           geom_line(aes(color = treatment_type)) +
                           facet_wrap(~sample) +
                           geom_hline(yintercept = 0))

## Determining the intersection points for LCP
# C1 and T1 have no light compensation point (are never >0)
light_sum_lcp <- light_sum %>% 
                    filter(sample == "C2" |
                           sample == "C3" | 
                           sample == "T2" |
                           sample == "T3")

# C2 intersection point
c2_1b <- c(100, -1.25)
c2_2b <- c(200, 1.39)
control_y_c2b <- c(100, 0)
control_y2_c2b <- c(200, 0)

line.line.intersection(c2_1b, c2_2b, control_y_c2b, control_y2_c2b, 
                       interior.only = FALSE)               # x = 147.3 µE m^-2 s^-1

# C3 intersection point
c3_1b <- c(50, -1.18)
c3_2b <- c(100, 1.60)
control_y_c3b <- c(500, 0)
control_y2_c3b <- c(100, 0)

line.line.intersection(c3_1b, c3_2b, control_y_c3b, control_y2_c3b, 
                       interior.only = FALSE)               # x = 71.2 µE m^-2 s^-1

# T2 intersection point
t2_1b <- c(12, -2.27)
t2_2b <- c(25, 0.56)
treatment_y_t2b <- c(10, 0)
treatment_y2_t2b <- c(50, 0)

line.line.intersection(t2_1b, t2_2b, treatment_y_t2b, treatment_y2_t2b, 
                       interior.only = FALSE)               # x = 22.4 µE m^-2 s^-1

# T3 intersection point
t3_1b <- c(25, -0.90)
t3_2b <- c(50, 6.95)
treatment_y_t3b <- c(20, 0)
treatment_y2_t3b <- c(60, 0)

line.line.intersection(t3_1b, t3_2b, treatment_y_t3b, treatment_y2_t3b, 
                       interior.only = FALSE)               # x = 27.9 µE m^-2 s^-1

light_sum_lcp$LCPcuv <- c(147.3, 71.2, 22.4, 27.9)

## T-Test for LCP
t.test(LCPcuv ~ treatment_type, data = light_sum_lcp)  # p = 0.27 (NOT significantly different)
# using the means of the calculated LCPcuv, control = 109.25 and treatment = 25.15 
# (different to pre-averaged data, a lot lower)
### Optimal Water Content Ranges ----
# potentially change the heading name



### Models for Climate (Temperature) ----
climate <- read.csv("Data/climate_combo.csv")

## Simple linear models 
temp_lm <- lm(temp ~ I(year-2004), data = climate)
summary(temp_lm)  # p = 0.749, not at all significant
anova(temp_m)

# checking model assumptions 
plot(temp_m)  # not very normally distributed 
hist(resid(temp_m))  # skewed, violates model assumptions 
bptest(temp_m)  # there is heteroskedasticity in the model, violates assumptions

# creating a null model to compare it to
temp_null <- lm(temp ~ 1, data = climate)
AIC(temp_m, temp_null)  # null model is better 

## Mixed effect models
temp_yr_m <- lmer(temp ~ year + (1|year), data = climate, REML = F)
temp_day_m <- lmer(temp ~ year + (1|day), data = climate, REML = F)
temp_date_m <- lmer(temp ~ year + (1|date_time), data = climate, REML = F)
temp_season_m <- lmer(temp ~ year + (1|season), data = climate, REML = F)
temp_month_yr_m

AIC(temp_yr_m, temp_day_m, temp_date_m, temp_season_m)
# day as random effect is best, season is 2nd best 

temp_month_season <- lmer(temp ~ year + (1|month) + (1|season), data = climate, REML = F)
temp_month_date <- lmer(temp ~ year + (1|month) + (1|date_time), data = climate, REML = F)

anova(temp_month_mixed, temp_month_season)  # month_season is best
anova(temp_month_mixed, temp_month_date)    # month_date is better
AIC(temp_month_season, temp_month_date)     # month_season is best  

# creating null models to compare it with
temp_m_null <- lmer(temp ~ 1 + (1|month) + (1|season), data = climate, REML = F)

# anova states which model is better at capturing the data
anova(temp_m_null, temp_month_season) 
# model with year as a fixed effect is better than the null models   

anova(temp_month_season, temp_m)  # temp_month_season is best 

# results 
summary(temp_month_season)   # year: -0.2165 (std error: 0.06413), small effect size 
confint(temp_month_season)   # year: -0.3463 to -0.08523 
# 95% confident the correlation between year and temperature is between those 2 values 

# visualizing the effects 
plot(temp_month_season)
(re_effects <- plot_model(temp_month_season, type = "re", show.values = TRUE)) 
# deviation for each random effect group from the model intercept estimates 
# lots of deviation between months (no overlap of CI's) and seasons (some overlap across seasons)
(fe_effects <- plot_model(temp_month_season, show.values = TRUE))
# -0.22, somewhat wide confidence interval (some uncertainty)

# calculating pseudo-R2 for mixed effects model 
r.squaredGLMM(temp_month_season)  # marginal R^2 associated with year (fixed effect) = 0.0238
# conditional R^2 associated with fixed + random = 0.635
# r2_nakagawa(temp_month_season)   # 'performance' - same output: R2(c) = 0.635, R2(m) = 0.024

# comparing to the null model (only random effects)
summary(temp_m_null)
r.squaredGLMM(temp_m_null)


## OLD Mixed effects models 
# if std error > estimate = year doesn't explain much of the variation
# can calculate % of leftover variation that's explained by the random effects 
# estimate for year = after controlling for the random effects 

# month = only 3 of them, not great as a random effect 

temp_mixed <- lmer(temp ~ year + (1|year), data = climate, REML = F)
temp_month_mixed <- lmer(temp ~ year + (1|month), data = climate, REML = F)
temp_day_mixed <- lmer(temp ~ year + (1|day), data = climate, REML = F)
temp_date_mixed <- lmer(temp ~ year + (1|date_time), data = climate, REML = F)
temp_season_mixed <- lmer(temp ~ year + (1|season), data = climate, REML = F)

AIC(temp_mixed, temp_month_mixed, temp_day_mixed, temp_date_mixed, temp_season_mixed)
# month as random effect is best

# models that failed to converge: (1|month) + (1|day), (year|month), (month|day)
temp_month_season <- lmer(temp ~ year + (1|month) + (1|season), data = climate, REML = F)
temp_month_date <- lmer(temp ~ year + (1|month) + (1|date_time), data = climate, REML = F)

anova(temp_month_mixed, temp_month_season)  # month_season is best
anova(temp_month_mixed, temp_month_date)    # month_date is better
AIC(temp_month_season, temp_month_date)     # month_season is best  

# creating null models to compare it with
temp_m_null <- lmer(temp ~ 1 + (1|month) + (1|season), data = climate, REML = F)

# anova states which model is better at capturing the data
anova(temp_m_null, temp_month_season) 
# model with year as a fixed effect is better than the null models   

anova(temp_month_season, temp_m)  # temp_month_season is best 

# results 
summary(temp_month_season)   # year: -0.2165 (std error: 0.06413), small effect size 
confint(temp_month_season)   # year: -0.3463 to -0.08523 
# 95% confident the correlation between year and temperature is between those 2 values 

# visualizing the effects 
plot(temp_month_season)
(re_effects <- plot_model(temp_month_season, type = "re", show.values = TRUE)) 
# deviation for each random effect group from the model intercept estimates 
# lots of deviation between months (no overlap of CI's) and seasons (some overlap across seasons)
(fe_effects <- plot_model(temp_month_season, show.values = TRUE))
# -0.22, somewhat wide confidence interval (some uncertainty)

# calculating pseudo-R2 for mixed effects model 
r.squaredGLMM(temp_month_season)  # marginal R^2 associated with year (fixed effect) = 0.0238
# conditional R^2 associated with fixed + random = 0.635
# r2_nakagawa(temp_month_season)   # 'performance' - same output: R2(c) = 0.635, R2(m) = 0.024

# comparing to the null model (only random effects)
summary(temp_m_null)
r.squaredGLMM(temp_m_null)
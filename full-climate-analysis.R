# Cape Hallett climate data visualization and analysis
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh

### Library ----
library(tidyverse)
library(lme4)
library(viridis)


# loading the data
climate2011 <- read.csv("Data/climate_long_2011-2018.csv")
climate2004 <- read.csv("Data/climate_long_2004-2010.csv")

### Data Manipulation 2011-2018 ----
head(climate2011)
str(climate2011)
summary(climate2011)

climate2011 <- climate2011 %>% 
                  na.omit() %>%   # removing NA's
                  filter(rH > 0) %>%  # removing incorrect rH values 
                  rename(year = Year,
                         temp = Temp) %>%  # renaming columns (lowercase)
                  mutate(VPD = as.numeric(VPD)) # changing VPD to numeric 
str(climate2011)

# adding season column
climate2011 <- climate2011 %>% 
                  mutate(season = case_when(year == "2011" & month == "11" ~ "2011/12",
                                            year == "2011" & month == "12" ~ "2011/12",
                                            year == "2012" & month == "1" ~ "2011/12",
                                            year == "2012" & month == "11" ~ "2012/13",
                                            year == "2012" & month == "12" ~ "2012/13",
                                            year == "2013" & month == "1" ~ "2012/13",
                                            year == "2013" & month == "11" ~ "2013/14",
                                            year == "2013" & month == "12" ~ "2013/14",
                                            year == "2014" & month == "1" ~ "2013/14",
                                            year == "2014" & month == "11" ~ "2014/15",
                                            year == "2014" & month == "12" ~ "2014/15",
                                            year == "2015" & month == "1" ~ "2014/15",
                                            year == "2015" & month == "11" ~ "2015/16",
                                            year == "2015" & month == "12" ~ "2015/16",
                                            year == "2016" & month == "1" ~ "2015/16",
                                            year == "2016" & month == "11" ~ "2016/17",
                                            year == "2016" & month == "12" ~ "2016/17",
                                            year == "2017" & month == "1" ~ "2016/17",
                                            year == "2017" & month == "11" ~ "2017/18",
                                            year == "2017" & month == "12" ~ "2017/18",
                                            year == "2018" & month == "1" ~ "2017/18")) %>% 
                  mutate(season = as.factor(season))

# removing 2012/13 season
climate2011 <- climate2011[!(climate2011$season == "2012/13"), ]


# fixing the time column to allow for easy change to POSIX class
climate2011 <- climate2011 %>% 
                  mutate(real_time = case_when(time == 0 ~ "00:00",
                                               time == 100 ~ "01:00",
                                               time == 200 ~ "02:00",
                                               time == 300 ~ "03:00",
                                               time == 400 ~ "04:00",
                                               time == 500 ~ "05:00",
                                               time == 600 ~ "06:00",
                                               time == 700 ~ "07:00",
                                               time == 800 ~ "08:00",
                                               time == 900 ~ "09:00",
                                               time == 1000 ~ "10:00",
                                               time == 1100 ~ "11:00",
                                               time == 1200 ~ "12:00",
                                               time == 1300 ~ "13:00",
                                               time == 1400 ~ "14:00",
                                               time == 1500 ~ "15:00",
                                               time == 1600 ~ "16:00",
                                               time == 1700 ~ "17:00",
                                               time == 1800 ~ "18:00",
                                               time == 1900 ~ "19:00",
                                               time == 2000 ~ "20:00",
                                               time == 2100 ~ "21:00",
                                               time == 2200 ~ "22:00",
                                               time == 2300 ~ "23:00"))

# creating date and date + time columns for time series plotting
climate2011 <- climate2011 %>% 
                  unite("date", c("year", "month", "day"), remove = FALSE, sep = "-") %>% 
                  unite("date_time", c("date", "real_time"), remove = FALSE, sep = " ") %>% 
                  mutate(date = as.Date(date)) %>% 
                  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M"))
str(climate2011)

### Data Manipulation 2004-2010 ----
head(climate2004)
str(climate2004)
summary(climate2004)

climate2004 <- climate2004 %>% 
                  na.omit() %>%  # removing NA's
                  mutate(date_time = as.POSIXct(date_time,   # changing to POSIX class
                                                format = "%d/%m/%Y %H:%M")) 
str(climate2004)

# adding season column
climate2004$year <- format(climate2004$date_time, format = "%Y")
climate2004$month <- format(climate2004$date_time, format = "%m")
climate2004$day <- format(climate2004$date_time, format = "%d")

climate2004 <- climate2004 %>% 
                  mutate(season = case_when(year == "2004" & month == "11" ~ "2004/5",
                                            year == "2004" & month == "12" ~ "2004/5",
                                            year == "2005" & month == "1" ~ "2005/6",
                                            year == "2005" & month == "11" ~ "2005/6",
                                            year == "2005" & month == "12" ~ "2005/6",
                                            year == "2006" & month == "1" ~ "2006/7",
                                            year == "2006" & month == "11" ~ "2006/7",
                                            year == "2006" & month == "12" ~ "2006/7",
                                            year == "2007" & month == "1" ~ "2007/8",
                                            year == "2007" & month == "11" ~ "2007/8",
                                            year == "2007" & month == "12" ~ "2007/8",
                                            year == "2008" & month == "1" ~ "2008/9",
                                            year == "2008" & month == "11" ~ "2008/9",
                                            year == "2008" & month == "12" ~ "2008/9",
                                            year == "2009" & month == "1" ~ "2009/10",
                                            year == "2009" & month == "11" ~ "2009/10",
                                            year == "2009" & month == "12" ~ "2009/10",
                                            year == "2010" & month == "1" ~ "2010/11",
                                            year == "2010" & month == "11" ~ "2010/11",
                                            year == "2010" & month == "12" ~ "2010/11")) %>% 
                  mutate(season = as.factor(season))


### Combining Data Frames ----
str(climate2011)
str(climate2004)

# making year and month integers for 'climate2004' 
climate2004 <- climate2004 %>% 
                  mutate(year = as.integer(year)) %>% 
                  mutate(month = as.integer(month)) %>% 
                  mutate(day = as.integer(day))
str(climate2004)

combo <- full_join(climate2004, climate2011)

# removing unnecessary columns
combo <- combo %>% 
            dplyr::select(date_time, temp, rH, year, month, day, season)


### Creating Summary Data ----
sum_year <- combo %>% 
              group_by(year) %>% 
              summarise(avg_temp = mean(temp),
                       # sd_temp = sd(temp),
                        min_temp = min(temp),
                        max_temp = max(temp))

sum_season <- combo %>% 
                group_by(season) %>% 
                summarise(avg_temp = mean(temp),
                        #  sd_temp = sd(temp),
                          min_temp = min(temp),
                          max_temp = max(temp))

# creating long versions for multi-variable plotting
sum_year_long <- sum_year %>% 
                    pivot_longer(cols = c(2:4),
                                 names_to = "type",
                                 values_to = "temp")
sum_season_long <- sum_season %>% 
                      pivot_longer(cols = c(2:4),
                                   names_to = "type",
                                   values_to = "temp")

### Active Days Calculations ----
active_combo <- combo %>% 
                    filter(temp > -1) %>% 
                    filter(rH > 85)

sum_active <- active_combo %>% 
                group_by(year, month) %>% 
                summarise(avg_temp = mean(temp),
                          sd_temp = sd(temp),
                          avg_rh = mean(rH),
                          sd_rh = sd(rH),
                          n_active_days = n_distinct(day)) %>% 
                mutate(month = as.factor(month))

# adding seasons to the active days summary 
sum_active <- sum_active %>% 
                mutate(season = case_when(year == "2004" & month == "12" ~ "2004/5",
                                          year == "2005" & month == "12" ~ "2005/6",
                                          year == "2007" & month == "12" ~ "2007/8",
                                          year == "2008" & month == "12" ~ "2007/8",
                                          year == "2010" & month == "12" ~ "2010/11",
                                          year == "2011" & month == "12" ~ "2011/12",
                                          year == "2012" & month == "1" ~ "2011/12",
                                          year == "2012" & month == "12" ~ "2012/13",
                                          year == "2013" & month == "1" ~ "2012/13",
                                          year == "2013" & month == "12" ~ "2013/14",
                                          year == "2014" & month == "1" ~ "2013/14",
                                          year == "2015" & month == "12" ~ "2015/16",
                                          year == "2016" & month == "1" ~ "2015/16",
                                          year == "2016" & month == "12" ~ "2016/17",
                                          year == "2018" & month == "1" ~ "2017/18")) %>% 
                mutate(season = as.factor(season))

### Data Visualization ----
## Temperature 
# temp over time (non-faceted, scatterplot) 
(temp <- ggplot(combo, aes(x = date_time, y = temp)) +
           geom_point(size = 0.1) +
           stat_smooth(method = "lm"))

# temp over time (non-faceted, boxplot) - FOR PUBLICATION 
(temp_boxplot <- ggplot(combo, aes(x = season, y = temp)) +
                    geom_boxplot(aes(fill = season)) +
                    theme(axis.text.x = element_text(angle = 90)))

# temp over time per season - FOR PUBLICATION APPENDIX 
(temp_facet <- ggplot(combo, aes(x = date_time, y = temp, fill = season)) +   
                 geom_line(aes(color = season)) +
                 scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
                 facet_wrap(vars(season), ncol = 3, scales = "free_x") +
                 scale_color_viridis(discrete = TRUE))

# average, minimum and maximum temperature over time (by year)
(avg_temp_year <- ggplot(sum_year_long, aes(x = year, y = temp, 
                                            color = type, shape = type)) +
                    geom_point(size = 2.5) +  
                    geom_line() +
                    stat_smooth(method = "lm"))

# average, minimum and maximum temperature over time (by season) - FOR DISSERTATION
(avg_temp_season <- ggplot(sum_season_long, aes(x = season, y = temp, 
                                                color = type, shape = type)) +
                      geom_point(size = 2.5) +  
                      geom_line(aes(group = type)) +
                      geom_hline(yintercept = 0, linetype = "dashed") +
                      theme_classic() +
                      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                      scale_color_manual(values = c("#004452", "#B5BA4F", "#12A7B8")))


## Relative Humidity 
# rH over time (non-faceted, scatterplot)
(rh <- ggplot(combo, aes(x = date_time, y = rH)) +
          geom_point(size = 0.1) +
          stat_smooth(method = "lm"))

# rH over time (non-faceted, boxplot) - FOR PUBLICATION 
(rh_boxplot <- ggplot(combo, aes(x = season, y = rH)) +
                  geom_boxplot() +
                  theme(axis.text.x = element_text(angle = 90)))

# rH over time per season - FOR PUBLICATION APPENDIX ??
(rh_facet <- ggplot(combo, aes(x = date_time, y = rH, fill = season)) +   
                geom_line(aes(color = season)) +
                scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
                facet_wrap(vars(season), ncol = 3, scales = "free_x") +
                scale_color_viridis(discrete = TRUE))


### Data Analysis for Temperature ----
## Simple linear models 
temp_m <- lm(temp ~ year, data = combo)
summary(temp_m)  # p = 0.749, not at all significant
anova(temp_m)

# checking model assumptions 
plot(temp_m)  # not very normally distributed 
hist(resid(temp_m))  # skewed, violates model assumptions 
bptest(temp_m)  # there is heteroskedasticity in the model, violates assumptions

# creating a null model to compare it to
temp_null <- lm(temp ~ 1, data = combo)
AIC(temp_m, temp_null)  # null model is better 

## Mixed effects models 
# if std error > estimate = year doesn't explain much of the variation
# can calculate % of leftover variation that's explained by the random effects 
# estimate for year = after controlling for the random effects 

temp_mixed <- lmer(temp ~ year + (1|year), data = combo, REML = F)
temp_month_mixed <- lmer(temp ~ year + (1|month), data = combo, REML = F)
temp_day_mixed <- lmer(temp ~ year + (1|day), data = combo, REML = F)
temp_date_mixed <- lmer(temp ~ year + (1|date_time), data = combo, REML = F)
temp_season_mixed <- lmer(temp ~ year + (1|season), data = combo, REML = F)

AIC(temp_mixed, temp_month_mixed, temp_day_mixed, temp_date_mixed, temp_season_mixed)
# month as random effect is best

# models that failed to converge: (1|month) + (1|day), (year|month), (month|day)
temp_month_season <- lmer(temp ~ year + (1|month) + (1|season), data = combo, REML = F)
temp_month_date <- lmer(temp ~ year + (1|month) + (1|date_time), data = combo, REML = F)

anova(temp_month_mixed, temp_month_season)  # month_season is best
anova(temp_month_mixed, temp_month_date)    # month_date is better
AIC(temp_month_season, temp_month_date)     # month_season is best  

# creating null models to compare it with
temp_m_null <- lmer(temp ~ 1 + (1|month) + (1|season), data = combo, REML = F)
temp_m_null2 <- lmer(temp ~ 1 + (1|month), data = combo, REML = F)

# anova states which model is better at capturing the data
anova(temp_m_null, temp_m_null2)
anova(temp_m_null, temp_month_season) 
# model with year as a fixed effect is better than the null models   

summary(temp_month_season)   # year: -0.2165 (std error: 0.06413), small effect size 
confint(temp_month_season)   # year: -0.3463 to -0.08523 
# 95% confident the correlation between year and temperature is between those 2 values 

anova(temp_month_season, temp_m)


### Data Analysis for Relative Humidity ----
## Simple linear models 
rh_m <- lm(rH ~ year, data = combo)
summary(rh_m)  # p << 0.05, significant 

# checking model assumptions 
plot(rh_m)  
hist(resid(rh_m))  # slightly skewed potentially 
bptest(rh_m)  # no heteroskedasticity 

## Mixed effects models 
rh_mixed <- lmer(rH ~ year + (1|year), data = combo)
rh_month_mixed <- lmer(rH ~ year + (1|month), data = combo)
rh_day_mixed <- lmer(rH ~ year + (1|day), data = combo)
rh_season_mixed <- lmer(rH ~ year + (1|season), data = combo)

AIC(rh_mixed, rh_month_mixed, rh_day_mixed, rh_season_mixed)
# month as random effect is best

# models that failed to converge: (month|day)
rh_month_season <- lmer(rH ~ year + (1|month) + (1|season), data = combo, REML = F)
rh_month_day <- lmer(rH ~ year + (1|month) + (1|day), data = combo, REML = F)

AIC(rh_month_mixed, rh_month_season, rh_month_day)  # month_day is best 

# creating null models to compare it with
rh_m_null <- lmer(temp ~ 1 + (1|month) + (1|day), data = combo, REML = F)
rh_m_null2 <- lmer(temp ~ 1 + (1|month), data = combo, REML = F)

# anova states which model is better at capturing the data
anova(rh_m_null, rh_m_null2)
anova(rh_m_null, rh_month_day)  # null model a lot better than the month_day model 

summary(rh_month_day)   # year: 0.1607 (std error: 0.01873)
confint(rh_month_day)   # year: 0.1243 to 0.1977 
# 95% confident the correlation between year and rH is between those 2 values 

# HOZOVA-8512

# Following repository provides dataset, simple code in R and paper summarizing results of the exercise.

# CODE
library('dplyr')
library(stargazer)

# summary
summary(prepared_data_for_research_Copy)

# plot
library(ggplot2)
newdata <- prepared_data_for_research_Copy[ which(prepared_data_for_research_Copy$Time==12),]
theme_set(theme_bw())
ggplot(newdata, aes(x=Average_temperature)) + 
  geom_line(aes(y=Energy_consumption), size=0.1) +
    labs(y="Energy consumption",
         x="Average temperature",
         caption="Energy consumption in MW")

# transformation of variables
lnconsump <- log(prepared_data_for_research_Copy$'Energy_consumption')
prepared_data_for_research_Copy$lnconsump <- lnconsump

# regression
dcidreg = lm(lnconcump ~ DST + treatment + DSTtreatment + Average_temperature +      Temperature_squared + Pressure + Relative_humidity + Weekend + Public_holiday, data = prepared_data_for_research_Copy)
summary(dcidreg)

# robustness check
didreg_rch = lm(lnconcump ~ DST + treatment + DSTtreatment + Cooling_degrees + Heating_degrees + Pressure + Relative_humidity + Weekend + Public_holiday, data = prepared_data_for_research_Copy)
summary(didreg_rch)

stargazer(dcidreg, didreg_rch, title="Regression Results', dep.var.labels=c("Electricity consumption", "Electricity consumption"), ci=TRUE, ci.level=0.90, single.row=TRUE)

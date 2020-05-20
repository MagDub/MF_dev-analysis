#### From example: https://www.datanovia.com/en/lessons/anova-in-r/#one-way-independent-measures

library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(readxl)



dataMFdev <- read_excel("~/GoogleDrive/UCL/MFdev/excels/data_thompson_mod12_B2.xlsx")           

meas =  "first_SH"

# Take only subset: concatenate the ones we want
data_tmp <- subset(dataMFdev , select=c("ID", meas, "ageGroup", "age", "isFemale"))

# Summary statistics
data_tmp %>%
  group_by(ageGroup) %>%
  get_summary_stats(meas, type = "mean_sd")

# Outliers
out <-data_tmp %>%
  group_by(ageGroup) %>%
  identify_outliers(meas)

out


##############
# Anova computation, no covariate
res.aov <- anova_test(
  data = data_tmp, dv = meas, wid = ID,
  between = ageGroup,
  effect.size = "pes"
)

# Pairwise comparisons
pwc <- data_tmp %>% 
  emmeans_test(
    p.adjust.method = "none"
  )
pwc
#### From example: https://www.datanovia.com/en/lessons/anova-in-r/#one-way-independent-measures

rm_anova_MF_1way_nocov <- function(meas) {
  
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(lsr)
library(readxl)

dataMFdev <- read_excel("~/GoogleDrive/UCL/MFdev/excels/data_thompson_mod12_B2.xlsx")           
  
# Take only subset
data_tmp <- dataMFdev %>%
  select(ID, ageGroup, meas) %>%
  rename(meas = meas)

# Anova computation, no covariate
tab <- anova_test(
  data = data_tmp, dv = meas, wid = ID,
  between = ageGroup,
  effect.size = "pes"
)

# Pairwise comparisons
pwc <- data_tmp %>% 
  emmeans_test(
    meas ~ ageGroup,
    p.adjust.method = "none"
  )
pwc


# Split by pairs
data_tmp12 <- data_tmp[data_tmp$ageGroup == 1 | data_tmp$ageGroup == 2, ]
data_tmp23 <- data_tmp[data_tmp$ageGroup == 2 | data_tmp$ageGroup == 3, ]
data_tmp13 <- data_tmp[data_tmp$ageGroup == 1 | data_tmp$ageGroup == 3, ]

# Age effect size
effect_pwcD12 <-cohensD(meas ~ ageGroup, data = data_tmp12)
effect_pwcD23 <-cohensD(meas ~ ageGroup, data = data_tmp23)
effect_pwcD13 <-cohensD(meas ~ ageGroup, data = data_tmp13)


sentence1=paste(
  " (age group main effect: F(",
  tab$DFn[1],",",tab$DFd[1],")=",round(tab$F[1],3),", p=", round(tab$p[1],3), ", pes=", round(tab$pes[1],3),
  ")") 

sentence2=paste(
  "Pairwise comparisons for age group effect (group1 vs group2: t(",
  pwc$df[1],")=", round(pwc$statistic[1],3),", p=", round(pwc$p[1],3),  ", d=", round(effect_pwcD12,3),
  "; group2 vs group3: t(",
  pwc$df[3],")=", round(pwc$statistic[3],3),", p=", round(pwc$p[3],3),  ", d=", round(effect_pwcD23,3),
  "; group1 vs group3: t(",
  pwc$df[2],")=", round(pwc$statistic[2],3),", p=", round(pwc$p[2],3),  ", d=", round(effect_pwcD13,3),
  ")")

mid=paste("-------------------------------------------------------")

output_txt = c(sentence1,paste(""), mid, paste(""),sentence2)

return(output_txt)

}
# source and then on terminal ex: 
# s<-rm_anova_MF('freq_D_picked_shortH', 'freq_D_picked_longH')

# From example: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

rm_anova_MFdev_nocov <- function(x1, x2) {

  library(car)
  library(tidyverse)
  library(ggpubr)
  library(rstatix)
  library(readxl)
  library(lsr)
  
  
  dataMFdev <- read_excel("~/GoogleDrive/UCL/MFdev/excels/data_thompson_mod12_B2.xlsx")   
  
  # Take only subset: concatenate the ones we want
  data_tmp <- subset(dataMFdev , select=c("ID", x1, x2, "ageGroup", "Wasi_IQ"))
  
  # Change from wide to long format
  data_tmp <- data_tmp %>%
    gather(key = "hor", value = "freq", x1, x2) %>%
    convert_as_factor(ID, hor)
  
  # Summary statistics
  data_tmp %>%
    group_by(hor, ageGroup) %>%
    get_summary_stats(freq, type = "mean_sd")
  
  # Anova computation
  two.way <- anova_test(
    data = data_tmp, dv = freq, wid = ID,
    within = hor,
    between = ageGroup,
    effect.size = "pes"
  )
  
  tab<-get_anova_table(two.way)
  
  # Pairwise comparisons
  pwcH <- data_tmp %>%
    group_by(ageGroup) %>%
    pairwise_t_test(freq ~ hor, paired = TRUE, pool.sd = FALSE)
  
  # Pairwise comparisons
  pwcD <- data_tmp %>%
    pairwise_t_test(freq ~ ageGroup, paired = FALSE, pool.sd = FALSE)
  
  # Pairwise comparisons
  pwcHD <- data_tmp %>%
    group_by(hor) %>%
    pairwise_t_test(freq ~ ageGroup, paired = FALSE, pool.sd = FALSE)
  
  # Split by group
  data_tmp1 <- data_tmp[data_tmp$ageGroup == 1, ]
  data_tmp2 <- data_tmp[data_tmp$ageGroup == 2, ]
  data_tmp3 <- data_tmp[data_tmp$ageGroup == 3, ]
  
  # Split by pairs
  data_tmp12 <- data_tmp[data_tmp$ageGroup == 1 | data_tmp$ageGroup == 2, ]
  data_tmp23 <- data_tmp[data_tmp$ageGroup == 2 | data_tmp$ageGroup == 3, ]
  data_tmp13 <- data_tmp[data_tmp$ageGroup == 1 | data_tmp$ageGroup == 3, ]
  
  # Horizon effect size
  effect_pwcH1 <-cohensD(freq ~ hor, data = data_tmp1, method = "paired")
  effect_pwcH2 <-cohensD(freq ~ hor, data = data_tmp2, method = "paired")
  effect_pwcH3 <-cohensD(freq ~ hor, data = data_tmp3, method = "paired")
  
  # Age effect size
  t_test_D12 <-t.test(freq ~ ageGroup, data = data_tmp12)
  effect_pwcD12 <-cohensD(freq ~ ageGroup, data = data_tmp12)
  effect_pwcD23 <-cohensD(freq ~ ageGroup, data = data_tmp23)
  effect_pwcD13 <-cohensD(freq ~ ageGroup, data = data_tmp13)
  
  sentence1=paste(
    " (horizon main effect: F(", 
    tab$DFn[2],",",tab$DFd[2],")=",round(tab$F[2],3),", p=", round(tab$p[2],3), ", pes=", round(tab$pes[2],3), 
    "; age main effect: F(",
    tab$DFn[1],",",tab$DFd[1],")=",round(tab$F[1],3),", p=", round(tab$p[1],3), ", pes=", round(tab$pes[1],3),
    "; age-by-horizon interaction: F(",
    tab$DFn[3],",",tab$DFd[3],")=",round(tab$F[3],3),", p=", round(tab$p[3],3), ", pes=", round(tab$pes[3],3),
    ")") 
  
  sentence2=paste(
    "Pairwise comparisons for horizon effect (group 1: t(",
    pwcH$n1[1],")=", round(pwcH$statistic[1],3),", p=", round(pwcH$p[1],3), ", d=", round(effect_pwcH1,3),
    "; group 2: t(",
    pwcH$n1[2],")=", round(pwcH$statistic[2],3),", p=", round(pwcH$p[2],3), ", d=", round(effect_pwcH2,3),
    "; group 3: t(",
    pwcH$n1[3],")=", round(pwcH$statistic[3],3),", p=", round(pwcH$p[3],3), ", d=", round(effect_pwcH3,3),
    ")")
  
  sentence3=paste(
    "Pairwise comparisons for age effect (group 1 vs 2: t(",
    pwcD$n1[1],")=", round(pwcD$statistic[1],3),", p=", round(pwcD$p[1],3), ", d=", round(effect_pwcD12,3),
    "; group 1 vs 3: t(",
    pwcD$n1[2],")=", round(pwcD$statistic[2],3),", p=", round(pwcD$p[2],3), ", d=", round(effect_pwcD13,3),
    "; group 2 vs 3: t(",
    pwcD$n1[3],")=", round(pwcD$statistic[3],3),", p=", round(pwcD$p[3],3), ", d=", round(effect_pwcD23,3),
    ")")
  
  
  sentence4=paste(
    "Pairwise comparisons for horizon x age effect: ", 
    " short horizon: ", 
    "(group 1 vs 2: t(",
    pwcHD$n1[4],")=", round(pwcHD$statistic[4],3),", p=", round(pwcHD$p[4],3),
    "; group 1 vs 3: t(",
    pwcHD$n1[5],")=", round(pwcHD$statistic[5],3),", p=", round(pwcHD$p[5],3),
    "; group 2 vs 3: t(",
    pwcHD$n1[6],")=", round(pwcHD$statistic[6],3),", p=", round(pwcHD$p[6],3),
    " long horizon: ", 
    "(group 1 vs 2: t(",
    pwcHD$n1[1],")=", round(pwcHD$statistic[1],3),", p=", round(pwcHD$p[1],3),
    "; group 1 vs 3: t(",
    pwcHD$n1[2],")=", round(pwcHD$statistic[2],3),", p=", round(pwcHD$p[2],3),
    "; group 2 vs 3: t(",
    pwcHD$n1[3],")=", round(pwcHD$statistic[3],3),", p=", round(pwcHD$p[3],3),
    ")) ")
  
  mid=paste("-------------------------------------------------------")

  output_txt = c(sentence1,paste(""), mid, paste(""),sentence2,paste(""), mid, paste(""),sentence3,paste(""), mid, paste(""),sentence4)
  
  return(output_txt)
}


# source and then on terminal ex: 
# s<-rm_anova_MF('freq_D_picked_shortH', 'freq_D_picked_longH')

# From example: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

rm_anova_MFdev_cov <- function(x1, x2) {
  
  #x1='low_SH'
  #x2='low_LH'

  library(car)
  library(tidyverse)
  library(ggpubr)
  library(rstatix)
  library(readxl)
  
  dataMFdev <- read_excel("~/GoogleDrive/UCL/MFdev/excels/data_thompson_mod12_final_B2.xlsx")  
  
  # Take only subset: concatenate the ones we want
  data_tmp <- subset(dataMFdev , select=c("ID", x1, x2, "ageGroup", "Wasi_IQ", "conners_adhd_tscore"))
  
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
    covariate = c(Wasi_IQ, conners_adhd_tscore),
    effect.size = "pes"
  )
  
  tab<-get_anova_table(two.way)
  
  # Pairwise comparisons
  pwcH <- data_tmp %>%
    group_by(ageGroup) %>%
    pairwise_t_test(freq ~ hor, paired = TRUE, pool.sd = FALSE, p.adjust.method = "bonferroni")
  
  # Pairwise comparisons
  pwcD <- data_tmp %>%
    pairwise_t_test(freq ~ ageGroup, paired = FALSE, pool.sd = FALSE, p.adjust.method = "bonferroni")
  
  # Pairwise comparisons
  pwcHD <- data_tmp %>%
    group_by(hor) %>%
    pairwise_t_test(freq ~ ageGroup, paired = FALSE, pool.sd = FALSE, p.adjust.method = "bonferroni")
  
  sentence1=paste(
    " (horizon main effect: F(", 
    tab$DFn[4],",",tab$DFd[4],")=",round(tab$F[4],3),", p=", round(tab$p[4],3), ", pes=", round(tab$pes[4],3), 
    "; age main effect: F(",
    tab$DFn[3],",",tab$DFd[3],")=",round(tab$F[3],3),", p=", round(tab$p[3],3), ", pes=", round(tab$pes[3],3),
    "; adhd main effect: F(",
    tab$DFn[2],",",tab$DFd[2],")=",round(tab$F[2],3),", p=", round(tab$p[2],3), ", pes=", round(tab$pes[2],3),
    "; WASI main effect: F(",
    tab$DFn[1],",",tab$DFd[1],")=",round(tab$F[1],3),", p=", round(tab$p[1],3), ", pes=", round(tab$pes[1],3),
    "; age-by-horizon interaction: F(",
    tab$DFn[5],",",tab$DFd[5],")=",round(tab$F[5],3),", p=", round(tab$p[5],3), ", pes=", round(tab$pes[5],3),
    "; WASI-by-horizon interaction: F(",
    tab$DFn[4],",",tab$DFd[4],")=",round(tab$F[4],3),", p=", round(tab$p[4],3), ", pes=", round(tab$pes[4],3),
    ")") 
  
  
  sentence2=paste(
    "Pairwise comparisons for horizon effect (group 1: t(",
    pwcH$n1[1],")=", round(pwcH$statistic[1],3),", p=", round(pwcH$p[1],3),
    "; group 2: t(",
    pwcH$n1[2],")=", round(pwcH$statistic[2],3),", p=", round(pwcH$p[2],3),
    "; group 3: t(",
    pwcH$n1[3],")=", round(pwcH$statistic[3],3),", p=", round(pwcH$p[3],3),
    ")")
  
  sentence3=paste(
    "Pairwise comparisons for age effect (group 1 vs 2: t(",
    pwcD$n1[1],")=", round(pwcD$statistic[1],3),", p=", round(pwcD$p[1],3),
    "; group 1 vs 3: t(",
    pwcD$n1[2],")=", round(pwcD$statistic[2],3),", p=", round(pwcD$p[2],3),
    "; group 2 vs 3: t(",
    pwcD$n1[3],")=", round(pwcD$statistic[3],3),", p=", round(pwcD$p[3],3),
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


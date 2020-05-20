
source('~/GoogleDrive/UCL/MFdev/analysis/stats/R_scripts/fct_analysis_1way_nocov.R')

#### ALL GROUPS

all_text = c(
  
  '', '', 
  'GENDER:','', rm_anova_MF_1way_nocov('isFemale'),'','', '', 
  'WASI:','', rm_anova_MF_1way_nocov('Wasi_IQ'),'','', '', 
  'WASI_RAW:','', rm_anova_MF_1way_nocov('Wasi_Raw'),'','', '', 
  'AGE:','', rm_anova_MF_1way_nocov('age'),'','', '',
  'Conners:','', rm_anova_MF_1way_nocov('conners_adhd_tscore'),'','', '',
  'Conners z:','', rm_anova_MF_1way_nocov('conners_z')
  
)

fileConn<-file("~/GoogleDrive/UCL/MFdev/analysis/stats/txt_res/results_1way.txt")
writeLines(all_text, fileConn)

source('~/GoogleDrive/UCL/MFdev/analysis/stats/R_scripts/fct_analysis_2way_cov.R')

#### ALL GROUPS

all_text = c(
  
  '', '', 
  
  'HIGH VALUE ', '', rm_anova_MFdev_cov('high_SH', 'high_LH'),'','', '',  
  'MEDIUM VALUE:','', rm_anova_MFdev_cov('med_SH', 'med_LH'),'','', '', 
  'LOW VALUE:', '',rm_anova_MFdev_cov('low_SH', 'low_LH'),'','', '', 
  'NOVEL VALUE:', '',rm_anova_MFdev_cov('novel_SH', 'novel_LH'),'','', '', 
  
  'SGM0:','', rm_anova_MFdev_cov('sgm0_SH', 'sgm0_LH'),'','', '', 
  'EPSILON:', '',rm_anova_MFdev_cov('xi_SH', 'xi_LH'),'','', '', 
  'NOV:', '',rm_anova_MFdev_cov('eta_SH', 'eta_LH'),'','', '', 
  
  'CONSIST:','', rm_anova_MFdev_cov('consistent_SH', 'consistent_LH'),'','', '', 
  'IG:','', rm_anova_MFdev_cov('IG_SH', 'IG_LH'),'','', '', 
  'EV:','', rm_anova_MFdev_cov('EV_SH', 'EV_LH'),'','', '', 
  
  'score 1st SH vs 1st LH:','', rm_anova_MFdev_cov('first_SH', 'first_LH'),'','', '', 
  'score 1st SH vs all LH::','', rm_anova_MFdev_cov('first_SH', 'all_LH')
  
  #'RT:','', rm_anova_MFdev_cov('RT_SH', 'RT_LH') 
  
)

fileConn<-file("~/GoogleDrive/UCL/MFdev/analysis/stats/txt_res/results_cov_B2.txt")
writeLines(all_text, fileConn)
close(fileConn)

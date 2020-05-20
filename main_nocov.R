
source('~/GoogleDrive/UCL/MFdev/analysis/stats/R_scripts/fct_analysis_2way_nocov.R')

#### ALL GROUPS

all_text = c(
  
  '', '', 
  
  'HIGH VALUE ', '', rm_anova_MFdev_nocov('high_SH', 'high_LH'),'','', '',  
  'MEDIUM VALUE:','', rm_anova_MFdev_nocov('med_SH', 'med_LH'),'','', '', 
  'LOW VALUE:', '',rm_anova_MFdev_nocov('low_SH', 'low_LH'),'','', '', 
  'NOVEL VALUE:', '',rm_anova_MFdev_nocov('novel_SH', 'novel_LH'),'','', '', 
  
  'SGM0:','', rm_anova_MFdev_nocov('sgm0_SH', 'sgm0_LH'),'','', '', 
  'EPSILON:', '',rm_anova_MFdev_nocov('xi_SH', 'xi_LH'),'','', '', 
  'NOV:', '',rm_anova_MFdev_nocov('eta_SH', 'eta_LH'),'','', '', 
  
  'CONSIST:','', rm_anova_MFdev_nocov('consistent_SH', 'consistent_LH'),'','', '', 
  'IG:','', rm_anova_MFdev_nocov('IG_SH', 'IG_LH'),'','', '', 
  'EV:','', rm_anova_MFdev_nocov('EV_SH', 'EV_LH'),'','', '', 
  
  'score 1st SH vs 1st LH:','', rm_anova_MFdev_nocov('first_SH', 'first_LH'),'','', '', 
  'score 1st SH vs all LH::','', rm_anova_MFdev_nocov('first_SH', 'all_LH'),'','', '', 
  
  'score LH explored vs exploit 1st','', rm_anova_MFdev_nocov('mean_explored_1', 'mean_exploit_1'),'','', '', 
  'score LH explored vs exploit last','', rm_anova_MFdev_nocov('mean_explored_6', 'mean_exploit_6')
  
  #'RT:','', rm_anova_MFdev_nocov('RT_SH', 'RT_LH') 
  
)

fileConn<-file("~/GoogleDrive/UCL/MFdev/analysis/stats/txt_res/results_nocov_B2.txt")
writeLines(all_text, fileConn)
close(fileConn)

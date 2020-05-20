
# from https://www.personality-project.org/r/html/partial.r.html 

library(psych)
library(ggpubr)
library(readxl)


x1 <- 'xi_SH'
x2 <- 'xi_LH'

dataMFdev <- read_excel("~/GoogleDrive/UCL/MFdev/excels/data_thompson_mod12_B2.xlsx")    

# Take only subset: concatenate the ones we want
data_tmp <- subset(dataMFdev , select=c("ID", x1, x2, "eta_SH", "eta_LH", "age", "ageGroup", "Wasi_IQ", "conners_adhd_tscore"))

data_tmp$mean <- rowMeans(data_tmp[c(x1, x2)], na.rm=TRUE)

parSH <- partial.r(data_tmp,c(x1,"conners_adhd_tscore"),c("age", "Wasi_IQ"), use="pairwise",method="pearson")
parLH <- partial.r(data_tmp,c(x2,"conners_adhd_tscore"),c("age", "Wasi_IQ"), use="pairwise",method="pearson")
parmean <- partial.r(data_tmp,c("mean","conners_adhd_tscore"),c("age", "Wasi_IQ"), use="pairwise",method="pearson")


#cor.test(data_tmp$nov_SH, data_tmp$conners_adhd_tscore, method = "spearman")

# To find the confidence intervals and "significance" of the correlations, 
# use the corr.p function with n = n - s where s is the numer of covariates.
n=99-1

cpSH <- corr.p(parSH,n)  
cpLH <- corr.p(parLH,n) 
cpmean <- corr.p(parmean,n) 

print(cpSH,short=FALSE)
print(cpLH,short=FALSE)
print(cpmean,short=FALSE)


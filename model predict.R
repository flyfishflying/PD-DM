load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_data_new.Rdata")
Interpolation_weighted$HTN_status<-Interpolation_data_new$HTN_status
Interpolation_weighted$HPL_status<-Interpolation_data_new$HPL_status
Interpolation_weighted$Age<-Interpolation_data_new$Age
Interpolation_weighted$BMI<-Interpolation_data_new$BMI

table(Interpolation_weighted$Age,useNA = "ifany")
table(Interpolation_weighted$BMI,useNA = "ifany")

library(pec)
library(survival)
library(foreign)
library(rms)
library(boot)
#install.packages("survIDINRI")
library(survIDINRI)
#install.packages("DescTools")
library("DescTools")
set.seed(1450)
#1 c-index
#all
#PD
c_index<-function(data,indices){
  dat <- data[indices,]
  vames<-c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")
  FML <- as.formula(paste('Surv(peryear,  MORT_stat==1)~',paste(vames, collapse = "+")))
  fit<- coxph(FML,data =dat )
  pr1<-predict(fit,newdata=dat)
  Cindex=rcorrcens(Surv(peryear,  MORT_stat==1) ~ pr1, data =dat)[1]
  Cindex=1-Cindex
  Cindex 
 }
c_index(Interpolation_weighted,1:1000)
results <- boot(data=Interpolation_weighted, statistic=c_index, R=1000)
c_index_PD<-results
#no PD
c_index<-function(data,indices){
  dat <- data[indices,]
  vames<-c("Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")
  FML <- as.formula(paste('Surv(peryear,  MORT_stat==1)~',paste(vames, collapse = "+")))
  fit<- coxph(FML,data =dat )
  pr1<-predict(fit,newdata=dat)
  Cindex=rcorrcens(Surv(peryear,  MORT_stat==1) ~ pr1, data =dat)[1]
  Cindex=1-Cindex
  Cindex 
}
c_index(Interpolation_weighted,1:1000)
results <- boot(data=Interpolation_weighted, statistic=c_index, R=1000)
c_index_noPD<-results
c_index_Change<-data.frame(c_index_PD$t,c_index_noPD$t)
MeanDiff<-MeanDiffCI(c_index_PD$t,c_index_noPD$t)
#print(c_index_PD) 
#print(c_index_noPD)
#plot(results)
c_index_PD_ci<-boot.ci(c_index_PD,conf = 0.95,type = c("norm"))
c_index_noPD_ci<-boot.ci(c_index_noPD,conf = 0.95,type = c("norm"))
c_index_all<-data.frame('model'=c("noPD","PD","Change"),
                    'Mean'=c(as.numeric(c_index_noPD[1]),as.numeric(c_index_PD[1]),as.numeric(MeanDiff[1])),
                    'lower .95'=c(c_index_noPD_ci[["normal"]][2],c_index_PD_ci[["normal"]][2],as.numeric(MeanDiff[2])),
                    'upper .95'=c(c_index_noPD_ci[["normal"]][3],c_index_PD_ci[["normal"]][3],as.numeric(MeanDiff[3])))
#cvd
#PD
c_index<-function(data,indices){
  dat <- data[indices,]
  vames<-c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")
  FML <- as.formula(paste('Surv(peryear,  CVD_MORT_stat==1)~',paste(vames, collapse = "+")))
  fit<- coxph(FML,data =dat )
  pr1<-predict(fit,newdata=dat)
  Cindex=rcorrcens(Surv(peryear,  CVD_MORT_stat==1) ~ pr1, data =dat)[1]
  Cindex=1-Cindex
  Cindex 
}
c_index(Interpolation_weighted,1:1000)
results <- boot(data=Interpolation_weighted, statistic=c_index, R=1000)
c_index_PD<-results
#no PD
c_index<-function(data,indices){
  dat <- data[indices,]
  vames<-c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")
  FML <- as.formula(paste('Surv(peryear,  CVD_MORT_stat==1)~',paste(vames, collapse = "+")))
  fit<- coxph(FML,data =dat )
  pr1<-predict(fit,newdata=dat)
  Cindex=rcorrcens(Surv(peryear,  CVD_MORT_stat==1) ~ pr1, data =dat)[1]
  Cindex=1-Cindex
  Cindex 
}
c_index(Interpolation_weighted,1:1000)
results <- boot(data=Interpolation_weighted, statistic=c_index, R=1000)
c_index_noPD<-results
c_index_Change<-data.frame(c_index_PD$t,c_index_noPD$t)
MeanDiff<-MeanDiffCI(c_index_PD$t,c_index_noPD$t)
#print(c_index_PD) 
#print(c_index_noPD)
#plot(results)
c_index_PD_ci<-boot.ci(c_index_PD,conf = 0.95,type = c("norm"))
c_index_noPD_ci<-boot.ci(c_index_noPD,conf = 0.95,type = c("norm"))
c_index_cvd<-data.frame('model'=c("noPD","PD","Change"),
                        'Mean'=c(as.numeric(c_index_noPD[1]),as.numeric(c_index_PD[1]),as.numeric(MeanDiff[1])),
                        'lower .95'=c(c_index_noPD_ci[["normal"]][2],c_index_PD_ci[["normal"]][2],as.numeric(MeanDiff[2])),
                        'upper .95'=c(c_index_noPD_ci[["normal"]][3],c_index_PD_ci[["normal"]][3],as.numeric(MeanDiff[3])))
c_index_all$mortality<-"All-cause"
c_index_cvd$mortality<-"CVD-related"
c_index<-rbind(c_index_all,c_index_cvd)
c_index
write.table(c_index,sep = ",",file ="G:/paper_2_PD&DM/data_DC/c-index.csv")


#nri
library(nricens)
#all
#coxph fit构建Cox生存函数模型
mstd= coxph(Surv(peryear,  MORT_stat==1)~ Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status, Interpolation_weighted ,x=TRUE)
mnew= coxph(Surv(peryear,  MORT_stat==1)~ PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status, Interpolation_weighted, x=TRUE)
#predicted risk at t0=2000,2000天时间点的死亡风险
p.std= get.risk.coxph(mstd, t0=5)
p.new= get.risk.coxph(mnew, t0=5)
nri_5_all<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 5, cut = c(0.05, 0.10),
                niter = 1000, updown = 'category')
nri_10_all<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 10, cut = c(0.05, 0.10),
        niter = 1000, updown = 'category')
nri_5_all_diff<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 5, cut=0.01,
                   niter = 1000, updown = 'diff')
nri_10_all_diff<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 10, cut=0.01,
                        niter = 1000, updown = 'diff')

#cvd
#coxph fit构建Cox生存函数模型
mstd= coxph(Surv(peryear,  CVD_MORT_stat==1)~ Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status, Interpolation_weighted ,x=TRUE)
mnew= coxph(Surv(peryear,  CVD_MORT_stat==1)~ PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status, Interpolation_weighted, x=TRUE)
#predicted risk at t0=2000,2000天时间点的死亡风险
p.std= get.risk.coxph(mstd, t0=5)
p.new= get.risk.coxph(mnew, t0=5)
nri_5_cvd<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 5, cut = c(0.05, 0.10),
                   niter = 1000, updown = 'category')
nri_10_cvd<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 10, cut = c(0.05, 0.10),
                    niter = 1000, updown = 'category')
nri_5_cvd_diff<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 5, cut=0.01,
                        niter = 1000, updown = 'diff')
nri_10_cvd_diff<-nricens(mdl.std= mstd, mdl.new = mnew, t0 = 10, cut=0.01,
                         niter = 1000, updown = 'diff')

save(nri_5_all, nri_10_all,nri_5_all_diff,nri_10_all_diff,nri_5_cvd,nri_10_cvd,nri_5_cvd_diff,nri_10_cvd_diff,file = "G:/paper_2_PD&DM/data_DC/nri.RData")
nri_5<-cbind(nri_5_all$nri,nri_5_cvd$nri)
nri_5$year<-5
nri_10<-cbind(nri_10_all$nri,nri_10_cvd$nri)
nri_10$year<-10
nri_Categorical<-rbind(nri_5,nri_10)
nri_Categorical<-round(nri_Categorical,3)
nri_Categorical$group<-"Categorical"
nri_5_diff<-cbind(nri_5_all_diff$nri,nri_5_cvd_diff$nri)
nri_5_diff$year<-5
nri_10_diff<-cbind(nri_10_all_diff$nri,nri_10_cvd_diff$nri)
nri_10_diff$year<-10
nri_Continuous<-rbind(nri_5_diff,nri_10_diff)
nri_Continuous<-round(nri_Continuous,3)
nri_Continuous$group<-"Continuous"
nri<-rbind(nri_Categorical,nri_Continuous)
write.table(nri,sep = ",",file ="G:/paper_2_PD&DM/data_DC/nri.csv")
#IDI
#all
colnames(Interpolation_weighted)
#covs0<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs0<-model.matrix(~Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted)
#covs1<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs1<-model.matrix(~PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted)
outcome=Interpolation_weighted[,c("peryear","MORT_stat")]
outcome$peryear<-outcome$peryear
outcome$MORT_stat<-as.numeric(outcome$MORT_stat)
outcome$MORT_stat[outcome$MORT_stat==1]<-0
outcome$MORT_stat[outcome$MORT_stat==2]<-1
na.omit(covs0)
outcome<-as.matrix(outcome)
library(tibble)  
covs1=as_tibble(covs1)
covs1<-covs1[,-1] 
covs0=as_tibble(covs0)
covs0<-covs0[,-1] 
x<-IDI.INF(outcome, covs0,covs1,  t0=5, npert=1000) 
IDI_5_ALL<-IDI.INF.OUT(x) 

#CVD
Interpolation_weighted_CVD<-Interpolation_weighted[,c("peryear","PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status","CVD_MORT_stat")]
colnames(Interpolation_weighted_CVD)
Interpolation_weighted_CVD<-na.omit(Interpolation_weighted_CVD)
#covs0<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs0<-model.matrix(~Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted_CVD)
#covs1<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs1<-model.matrix(~PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted_CVD)
outcome=Interpolation_weighted_CVD[,c("peryear","CVD_MORT_stat")]
outcome$peryear<-outcome$peryear
outcome$CVD_MORT_stat<-as.numeric(outcome$CVD_MORT_stat)
outcome$CVD_MORT_stat[outcome$CVD_MORT_stat==1]<-0
outcome$CVD_MORT_stat[outcome$CVD_MORT_stat==2]<-1
na.omit(covs0)
outcome<-as.matrix(outcome)
library(tibble)  
covs1=as_tibble(covs1)
covs1<-covs1[,-1] 
covs0=as_tibble(covs0)
covs0<-covs0[,-1] 
x<-IDI.INF(outcome, covs0,covs1,  t0=5, npert=1000) 
IDI_5_CVD<-IDI.INF.OUT(x) 

#all
colnames(Interpolation_weighted)
#covs0<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs0<-model.matrix(~Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted)
#covs1<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs1<-model.matrix(~PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted)
outcome=Interpolation_weighted[,c("peryear","MORT_stat")]
outcome$peryear<-outcome$peryear
outcome$MORT_stat<-as.numeric(outcome$MORT_stat)
outcome$MORT_stat[outcome$MORT_stat==1]<-0
outcome$MORT_stat[outcome$MORT_stat==2]<-1
na.omit(covs0)
outcome<-as.matrix(outcome)
library(tibble)  
covs1=as_tibble(covs1)
covs1<-covs1[,-1] 
covs0=as_tibble(covs0)
covs0<-covs0[,-1] 
x<-IDI.INF(outcome, covs0,covs1,  t0=10, npert=1000) 
IDI_10_ALL<-IDI.INF.OUT(x) 

#CVD
Interpolation_weighted_CVD<-Interpolation_weighted[,c("peryear","PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status","CVD_MORT_stat")]
colnames(Interpolation_weighted_CVD)
Interpolation_weighted_CVD<-na.omit(Interpolation_weighted_CVD)
#covs0<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs0<-model.matrix(~Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted_CVD)
#covs1<-as.matrix(Interpolation_weighted[,c("PD_diagnosis", "PD_diagnosis", "Age", "Gender","Race_ethnicity","BMI","HbA1c","HTN_status","HPL_status")])
covs1<-model.matrix(~PD_diagnosis+Age+Gender+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,Interpolation_weighted_CVD)
outcome=Interpolation_weighted_CVD[,c("peryear","CVD_MORT_stat")]
outcome$peryear<-outcome$peryear
outcome$CVD_MORT_stat<-as.numeric(outcome$CVD_MORT_stat)
outcome$CVD_MORT_stat[outcome$CVD_MORT_stat==1]<-0
outcome$CVD_MORT_stat[outcome$CVD_MORT_stat==2]<-1
na.omit(covs0)
outcome<-as.matrix(outcome)
library(tibble)  
covs1=as_tibble(covs1)
covs1<-covs1[,-1] 
covs0=as_tibble(covs0)
covs0<-covs0[,-1] 
x<-IDI.INF(outcome, covs0,covs1,  t0=10, npert=1000) 
IDI_10_CVD<-IDI.INF.OUT(x) 
IDI_5<-rbind(c(IDI_5_ALL[1,],"all"),c(IDI_5_CVD[1,],"CVD"))
IDI_10<-rbind(c(IDI_10_ALL[1,],"all"),c(IDI_10_CVD[1,],"CVD"))
IDI_5<-as.data.frame(IDI_5)
IDI_5$yaer<-5
IDI_10<-as.data.frame(IDI_10)
IDI_10$yaer<-10
IDI<-rbind(IDI_5,IDI_10)
IDI
write.table(IDI,sep = ",",file ="G:/paper_2_PD&DM/data_DC/IDI.csv")

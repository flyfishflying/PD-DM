# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
  library(caret)
  library(car)
  library(stringr)
  library(cmprsk)
  library(dplyr)
  library(foreign)
  library(ggplot2)
  library(ggsci)
  library(ggrepel)
  library(lava)
  library(Matching)
  library(mediation)
  library(mice)
  library(devtools)
  #install_github("tagteam/riskRegression")
  library(pec)
  #install.packages("poLCA", dependencies = TRUE)
  library(poLCA)
  library(plyr)
  library(dplyr)  
  library(prodlim)
  library(reshape2)
  library(rms)
  library(riskRegression)
  library(survey)
  library(scales)
  library(survminer)
  library(survival)
  library(splines)
  library(timeROC)
  library(tableone)
}
# +++++++++++================================+++++++++++ ####
# +++++++++++============Tables==============+++++++++++ ####
# +++++++++++================================+++++++++++ ####  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Multiple interpolation data (Table 1)  ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
       "BMI_grade","HbA1c_status","Medicine_use")
VAR_ALL<-c("CAL_mean","PPD_mean","Age","Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
           "BMI","BMI_grade","HbA1c","HbA1c_status","Medicine_use")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
Table1<-cbind(Over_all,noPD[,c("Mean","SE")],PD[,c("Mean","SE")])
Table1
save(Table1,file = "G:/paper_2_PD&DM/data_DC/Table1_Rdata")  
{ #** section 19.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "G:/paper_2_PD&DM/data_DC/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
colnames(Table1)<-c("Covariates","grade","Mean_all","SE_all",
                    "Mean_noPD","SE_noPD",
                    "Mean_PD","SE_PD","P.value")
Table1$Covariates<-factor(Table1$Covariates,levels = unique(Over_all$Covariates))
Table1<-Table1[order(Table1[,1]),]

write.table(Table1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table1.csv")
table(Interpolation_weighted$PD_diagnosis)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 All-cause and cause-specific mortality (Table 2) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  Table2<-cbind(head,row1,row2,row3)
  write.table(Table2,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table2.csv",row.names = F)
  
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 Stratified analyses (Table 3) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* AGE #####
  rhcSvy_AGE_30<-subset(rhcSvy,Age_grade=="<40")
  rhcSvy_AGE_40<-subset(rhcSvy,Age_grade=="[40,50)")
  rhcSvy_AGE_50<-subset(rhcSvy,Age_grade=="[50,60)")
  rhcSvy_AGE_60<-subset(rhcSvy,Age_grade=="[60,70)")
  rhcSvy_AGE_70<-subset(rhcSvy,Age_grade=="[70,80)")
  rhcSvy_AGE_80<-subset(rhcSvy,Age_grade==">=80")
  ##* ALL cause #####
  #AGE 30
  MODEL_30_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_30)
  Age30<-summary(MODEL_30_AGE)
  Age30
  P<-Age30[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age30[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="<40",'status'="all cause")
  #AGE 40
  MODEL_40_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_40)
  Age40<-summary(MODEL_40_AGE)
  Age40
  P<-Age40[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age40[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[40,50)",'status'="all cause")
  #AGE 50
  MODEL_50_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_50)
  Age50<-summary(MODEL_50_AGE)
  Age50
  P<-Age50[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age50[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[50,60)",'status'="all cause")
  #AGE 60
  MODEL_60_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_60)
  Age60<-summary(MODEL_60_AGE)
  P<-Age60[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age60[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[60,70)",'status'="all cause")
  #AGE 70
  MODEL_70_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_70)
  Age70<-summary(MODEL_70_AGE)
  Age70
  P<-Age70[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age70[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age5 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[70,80)",'status'="all cause")
  #AGE 80
  MODEL_80_AGE<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_80)
  Age80<-summary(MODEL_80_AGE)
  Age80
  P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age6 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"=">=80",'status'="all cause")
  
  Age.ALL<-rbind(Age1,Age2,Age3,Age4,Age5,Age6)
  Age.ALL
  ##* CVD-related ####
  #AGE 30
  MODEL_30_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_30)
  Age30<-summary(MODEL_30_AGE)
  Age30
  P<-Age30[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age30[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="<40",'status'="CVD cause")
  #AGE 40
  MODEL_40_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_40)
  Age40<-summary(MODEL_40_AGE)
  Age40
  P<-Age40[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age40[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[40,50)",'status'="CVD cause")
  #AGE 50
  MODEL_50_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_50)
  Age50<-summary(MODEL_50_AGE)
  Age50
  P<-Age50[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age50[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[50,60)",'status'="CVD cause")
  #AGE 60
  MODEL_60_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_60)
  Age60<-summary(MODEL_60_AGE)
  P<-Age60[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age60[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[60,70)",'status'="CVD cause")
  #AGE 70
  MODEL_70_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_70)
  Age70<-summary(MODEL_70_AGE)
  Age70
  P<-Age70[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age70[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age5 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"="[70,80)",'status'="CVD cause")
  #AGE 80
  MODEL_80_AGE<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_AGE_80)
  Age80<-summary(MODEL_80_AGE)
  Age80
  P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Age6 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Age","group"=">=80",'status'="CVD cause")
  
  Age.CVD<-rbind(Age1,Age2,Age3,Age4,Age5,Age6)
  Age.LAST<-cbind(Age.ALL,Age.CVD)
  Age.LAST
}
{#* Sex #####
  rhcSvy_Female<-subset(rhcSvy,Sex=="Female")
  rhcSvy_Male<-subset(rhcSvy,Sex=="Male")
  
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_Female)
  Racewhite<-summary(MODEL_ALL_inter)
  P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Sex1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="Sex","group"="Female",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_Male)
  Racewhite<-summary(MODEL_ALL_inter)
  P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Sex2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="Sex","group"="Male",'status'="all cause")
  Sex.ALL<-rbind(Sex1,Sex2)
  ##* CVD-related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_Female)
  Racewhite<-summary(MODEL_ALL_inter)
  P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Sex3<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Sex","group"="Female",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_Male)
  Sexwhite<-summary(MODEL_ALL_inter)
  P<-Sexwhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(Sexwhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Sex4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Sex","group"="Male",'status'="CVD cause")
  Sex.CVD<-rbind(Sex3,Sex4)
  Sex.LAST<-cbind(Sex.ALL,Sex.CVD)
  Sex.LAST
}  

{ #* RACE #####
  rhcSvy_White<-subset(rhcSvy,Race_ethnicity=="Non-Hispanic White")
  rhcSvy_Black<-subset(rhcSvy,Race_ethnicity=="Non-Hispanic Black")
  rhcSvy_Hispanic<-subset(rhcSvy,Race_ethnicity=="Hispanic")
  rhcSvy_Other<-subset(rhcSvy,Race_ethnicity=="Other Race")
  
  ##* ALL cause ####
  # white
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_White)
  RaceWhite<-summary(MODEL_ALL_inter)
  P<-RaceWhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceWhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="White",'status'="all cause")
  Race1
  #black
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Black)
  RaceBlack<-summary(MODEL_ALL_inter)
  P<-RaceBlack[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceBlack[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Black",'status'="all cause")
  Race2
  #Hispanic
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Hispanic)
  RaceHispanic<-summary(MODEL_ALL_inter)
  P<-RaceHispanic[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceHispanic[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Hispanic",'status'="all cause")
  Race3
  #others
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Other)
  RaceOther<-summary(MODEL_ALL_inter)
  P<-RaceOther[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceOther[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Other",'status'="all cause")
  Race4
  Race.ALL<-rbind(Race1,Race2,Race3,Race4)
  Race.ALL
  
  ##* CVD-related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_White)
  Racewhite<-summary(MODEL_ALL_inter)
  P<-RaceWhite[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceWhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="White",'status'="all cause")
  Race1
  #black
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Black)
  RaceBlack<-summary(MODEL_ALL_inter)
  P<-RaceBlack[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceBlack[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Black",'status'="all cause")
  Race2
  #Hispanic
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Hispanic)
  RaceHispanic<-summary(MODEL_ALL_inter)
  P<-RaceHispanic[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceHispanic[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Hispanic",'status'="all cause")
  Race3
  #others
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+
                              SES+Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy_Other)
  RaceOther<-summary(MODEL_ALL_inter)
  P<-RaceOther[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(RaceOther[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Race4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Race","group"="Other",'status'="all cause")
  Race4
  Race.CVD<-rbind(Race1,Race2,Race3,Race4)
  Race.LAST<-cbind(Race.ALL,Race.CVD)
  Race.LAST
}
{#* SES #####
  table(Interpolation_weighted$SES)
  rhcSvy_low<-subset(rhcSvy,SES=="low")
  rhcSvy_medium<-subset(rhcSvy,SES=="medium")
  rhcSvy_high<-subset(rhcSvy,SES=="high")
  
  ##* ALL cause ####
  #low
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_low)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="low",'status'="all cause")
  SES1
  #middle
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_medium)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="middle",'status'="all cause")
  SES2
  #High
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_high)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="high",'status'="all cause")
  SES3
  SES.ALL<-rbind(SES1,SES2,SES3)
  ##* CVD related ####
  #low
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_low)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="low",'status'="CVD cause")
  SES1
  #middle
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_medium)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="middle",'status'="CVD cause")
  SES2
  #High
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              BMI_grade+Drinking_status+HEI+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_high)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  SES3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="SES","group"="high",'status'="CVD cause")
  SES3
  SES.CVD<-rbind(SES1,SES2,SES3)
  SES.LAST<-cbind(SES.ALL,SES.CVD)
  SES.LAST
}


{#* Smoking #####
  table(Interpolation_weighted$Smoking_status)
  rhcSvy_Current<-subset(rhcSvy,Smoking_status=="Current smoker")
  rhcSvy_Former<-subset(rhcSvy,Smoking_status=="Former smoker")
  rhcSvy_Never<-subset(rhcSvy,Smoking_status=="Never smoker")
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Current)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Current",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Former)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Former",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Never)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Never",'status'="all cause")
  Smoke.ALL<-rbind(Smoke1,Smoke2,Smoke3)
  Smoke.ALL
  
  ##* CVD related####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Current)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Current",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Former)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Former",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Never)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Smoke3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Smoke","group"="Never",'status'="CVD cause")
  Smoke.CVD<-rbind(Smoke1,Smoke2,Smoke3)
  Smoke.LAST<-cbind(Smoke.ALL,Smoke.CVD)
  Smoke.LAST
}
{#* Drinking #####
  table(Interpolation_weighted$Drinking_status)
  rhcSvy_Heavier<-subset(rhcSvy,Drinking_status=="Heavier drinker")
  rhcSvy_Light<-subset(rhcSvy,Drinking_status=="Light/moderate drinker")
  rhcSvy_Non<-subset(rhcSvy,Drinking_status=="Non-drinker")
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Heavier)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Heavier",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Light)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Light",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Non)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Non",'status'="all cause")
  Drink.ALL<-rbind(Drink1,Drink2,Drink3)
  Drink.ALL
  
  ##* CVD related####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Heavier)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Heavier",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Light)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Light",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Smoking_status+HEI+Medicine_use+HbA1c_status, design =rhcSvy_Non)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drink3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="Drink","group"="Non",'status'="CVD cause")
  Drink.CVD<-rbind(Drink1,Drink2,Drink3)
  Drink.LAST<-cbind(Drink.ALL,Drink.CVD)
  Drink.LAST
}

{#*HEI #####
  table(Interpolation_weighted$HEI)
  rhcSvy_HEI1<-subset(rhcSvy,HEI=="Quintile 1")
  rhcSvy_HEI2<-subset(rhcSvy,HEI=="Quintile 2")
  rhcSvy_HEI3<-subset(rhcSvy,HEI=="Quintile 3")
  rhcSvy_HEI4<-subset(rhcSvy,HEI=="Quintile 4")
  rhcSvy_HEI5<-subset(rhcSvy,HEI=="Quintile 5")
  ##* ALL cause ####
  # Quintile 1
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 1",'status'="all cause")
  # Quintile 2
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 3",'status'="all cause")
  # Quintile 3
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI3)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 3",'status'="all cause")
  # Quintile 4
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI4)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 4",'status'="all cause")
  # Quintile 5
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI5)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub5 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 5",'status'="all cause")
  HEI.ALL<-rbind(HEI_sub1,HEI_sub2,HEI_sub3,HEI_sub4,HEI_sub5)
  HEI.ALL
  ##* CVD related ####
  # Quintile 1
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 1",'status'="CVD cause")
  # Quintile 2
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 3",'status'="CVD cause")
  # Quintile 3
  MODEL_ALL_inter<-svycoxph(Surv(peryear,CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI3)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 3",'status'="CVD cause")
  # Quintile 4
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI4)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 4",'status'="CVD cause")
  # Quintile 5
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HbA1c_status, design =rhcSvy_HEI5)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HEI_sub5 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'subgroup'="HEI","group"="Quintile 5",'status'="CVD cause")
  HEI.CVD<-rbind(HEI_sub1,HEI_sub2,HEI_sub3,HEI_sub4,HEI_sub5)
  HEI.LAST<-cbind(HEI.ALL,HEI.CVD)
  HEI.LAST
}
{#* BMI #####
  table(Interpolation_weighted$BMI_grade)
  rhcSvy_low<-subset(rhcSvy,BMI_grade=="(0,25)")
  rhcSvy_middle<-subset(rhcSvy,BMI_grade=="[25.0-30)")
  rhcSvy_high<-subset(rhcSvy,BMI_grade=="[30,inf)")
  ##* ALL cause ####
  #low
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_low)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="low",'status'="all cause")
  #middle
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_middle)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="middle",'status'="all cause")
  #high
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_high)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="high",'status'="all cause")
  BMI.ALL<-rbind(BMI1,BMI2,BMI3)
  
  ##* CVD related ####
  #low
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_low)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="low",'status'="CVD cause")
  #middle
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_middle)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="middle",'status'="CVD cause")
  #high
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+Smoking_status+Drinking_status+HEI+Medicine_use+HbA1c_status
                            , design =rhcSvy_high)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  BMI3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="high",'status'="CVD cause")
  BMI.CVD<-rbind(BMI1,BMI2,BMI3)
  BMI.LAST<-cbind(BMI.ALL,BMI.CVD)
  BMI.LAST
}
{#*Medicine #####
  table(Interpolation_weighted$Medicine_use)
  rhcSvy_Medicine1<-subset(rhcSvy,Medicine_use=="NO")
  rhcSvy_Medicine2<-subset(rhcSvy,Medicine_use=="YES")
  
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+HEI+HbA1c_status, design =rhcSvy_Medicine1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Medicine_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'subgroup'="Medicine_sub","group"="No",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+HEI+HbA1c_status, design =rhcSvy_Medicine2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Medicine_sub2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'subgroup'="Medicine_sub","group"="YES",'status'="all cause")
  Medicine.ALL<-rbind(Medicine_sub1,Medicine_sub2)
  
  ##* CVD related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+HEI+HbA1c_status, design =rhcSvy_Medicine1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Medicine_sub3<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Medicine_sub","group"="NO",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+HEI+HbA1c_status, design =rhcSvy_Medicine2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Medicine_sub4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Medicine_sub","group"="YES",'status'="CVD cause")
  Medicine.CVD<-rbind(Medicine_sub3,Medicine_sub4)
  Medicine.LAST<-cbind(Medicine.ALL,Medicine.CVD)
  Medicine.LAST
}
{#*HbA1c #####
  table(Interpolation_weighted$HbA1c_status)
  rhcSvy_HbA1c1<-subset(rhcSvy,HbA1c_status=="HbA1c<7")
  rhcSvy_HbA1c2<-subset(rhcSvy,HbA1c_status=="HbA1c>=7")
  
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HEI, design =rhcSvy_HbA1c1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HbA1c_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_sub","group"="HbA1c<7",'status'="all cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HEI, design =rhcSvy_HbA1c2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HbA1c_sub2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_sub","group"="HbA1c>=7",'status'="all cause")
  HbA1c.ALL<-rbind(HbA1c_sub1,HbA1c_sub2)
  HbA1c.ALL
  
  ##* CVD related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HEI, design =rhcSvy_HbA1c1)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HbA1c_sub3<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="HbA1c_sub","group"="HbA1c<7",'status'="CVD cause")
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Race_ethnicity+Sex+
                              SES+BMI_grade+Drinking_status+Smoking_status+Medicine_use+HEI, design =rhcSvy_HbA1c2)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  HbA1c_sub4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'subgroup'="HbA1c_sub","group"="HbA1c>=7",'status'="CVD cause")
  HbA1c.CVD<-rbind(HbA1c_sub3,HbA1c_sub4)
  HbA1c.LAST<-cbind(HbA1c.ALL,HbA1c.CVD)
  HbA1c.LAST
}
all.subgroup<-rbind(Age.LAST,Race.LAST,Sex.LAST,BMI.LAST,Smoke.LAST,Drink.LAST,
                    SES.LAST,HEI.LAST,Medicine.LAST,HbA1c.LAST)

Subgroup<-all.subgroup[,c(5,6,1:4,8:11)]
colnames(Subgroup)<-c("subgroup","group","HR_ALL","lower_ALL","upper_ALL","P_ALL",
                      "HR_CVD","lower_CVD","upper_CVD","P_CVD")

Subgroup$HR_ALL<-as.character(round(Subgroup$HR_ALL,2))
Subgroup$HR_CVD<-as.character(round(Subgroup$HR_CVD,2))
Subgroup$lower_ALL<-as.character(round(Subgroup$lower_ALL,2))
Subgroup$lower_CVD<-as.character(round(Subgroup$lower_CVD,2))
Subgroup$upper_ALL<-as.character(round(Subgroup$upper_ALL,2))
Subgroup$upper_CVD<-as.character(round(Subgroup$upper_CVD,2))
Subgroup$P_ALL<-as.character(round(Subgroup$P_ALL,3))
Subgroup$P_CVD<-as.character(round(Subgroup$P_CVD,3))
Subgroup$a<-" ("
Subgroup$b<-", "
Subgroup$c<-")"

Subgroup$HR_ALL<-str_pad(Subgroup[,c('HR_ALL')],width = 4,side = "right",pad = "0")
Subgroup$lower_ALL<-str_pad(Subgroup[,c('lower_ALL')],width = 4,side = "right",pad = "0")
Subgroup$upper_ALL<-str_pad(Subgroup[,c('upper_ALL')],width = 4,side = "right",pad = "0")
Subgroup$HR_CVD<-str_pad(Subgroup[,c('HR_CVD')],width = 4,side = "right",pad = "0")
Subgroup$lower_CVD<-str_pad(Subgroup[,c('lower_CVD')],width = 4,side = "right",pad = "0")
Subgroup$upper_CVD<-str_pad(Subgroup[,c('upper_CVD')],width = 4,side = "right",pad = "0")
Subgroup$P_ALL<-str_pad(Subgroup[,c('P_ALL')],width = 5,side = "right",pad = "0")
Subgroup$P_CVD<-str_pad(Subgroup[,c('P_CVD')],width = 5,side = "right",pad = "0")
Subgroup[,c('P_ALL','P_CVD')][Subgroup[,c('P_ALL','P_CVD')]=="00000"]<-"<0.001"
Subgroup[Subgroup=="1000"]<-"1.00"
Subgroup[Subgroup=="2000"]<-"2.00"
Subgroup
Subgroup$HR_95_ALL<-paste(Subgroup$HR_ALL,Subgroup$a,Subgroup$lower_ALL,Subgroup$b<-", ",Subgroup$upper_ALL,Subgroup$c,sep = "")
Subgroup$HR_95_CVD<-paste(Subgroup$HR_CVD,Subgroup$a,Subgroup$lower_CVD,Subgroup$b<-", ",Subgroup$upper_CVD,Subgroup$c,sep = "")
Subgroup.LAST<-Subgroup[,c('subgroup','group','HR_95_ALL','P_ALL','HR_95_CVD','P_CVD')]
Table3<-Subgroup.LAST


write.table(Table3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table3.csv")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative mortality rates (Table S1) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
#PD_DM
PD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
PD_DM$year<-PD_DM$peryear
PD_DM$Pyear<-PD_DM$year*PD_DM$weight
PD_DM_death_all<-PD_DM[which(PD_DM$MORT_stat==1),]
PD_DM_death_CVD<-PD_DM[which(PD_DM$CVD_MORT_stat==1),]
PD_DM_death_Cancer<-PD_DM[which(PD_DM$Cancer_MORT_stat==1),]
PD_DM_death_DM<-PD_DM[which(PD_DM$DM_MORT_stat==1),]
PD_DM_Perseon_year_un<-sum(PD_DM$year)
PD_DM_Perseon_year_ad<-sum(PD_DM$Pyear)
PD_DM_Perseon<-sum(PD_DM$weight)
PD_DM_Perseon_un_ALL<-as.numeric(summary(PD_DM$MORT_stat==1)[3])
PD_DM_Perseon_un_CVD<-as.numeric(summary(PD_DM$CVD_MORT_stat==1)[3])
PD_DM_Perseon_un_Cancer<-as.numeric(summary(PD_DM$Cancer_MORT_stat==1)[3])
PD_DM_Perseon_un_DM<-as.numeric(summary(PD_DM$DM_MORT_stat==1)[3])

#noPD_DM
noPD_DM<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
noPD_DM$year<-noPD_DM$peryear
noPD_DM$Pyear<-noPD_DM$year*noPD_DM$weight
noPD_DM_death_all<-noPD_DM[which(noPD_DM$MORT_stat==1),]
noPD_DM_death_CVD<-noPD_DM[which(noPD_DM$CVD_MORT_stat==1),]
noPD_DM_death_Cancer<-noPD_DM[which(noPD_DM$Cancer_MORT_stat==1),]
noPD_DM_death_DM<-noPD_DM[which(noPD_DM$DM_MORT_stat==1),]
noPD_DM_Perseon_year_un<-sum(noPD_DM$year)
noPD_DM_Perseon_year_ad<-sum(noPD_DM$Pyear)
noPD_DM_Perseon<-sum(noPD_DM$weight)
noPD_DM_Perseon_un_ALL<-as.numeric(summary(noPD_DM$MORT_stat==1)[3])
noPD_DM_Perseon_un_CVD<-as.numeric(summary(noPD_DM$CVD_MORT_stat==1)[3])
noPD_DM_Perseon_un_Cancer<-as.numeric(summary(noPD_DM$Cancer_MORT_stat==1)[3])
noPD_DM_Perseon_un_DM<-as.numeric(summary(noPD_DM$DM_MORT_stat==1)[3])

#PD_ALL
PD_ALL<-PD_DM_Perseon_un_ALL*(1000/PD_DM_Perseon_year_un)
PD_ALL_UCL<-(PD_DM_Perseon_un_ALL+(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_LCL<-(PD_DM_Perseon_un_ALL-(1.96*sqrt(PD_DM_Perseon_un_ALL)))*(1000/PD_DM_Perseon_year_un)
PD_ALL_Incidence<-paste0(round(PD_ALL,2)," (",round(PD_ALL_LCL,2),"-",round(PD_ALL_UCL,2),")")
PD_ALL_Incidence
#PD_CVD
PD_CVD<-PD_DM_Perseon_un_CVD*(1000/PD_DM_Perseon_year_un)
PD_CVD_UCL<-(PD_DM_Perseon_un_CVD+(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_LCL<-(PD_DM_Perseon_un_CVD-(1.96*sqrt(PD_DM_Perseon_un_CVD)))*(1000/PD_DM_Perseon_year_un)
PD_CVD_Incidence<-paste0(round(PD_CVD,2)," (",round(PD_CVD_LCL,2),"-",round(PD_CVD_UCL,2),")")
PD_CVD_Incidence
#PD_Cancer
PD_Cancer<-PD_DM_Perseon_un_Cancer*(1000/PD_DM_Perseon_year_un)
PD_Cancer_UCL<-(PD_DM_Perseon_un_Cancer+(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_LCL<-(PD_DM_Perseon_un_Cancer-(1.96*sqrt(PD_DM_Perseon_un_Cancer)))*(1000/PD_DM_Perseon_year_un)
PD_Cancer_Incidence<-paste0(round(PD_Cancer,2)," (",round(PD_Cancer_LCL,2),"-",round(PD_Cancer_UCL,2),")")
PD_Cancer_Incidence
#PD_DM
PD_DM<-PD_DM_Perseon_un_DM*(1000/PD_DM_Perseon_year_un)
PD_DM_UCL<-(PD_DM_Perseon_un_DM+(1.96*sqrt(PD_DM_Perseon_un_DM)))*(1000/PD_DM_Perseon_year_un)
PD_DM_LCL<-(PD_DM_Perseon_un_DM-(1.96*sqrt(PD_DM_Perseon_un_DM)))*(1000/PD_DM_Perseon_year_un)
PD_DM_Incidence<-paste0(round(PD_DM,2)," (",round(PD_DM_LCL,2),"-",round(PD_DM_UCL,2),")")


#noPD_ALL
noPD_ALL<-noPD_DM_Perseon_un_ALL*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_UCL<-(noPD_DM_Perseon_un_ALL+(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_LCL<-(noPD_DM_Perseon_un_ALL-(1.96*sqrt(noPD_DM_Perseon_un_ALL)))*(1000/noPD_DM_Perseon_year_un)
noPD_ALL_Incidence<-paste0(round(noPD_ALL,2)," (",round(noPD_ALL_LCL,2),"-",round(noPD_ALL_UCL,2),")")

#noPD_CVD
noPD_CVD<-noPD_DM_Perseon_un_CVD*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_UCL<-(noPD_DM_Perseon_un_CVD+(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_LCL<-(noPD_DM_Perseon_un_CVD-(1.96*sqrt(noPD_DM_Perseon_un_CVD)))*(1000/noPD_DM_Perseon_year_un)
noPD_CVD_Incidence<-paste0(round(noPD_CVD,2)," (",round(noPD_CVD_LCL,2),"-",round(noPD_CVD_UCL,2),")")
#noPD_Cancer
noPD_Cancer<-noPD_DM_Perseon_un_Cancer*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_UCL<-(noPD_DM_Perseon_un_Cancer+(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_LCL<-(noPD_DM_Perseon_un_Cancer-(1.96*sqrt(noPD_DM_Perseon_un_Cancer)))*(1000/noPD_DM_Perseon_year_un)
noPD_Cancer_Incidence<-paste0(round(noPD_Cancer,2)," (",round(noPD_Cancer_LCL,2),"-",round(noPD_Cancer_UCL,2),")")
#PD_DM
noPD_DM<-noPD_DM_Perseon_un_DM*(1000/noPD_DM_Perseon_year_un)
noPD_DM_UCL<-(noPD_DM_Perseon_un_DM+(1.96*sqrt(noPD_DM_Perseon_un_DM)))*(1000/noPD_DM_Perseon_year_un)
noPD_DM_LCL<-(noPD_DM_Perseon_un_DM-(1.96*sqrt(noPD_DM_Perseon_un_DM)))*(1000/noPD_DM_Perseon_year_un)
noPD_DM_Incidence<-paste0(round(noPD_DM,2)," (",round(noPD_DM_LCL,2),"-",round(noPD_DM_UCL,2),")")

#EVENTS
#all.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#Cancer.cause
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#DM.cause
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
total.counts

TableS1_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS1_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N", "Incidence Rate (95% CI)")
TableS1_3<-c("All-cause mortality",total.counts[1,1],noPD_ALL_Incidence,total.counts[1,2],PD_ALL_Incidence)
TableS1_4<-c("CVD mortality",total.counts[2,1],noPD_CVD_Incidence,total.counts[2,2],PD_CVD_Incidence)
TableS1_5<-c("Cancer mortality",total.counts[3,1],noPD_Cancer_Incidence,total.counts[3,2],PD_Cancer_Incidence)
TableS1_6<-c("DM mortality",total.counts[4,1],noPD_DM_Incidence,total.counts[2,2],PD_DM_Incidence)
TableS1<-as.data.frame(rbind(TableS1_1,TableS1_2,TableS1_3,TableS1_4,TableS1_5,TableS1_6))
TableS1
write.table(TableS1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS1.csv")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 dose -response effect (Table S2) #### 
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$Periodontitis_diagnosis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-result3
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3.CVD <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-result3.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3.Cancer <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-result3.Cancer
}

{ #* DM model #####
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_DM_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-result3
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  
  write.table(TableS2,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS2.csv")
}
Interpolation_weighted$Periodontitis_diagnosis<-as.character(Interpolation_weighted$Periodontitis_diagnosis)
table(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis_diagnosis[Interpolation_weighted$Periodontitis_diagnosis=="normal"]<-0
Interpolation_weighted$Periodontitis_diagnosis[Interpolation_weighted$Periodontitis_diagnosis=="mild"]<-1
Interpolation_weighted$Periodontitis_diagnosis[Interpolation_weighted$Periodontitis_diagnosis=="moderate"]<-2
Interpolation_weighted$Periodontitis_diagnosis[Interpolation_weighted$Periodontitis_diagnosis=="severe"]<-3
Interpolation_weighted$Periodontitis_diagnosis<-as.numeric(Interpolation_weighted$Periodontitis_diagnosis)

{#* P for trend ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  { #* all model #####
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-result3
    result.all
  }
  
  { #* CVD model #####
    rhcSvy_DM_CVD<-rhcSvy
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
    model3_CVD_result<-summary(model3_CVD)
    P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model3",'status'="CVD cause")
    result.CVD<-result3.CVD
    result.CVD
    
  }
  
  { #* Cancer model #####
    rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
    svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
    #Cancer model2
    model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                              Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,design =rhcSvy_DM_Cancer)
    model3_Cancer_result<-summary(model3_Cancer)
    P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                 'P value' =P,'model'="model3",'status'="Cancer cause")
    result.Cancer<-result3.Cancer
    result.Cancer
  }
  
  { #* DM model #####
    model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                          Periodontitis_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                          Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
    model3_DM_result<-summary(model3_DM)
    
    P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="DM cause")
    result.DM<-result3
    result.DM
  }
  
  { #* Combine #####
    result.trend.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
    result.trend.all.cause$HR<-round(result.trend.all.cause$HR,2)
    result.trend.all.cause$lower..95<-round(result.trend.all.cause$lower..95,2)
    result.trend.all.cause$upper..95<-round(result.trend.all.cause$upper..95,2)
    result.trend.all.cause$P.value<-round(result.trend.all.cause$P.value,3)
    TableS2b<-result.trend.all.cause
    write.table(TableS2b,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS2b.csv")
  }
}

table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$MORT_stat)

table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$CVD_MORT_stat)

table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$Cancer_MORT_stat)


table(Interpolation_weighted$Periodontitis_diagnosis,Interpolation_weighted$DM_MORT_stat)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24  Standard risk factors characteristics  (Table S3) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("Sex","Race_ethnicity","HPL_status","HTN_status")
VAR_ALL<-c("Age","Sex","Race_ethnicity","BMI","HbA1c","HPL_status","HTN_status")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
TableS3<-cbind(Over_all,noPD[,c("Mean","SE")],PD[,c("Mean","SE")])
TableS3
save(TableS3,file = "G:/paper_2_PD&DM/data_DC/TableS3_Rdata")  
{ #** section 19.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "G:/paper_2_PD&DM/data_DC/TableS3_Rdata")
TableS3<-merge(TableS3,test_data,by="Covariates",all.x = T)
colnames(TableS3)<-c("Covariates","grade","Mean_all","SE_all",
                    "Mean_noPD","SE_noPD",
                    "Mean_PD","SE_PD","P.value")
TableS3$Covariates<-factor(TableS3$Covariates,levels = unique(Over_all$Covariates))
TableS3<-TableS3[order(TableS3[,1]),]

write.table(TableS3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS3.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 Sensitivity Analysis 1 CAL (Table S4-1) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        CAL_mean, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        CAL_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        CAL_mean+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS4_1<-cbind(head,row1,row2,row3)
  TableS4_1
  write.table(TableS4_1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS4_1.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 Sensitivity Analysis 1 PPD (Table S4-2) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PPD_mean, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PPD_mean+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PPD_mean+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS4_2<-cbind(head,row1,row2,row3)
  TableS4_2
  write.table(TableS4_2,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS4_2.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 peryear>=2 (Table S5) ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
Interpolation_weighted<-subset(Interpolation_weighted,peryear>=2)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS5<-cbind(head,row1,row2,row3)
  write.table(TableS5,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS5.csv",row.names = F)
} 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 omitted cancer and CVD patients (Table S6) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
table(Interpolation_weighted$CVD_status)
Interpolation_weighted<-subset(Interpolation_weighted,CVD_status=="NO"&Cancer_status=="NO")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS6<-cbind(head,row1,row2,row3)
  write.table(TableS6,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS6.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Original data (Table S7)  ####
load(file="G:/paper_2_PD&DM/data_DC/Original_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Original_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
       "BMI_grade","HbA1c_status","Medicine_use")
VAR_ALL<-c("CAL_mean","PPD_mean","Age","Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
           "BMI","BMI_grade","HbA1c","HbA1c_status","Medicine_use")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.2 No/Mild  ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
TableS7<-cbind(Over_all,noPD[,c("Mean","SE")],PD[,c("Mean","SE")])
TableS7
save(TableS7,file = "G:/paper_2_PD&DM/data_DC/TableS7_Rdata")  
{ #** section 19.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "G:/paper_2_PD&DM/data_DC/TableS7_Rdata")
TableS7<-merge(TableS7,test_data,by="Covariates",all.x = T)
colnames(TableS7)<-c("Covariates","grade","Mean_all","SE_all",
                    "Mean_noPD","SE_noPD",
                    "Mean_PD","SE_PD","P.value")
TableS7$Covariates<-factor(TableS7$Covariates,levels = unique(Over_all$Covariates))
TableS7<-TableS7[order(TableS7[,1]),]
write.table(TableS7,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS7.csv")  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Original data HRs (Table S8) ####
load(file="G:/paper_2_PD&DM/data_DC/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS8<-cbind(head,row1,row2,row3)
  write.table(TableS8,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS8.csv")
}



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 31 Complete data (Table S9)   ####
load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Complete_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
       "BMI_grade","HbA1c_status","Medicine_use")
VAR_ALL<-c("CAL_mean","PPD_mean","Age","Age_grade","Sex","Race_ethnicity","SES","Smoking_status","Drinking_status","HEI",
           "BMI","BMI_grade","HbA1c","HbA1c_status","Medicine_use")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  Over_all<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR_ALL, model))
}  
{ #** section 19.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
TableS9<-cbind(Over_all,noPD[,c("Mean","SE")],PD[,c("Mean","SE")])
TableS9
save(TableS9,file = "G:/paper_2_PD&DM/data_DC/TableS9_Rdata")  
{ #** section 19.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR_ALL, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "G:/paper_2_PD&DM/data_DC/TableS9_Rdata")
TableS9<-merge(TableS9,test_data,by="Covariates",all.x = T)
colnames(TableS9)<-c("Covariates","grade","Mean_all","SE_all",
                    "Mean_noPD","SE_noPD",
                    "Mean_PD","SE_PD","P.value")
TableS9$Covariates<-factor(TableS9$Covariates,levels = unique(Over_all$Covariates))
TableS9<-TableS9[order(TableS9[,1]),]
write.table(TableS9,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS9.csv")  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 32 Complete data HRs (Table S10) ####
load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS10<-cbind(head,row1,row2,row3)
  write.table(TableS10,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S10.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 32 T2D with 3 diagnose (Table S11) ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/DM_new.Rdata")
options(survey.lonely.psu="adjust")
Interpolation_weighted<-merge(Interpolation_weighted,DM[,c("ID","T2D_3")],by = "ID",all.x = T)
Interpolation_weighted<-subset(Interpolation_weighted,T2D_3=="T2D")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  #all model3
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  #CVD model3
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_CVD)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  #Cancer model3
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer)
  model4_Cancer_result<-summary(model4_Cancer)
  
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  model4_DM_result<-summary(model4_DM)
  
  P<-model4_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
  
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS11<-cbind(head,row1,row2,row3)
  write.table(TableS11,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S11.csv")
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 32  C (Table S12 ) ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
Interpolation_weighted<-subset(Interpolation_weighted,peryear>0)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis, design =rhcSvy,model=T, x=T,
                         y=T, dist="weibull")
  model1_all_result<-summary(model1_all)
  model1_all_result
  HR<-exp(model1_all_result[["coefficients"]][2])
  lower<-HR-1.96*model1_all_result[["table"]][,2][2]
  upper<-HR+1.96*model1_all_result[["table"]][,2][2]
  P<-model1_all_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy,dist="weibull")
  model2_all_result<-summary(model2_all)
  model2_all_result
  HR<-exp(model2_all_result[["coefficients"]][2])
  lower<-HR-1.96*model2_all_result[["table"]][,2][2]
  upper<-HR+1.96*model2_all_result[["table"]][,2][2]
  P<-model2_all_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="All cause")
  result.all<-rbind(result,result2)
  result.all
  #all model2
  model3_all<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy,dist="weibull")
  model3_all_result<-summary(model3_all)
  HR<-exp(model3_all_result[["coefficients"]][2])
  lower<-HR-1.96*model3_all_result[["table"]][,2][2]
  upper<-HR+1.96*model3_all_result[["table"]][,2][2]
  P<-model3_all_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model3
  model4_all<-svysurvreg(Surv(peryear, MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy,dist="weibull")
  model4_all_result<-summary(model4_all)
  HR<-exp(model4_all_result[["coefficients"]][2])
  lower<-HR-1.96*model4_all_result[["table"]][,2][2]
  upper<-HR+1.96*model4_all_result[["table"]][,2][2]
  P<-model4_all_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-rhcSvy
  #CVD Crude
  model1_CVD<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis, design = rhcSvy_DM_CVD, dist="weibull")
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  HR<-exp(model1_CVD_result[["coefficients"]][2])
  lower<-HR-1.96*model1_CVD_result[["table"]][,2][2]
  upper<-HR+1.96*model1_CVD_result[["table"]][,2][2]
  P<-model1_CVD_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design = rhcSvy_DM_CVD,dist="weibull")
  model2_CVD_result<-summary(model2_CVD)
  model2_CVD_result
  HR<-exp(model2_CVD_result[["coefficients"]][2])
  lower<-HR-1.96*model2_CVD_result[["table"]][,2][2]
  upper<-HR+1.96*model2_CVD_result[["table"]][,2][2]
  P<-model2_CVD_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result,result2)
  result.CVD
  #CVD model2
  model3_CVD<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade, design = rhcSvy_DM_CVD,dist="weibull")
  model3_CVD_result<-summary(model3_CVD)
  HR<-exp(model3_CVD_result[["coefficients"]][2])
  lower<-HR-1.96*model3_CVD_result[["table"]][,2][2]
  upper<-HR+1.96*model3_CVD_result[["table"]][,2][2]
  P<-model3_CVD_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model3
  model4_CVD<-svysurvreg(Surv(peryear, CVD_MORT_stat==1) ~
                           PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design = rhcSvy_DM_CVD,dist="weibull")
  model4_CVD_result<-summary(model4_CVD)
  HR<-exp(model4_CVD_result[["coefficients"]][2])
  lower<-HR-1.96*model4_CVD_result[["table"]][,2][2]
  upper<-HR+1.96*model4_CVD_result[["table"]][,2][2]
  P<-model4_CVD_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-rhcSvy
  #Cancer Crude
  model1_Cancer<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis, design =rhcSvy_DM_Cancer, dist="weibull")
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  HR<-exp(model1_Cancer_result[["coefficients"]][2])
  lower<-HR-1.96*model1_Cancer_result[["table"]][,2][2]
  upper<-HR+1.96*model1_Cancer_result[["table"]][,2][2]
  P<-model1_Cancer_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES, design =rhcSvy_DM_Cancer,dist="weibull")
  model2_Cancer_result<-summary(model2_Cancer)
  model2_Cancer_result
  HR<-exp(model2_Cancer_result[["coefficients"]][2])
  lower<-HR-1.96*model2_Cancer_result[["table"]][,2][2]
  upper<-HR+1.96*model2_Cancer_result[["table"]][,2][2]
  P<-model2_Cancer_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result,result2)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade, design =rhcSvy_DM_Cancer,dist="weibull")
  model3_Cancer_result<-summary(model3_Cancer)
  HR<-exp(model3_Cancer_result[["coefficients"]][2])
  lower<-HR-1.96*model3_Cancer_result[["table"]][,2][2]
  upper<-HR+1.96*model3_Cancer_result[["table"]][,2][2]
  P<-model3_Cancer_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  #Cancer model3
  model4_Cancer<-svysurvreg(Surv(peryear, Cancer_MORT_stat==1) ~
                              PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy_DM_Cancer,dist="weibull")
  model4_Cancer_result<-summary(model4_Cancer)
  HR<-exp(model4_Cancer_result[["coefficients"]][2])
  lower<-HR-1.96*model4_Cancer_result[["table"]][,2][2]
  upper<-HR+1.96*model4_Cancer_result[["table"]][,2][2]
  P<-model4_Cancer_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svysurvreg(Surv(peryear, DM_MORT_stat==1) ~
                          PD_diagnosis, design =rhcSvy, dist="weibull")
  model1_DM_result<-summary(model1_DM)
  model1_DM_result
  HR<-exp(model1_DM_result[["coefficients"]][2])
  lower<-HR-1.96*model1_DM_result[["table"]][,2][2]
  upper<-HR+1.96*model1_DM_result[["table"]][,2][2]
  P<-model1_DM_result[["table"]][,4][2]
  result <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #DM model1
  model2_DM<-svysurvreg(Surv(peryear, DM_MORT_stat==1) ~
                          PD_diagnosis+Age_grade+Sex+Race_ethnicity, design =rhcSvy,dist="weibull") #Deleted SES
  model2_DM_result<-summary(model2_DM)
  model2_DM_result
  HR<-exp(model2_DM_result[["coefficients"]][2])
  lower<-HR-1.96*model2_DM_result[["table"]][,2][2]
  upper<-HR+1.96*model2_DM_result[["table"]][,2][2]
  P<-model2_DM_result[["table"]][,4][2]
  result2 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  result.DM
  #DM model2
  model3_DM<-svysurvreg(Surv(peryear, DM_MORT_stat==1) ~
                          PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                          Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use, design =rhcSvy,dist="weibull")#Aded Medicine_use
  model3_DM_result<-summary(model3_DM)
  HR<-exp(model3_DM_result[["coefficients"]][2])
  lower<-HR-1.96*model3_DM_result[["table"]][,2][2]
  upper<-HR+1.96*model3_DM_result[["table"]][,2][2]
  P<-model3_DM_result[["table"]][,4][2]
  result3 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
  #DM model3
  model4_DM<-svysurvreg(Surv(peryear, DM_MORT_stat==1) ~
                          PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                          Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy,dist="weibull")
  model4_DM_result<-summary(model4_DM)
  HR<-exp(model4_DM_result[["coefficients"]][2])
  lower<-HR-1.96*model4_DM_result[["table"]][,2][2]
  upper<-HR+1.96*model4_DM_result[["table"]][,2][2]
  P<-model4_DM_result[["table"]][,4][2]
  result4 <- data.frame('HR'=HR,'lower .95'=lower,'upper .95'=upper,
                        'P value' =P,'model'="model4",'status'="DM cause")
  result.DM<-rbind(result.DM,result4)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 0‡","Model 1§","Model 2¶","Model 3*",
              "CVD-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 0","Model 1","Model 2","Model 3")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:4])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[5:8])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[9:12])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[13:16])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:4])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[5:8])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[9:12])
  P_DM<-as.data.frame(result.all.cause.LAST$P[13:16])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS12<-cbind(head,row1,row2,row3)
  write.table(TableS12,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S12.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 33 NHANES period (Table S13) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
table(Interpolation_weighted$Period)
  
  ##* ALL cause ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_all1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="Model 4",'status'="all cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status+Period, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_all2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="Model period",'status'="all cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Period+PD_diagnosis*Period+
                              Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Period_all.inter<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="Interaction",'status'="all cause")
  Period_all<-rbind(Period_all1,Period_all2,Period_all.inter)
  Period_all
  ##* CVD related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_CVD1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model 4",'status'="CVD cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status+Period, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_CVD2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model period",'status'="CVD cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Period+PD_diagnosis*Period+
                              Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Period_CVD.inter<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'model'="Interaction",'status'="CVD cause")
  Period_CVD<-rbind(Period_CVD1,Period_CVD2,Period_CVD.inter)
  Period_CVD
  ##* Cancer related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_Cancer1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model 4",'status'="Cancer cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status+Period, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_Cancer2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model period",'status'="Cancer cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~PD_diagnosis+Period+PD_diagnosis*Period+
                              Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Period_Cancer.inter<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'model'="Interaction",'status'="Cancer cause")
  Period_Cancer<-rbind(Period_Cancer1,Period_Cancer2,Period_Cancer.inter)
  Period_Cancer
  
  ##* DM related ####
  MODEL_ALL_inter<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_DM1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model 4",'status'="DM cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~PD_diagnosis+Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status+Period, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Period_DM2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="Model period",'status'="DM cause")
  
  MODEL_ALL_inter<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~PD_diagnosis+Period+PD_diagnosis*Period+
                              Age_grade+Sex+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_grade+
                              Medicine_use+HbA1c_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Period_DM.inter<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                                'P value' =P,'model'="Interaction",'status'="DM cause")
  Period_DM<-rbind(Period_DM1,Period_DM2,Period_DM.inter)
  Period_DM

  Period.LAST<-rbind(Period_all,Period_CVD,Period_Cancer,Period_DM)
  Period.LAST

#* Combine #####
  result.all.cause<-Period.LAST
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  result.all.cause
  colnames(result.all.cause)<-c("HR","lower","upper","P","model","status")
  result.all.cause$a<-" ("
  result.all.cause$b<-", "
  result.all.cause$c<-")"
  
  result.all.cause$HR<-str_pad(result.all.cause[,c('HR')],width = 4,side = "right",pad = "0")
  result.all.cause$lower<-str_pad(result.all.cause[,c('lower')],width = 4,side = "right",pad = "0")
  result.all.cause$upper<-str_pad(result.all.cause[,c('upper')],width = 4,side = "right",pad = "0")
  result.all.cause$P<-str_pad(result.all.cause[,c('P')],width = 5,side = "right",pad = "0")
  result.all.cause[,c('P')][result.all.cause[,c('P')]=="00000"]<-"<0.001"
  result.all.cause[result.all.cause=="1000"]<-"1.00"
  result.all.cause[result.all.cause=="2000"]<-"2.00"
  result.all.cause$HR_95<-paste(result.all.cause$HR,
                                result.all.cause$a,
                                result.all.cause$lower,
                                result.all.cause$b,
                                result.all.cause$upper,
                                result.all.cause$c,sep = "")
  
  result.all.cause.LAST<-result.all.cause[,c('HR_95','P','model','status')]
  #EVENTS
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #DM.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
  DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))
  total.counts
  head<-rbind("All cause†","Deaths/total (unweighted)",
              "Model 3‡","Model period§","Interaction",
              "CVD-related","Deaths/total (unweighted)",
              "Model 3","Model period","Interaction",
              "Cancer-related","Deaths/total (unweighted)",
              "Model 3","Model period","Interaction",
              "Diabetes-related","Deaths/total (unweighted)",
              "Model 3","Model period","Interaction")
  row1<-rbind("",total.counts[1,1],"1.00","1.00","1.00",
              "",total.counts[2,1],"1.00","1.00","1.00",
              "",total.counts[3,1],"1.00","1.00","1.00",
              "",total.counts[4,1],"1.00","1.00","1.00")
  HR_ALL<-as.data.frame(result.all.cause.LAST$HR_95[1:3])
  HR_CVD<-as.data.frame(result.all.cause.LAST$HR_95[4:6])
  HR_CAN<-as.data.frame(result.all.cause.LAST$HR_95[7:9])
  HR_DM<-as.data.frame(result.all.cause.LAST$HR_95[10:12])
  colnames(HR_ALL)<-"HR"
  colnames(HR_CVD)<-"HR"
  colnames(HR_CAN)<-"HR"
  colnames(HR_DM)<-"HR"
  row2<-rbind("",total.counts[1,2],HR_ALL,
              "",total.counts[2,2],HR_CVD,
              "",total.counts[3,2],HR_CAN,
              "",total.counts[4,2],HR_DM)
  P_ALL<-as.data.frame(result.all.cause.LAST$P[1:3])
  P_CVD<-as.data.frame(result.all.cause.LAST$P[4:6])
  P_CAN<-as.data.frame(result.all.cause.LAST$P[7:9])
  P_DM<-as.data.frame(result.all.cause.LAST$P[10:12])
  colnames(P_ALL)<-"P"
  colnames(P_CVD)<-"P"
  colnames(P_CAN)<-"P"
  colnames(P_DM)<-"P"
  row3<-rbind("","",P_ALL,
              "","",P_CVD,
              "","",P_CAN,
              "","",P_DM)
  TableS13<-cbind(head,row1,row2,row3)
  write.table(TableS13,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS13.csv",row.names = F)


# +++++++++++================================+++++++++++ ####
# +++++++++++============Figures=============+++++++++++ ####
# +++++++++++================================+++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 33 Kaplan-Meier (Figure 1) ####
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  library("ggthemes")
  FIT_ALL_PD<-survfit(Surv(peryear, MORT_stat==1) ~ PD_diagnosis, Interpolation_weighted)
  Figure_2A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#2e5662", "#e7881c"), 
                        ncensor.plot=F,risk.table = T, fun = "event",
                        break.x.by = 5,font.x = c(16,"black"),font.y = c(16, "black"),font.tickslab = c(16, "plain", "black"),
                        xlab ="Time in Year", pval = , legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),
                                                                                         #panel.grid.major.y = element_line(color="black",size = 0.5),
                                                                                         panel.border = element_rect(fill=NA,color="black", size=0.7)))
  Figure_2A
  #library(eoffice)
  topptx(Figure_2A ,"temp.pptx")
  dev.off()
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1A.pdf",  device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ #### 
# >>>>> section 33 AUC (Figure 2) ####
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  Interpolation_weighted$Ten_status[Interpolation_weighted$peryear>10]<-0
  Interpolation_weighted$Ten_status[Interpolation_weighted$peryear<=10]<-1
  Interpolation_weighted$Five_status[Interpolation_weighted$peryear>5]<-0
  Interpolation_weighted$Five_status[Interpolation_weighted$peryear<=5]<-1
  #* all cause  ####
  model_1<-coxph(Surv(peryear,MORT_stat==1)~PD_diagnosis+Age+Sex+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,data =Interpolation_weighted , x = TRUE, y = TRUE)
  model_2<-coxph(Surv(peryear,MORT_stat==1)~Age+Sex+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,data =Interpolation_weighted , x = TRUE, y = TRUE)
  
  ##* Five years ####
  Five_PD1<-predictCox(model_1,times=5,
                       newdata =Interpolation_weighted,
                       centered = TRUE,
                       type = c("cumhazard", "survival"))
  Interpolation_weighted$Five_survival1<-Five_PD1$survival
  roc_multivar_five1<-roc(Interpolation_weighted$Five_status,Interpolation_weighted$Five_survival1)
  Five_PD2<-predictCox(model_2,times=5,
                       newdata =Interpolation_weighted,
                       centered = TRUE,
                       type = c("cumhazard", "survival"))
  Interpolation_weighted$Five_survival2<-Five_PD2$survival
  roc_multivar_five2<-roc(Interpolation_weighted$Five_status,Interpolation_weighted$Five_survival2)
  
  plot(roc1)
  auc_low_original<-round(ci(roc_multivar_five2,of="auc")[1],3)
  auc__original<-round(ci(roc_multivar_five2,of="auc")[2],3)
  auc_high_original<-round(ci(roc_multivar_five2,of="auc")[3],3)
  auc_low_new<-round(ci(roc_multivar_five1,of="auc")[1],3)
  auc_new<-round(ci(roc_multivar_five1,of="auc")[2],3)
  auc_high_new<-round(ci(roc_multivar_five1,of="auc")[3],3)
  roc.test(roc_multivar_five1,roc_multivar_five2,method = 'delong')
  ggroc(list(`Original model \n AUC=0.631 (0.606, 0.656)`=roc_multivar_five2,
             `Add periodontitis \n AUC=0.652 (0.627, 0.676)`=roc_multivar_five1),legacy.axes = TRUE,size = 1.5)+ 
    scale_colour_manual(values = c("#2e5662", "#e7881c"))+
    guides(color=guide_legend(title = "Regression models"))+
    theme_minimal()+theme_bw()+theme(legend.position = c(.6,0.2),
                                     legend.key.width = unit(2,"cm"),legend.key.height = unit(1.5,"cm"),
                                     panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank())+ 
    theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))+
    theme(axis.text=element_text(size=16))+
    theme(legend.title=element_text(size=15), legend.text=element_text(size=15))+
    annotate("text", x = 0.35 , y = 1,label = "Five-year risk for all-cause mortality",size=6.5) 
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_2A.pdf",device = "pdf", width = 7, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  
  ##* Ten years ####
  Ten_PD1<-predictCox(model_1,times=10,
                      newdata =Interpolation_weighted,
                      centered = TRUE,
                      type = c("cumhazard", "survival"))
  Interpolation_weighted$Ten_survival1<-Ten_PD1$survival
  roc_multivar_Ten1<-roc(Interpolation_weighted$Ten_status,Interpolation_weighted$Ten_survival1)
  Ten_PD2<-predictCox(model_2,times=10,
                      newdata =Interpolation_weighted,
                      centered = TRUE,
                      type = c("cumhazard", "survival"))
  Interpolation_weighted$Ten_survival2<-Ten_PD2$survival
  roc_multivar_Ten2<-roc(Interpolation_weighted$Ten_status,Interpolation_weighted$Ten_survival2)
  
  auc_low_original<-round(ci(roc_multivar_Ten2,of="auc")[1],3)
  auc__original<-round(ci(roc_multivar_Ten2,of="auc")[2],3)
  auc_high_original<-round(ci(roc_multivar_Ten2,of="auc")[3],3)
  auc_low_new<-round(ci(roc_multivar_Ten1,of="auc")[1],3)
  auc_new<-round(ci(roc_multivar_Ten1,of="auc")[2],3)
  auc_high_new<-round(ci(roc_multivar_Ten1,of="auc")[3],3)
  roc.test(roc_multivar_Ten1,roc_multivar_Ten2,method = 'delong')
  ggroc(list(`Original model \n AUC=0.532 (0.515, 0.550)`=roc_multivar_Ten2,
             `Add periodontitis \n AUC=0.550 (0.532, 0.567)`=roc_multivar_Ten1),legacy.axes = TRUE,size = 1.5)+ 
    scale_colour_manual(values = c("#2e5662", "#e7881c"))+
    guides(color=guide_legend(title = "Regression models"))+
    theme_minimal()+theme_bw()+theme(legend.position = c(.6,0.2),
                                     legend.key.width = unit(2,"cm"),legend.key.height = unit(1.5,"cm"),
                                     panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank())+ 
    theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))+
    theme(axis.text=element_text(size=16))+
    theme(legend.title=element_text(size=15), legend.text=element_text(size=15))+
    annotate("text", x = 0.35 , y = 1,label = "Ten-year risk for all-cause mortality",size=6.5) 
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_2B.pdf",device = "pdf", width = 7, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  
  
  #* CVD related  ####
  model_1<-coxph(Surv(peryear,CVD_MORT_stat==1)~PD_diagnosis+Age+Sex+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,data =Interpolation_weighted , x = TRUE, y = TRUE)
  model_2<-coxph(Surv(peryear,CVD_MORT_stat==1)~Age+Sex+Race_ethnicity+BMI+HbA1c+HTN_status+HPL_status,data =Interpolation_weighted , x = TRUE, y = TRUE)
  
  ##* Five years ####
  Five_PD1<-predictCox(model_1,times=5,
                       newdata =Interpolation_weighted,
                       centered = TRUE,
                       type = c("cumhazard", "survival"))
  Interpolation_weighted$Five_survival1<-Five_PD1$survival
  roc_multivar_five1<-roc(Interpolation_weighted$Five_status,Interpolation_weighted$Five_survival1)
  Five_PD2<-predictCox(model_2,times=5,
                       newdata =Interpolation_weighted,
                       centered = TRUE,
                       type = c("cumhazard", "survival"))
  Interpolation_weighted$Five_survival2<-Five_PD2$survival
  roc_multivar_five2<-roc(Interpolation_weighted$Five_status,Interpolation_weighted$Five_survival2)
  auc_low_original<-round(ci(roc_multivar_five2,of="auc")[1],3)
  auc__original<-round(ci(roc_multivar_five2,of="auc")[2],3)
  auc_high_original<-round(ci(roc_multivar_five2,of="auc")[3],3)
  auc_low_new<-round(ci(roc_multivar_five1,of="auc")[1],3)
  auc_new<-round(ci(roc_multivar_five1,of="auc")[2],3)
  auc_high_new<-round(ci(roc_multivar_five1,of="auc")[3],3)
  roc.test(roc_multivar_five1,roc_multivar_five2,method = 'delong')
  ggroc(list(`Original model \n AUC=0.629 (0.604, 0.655)`=roc_multivar_five2,
             `Add periodontitis \n AUC=0.649 (0.624, 0.674)`=roc_multivar_five1),legacy.axes = TRUE,size = 1.5)+ 
    scale_colour_manual(values = c("#2e5662", "#e7881c"))+
    guides(color=guide_legend(title = "Regression models"))+
    theme_minimal()+theme_bw()+theme(legend.position = c(.6,0.2),
                                     legend.key.width = unit(2,"cm"),legend.key.height = unit(1.5,"cm"),
                                     panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank())+ 
    theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))+
    theme(axis.text=element_text(size=16))+
    theme(legend.title=element_text(size=15), legend.text=element_text(size=15))+
    annotate("text", x = 0.35 , y = 1,label = "Five-year risk for CVD-related mortality",size=6.5) 
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_2C.pdf",device = "pdf", width = 7, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  
  ##* Ten years ####
  Ten_PD1<-predictCox(model_1,times=10,
                      newdata =Interpolation_weighted,
                      centered = TRUE,
                      type = c("cumhazard", "survival"))
  Interpolation_weighted$Ten_survival1<-Ten_PD1$survival
  roc_multivar_Ten1<-roc(Interpolation_weighted$Ten_status,Interpolation_weighted$Ten_survival1)
  Ten_PD2<-predictCox(model_2,times=10,
                      newdata =Interpolation_weighted,
                      centered = TRUE,
                      type = c("cumhazard", "survival"))
  Interpolation_weighted$Ten_survival2<-Ten_PD2$survival
  roc_multivar_Ten2<-roc(Interpolation_weighted$Ten_status,Interpolation_weighted$Ten_survival2)
  
  auc_low_original<-round(ci(roc_multivar_Ten2,of="auc")[1],3)
  auc__original<-round(ci(roc_multivar_Ten2,of="auc")[2],3)
  auc_high_original<-round(ci(roc_multivar_Ten2,of="auc")[3],3)
  auc_low_new<-round(ci(roc_multivar_Ten1,of="auc")[1],3)
  auc_new<-round(ci(roc_multivar_Ten1,of="auc")[2],3)
  auc_high_new<-round(ci(roc_multivar_Ten1,of="auc")[3],3)
  roc.test(roc_multivar_Ten1,roc_multivar_Ten2,method = 'delong')
  ggroc(list(`Original model \n AUC=0.539 (0.522, 0.556)`=roc_multivar_Ten2,
             `Add periodontitis \n AUC=0.553 (0.535, 0.570)`=roc_multivar_Ten1),legacy.axes = TRUE,size = 1.5)+ 
    scale_colour_manual(values = c("#2e5662", "#e7881c"))+
    guides(color=guide_legend(title = "Regression models"))+
    theme_minimal()+theme_bw()+theme(legend.position = c(.6,0.2),
                                     legend.key.width = unit(2,"cm"),legend.key.height = unit(1.5,"cm"),
                                     panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank())+ 
    theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))+
    theme(axis.text=element_text(size=16))+
    theme(legend.title=element_text(size=15), legend.text=element_text(size=15))+
    annotate("text", x = 0.38 , y = 1,label = "Ten-year risk for CVD-related mortality",size=6.5) 
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_2D.pdf",device = "pdf", width = 7, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 RCS for CAL and PPD (Figure S4) ####
setwd("G:/paper_2_PD&DM/data_DC")
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
RCS <- Interpolation_weighted
{#* All cause mortarity ####
  {#** CAL_mean ####
    pdf("Figure_S4A.pdf",width=6,height=5)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(CAL_mean,i)+Age_grade+Sex+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(CAL_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.63
    
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#F39B7FFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 1.5, paste0("ref value = ","1.63")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    colnames(RCS)
    setwd("G:/paper_2_PD&DM/data_DC")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Figure_S4E.pdf",width=6,height=5)
    colnames(RCS)
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(PPD_mean,i)+Age_grade+Sex+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(PPD_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.41
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    points(3.27,1,pch=16,cex=1)
    text(1.41, 1.2, paste0("ref value = ","1.41")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
{#* CVD mortarity ####
  RCS<-RCS
  RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
  RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
  {#** CAL_mean ####
    pdf("Figure_S4B.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(CAL_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.53
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#F39B7FFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 1.5, paste0("ref value = ","1.53")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
    RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
    colnames(RCS)
    setwd("G:/paper_2_PD&DM/data_DC")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Figure_S4F.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(PPD_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.42
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    points(3.89,1,pch=16,cex=1)
    text(refvalue, 1.5, paste0("ref value = ","1.42")) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
{#* Cancer mortarity ####
  RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
  RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
  {#** CAL_mean ####
    pdf("Figure_S4C.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.56
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#F39B7FFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 0.7, paste0("ref value = ","1.56")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    colnames(RCS)
    setwd("G:/paper_2_PD&DM/data_DC")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Figure_S4G.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.41
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 1.41, paste0("ref value = ","1.41")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
{#* DM mortarity ####
  RCS<-Interpolation_weighted
  RCS$DM_MORT_stat<-as.character(RCS$DM_MORT_stat)
  RCS$DM_MORT_stat<-as.numeric(RCS$DM_MORT_stat)
  {#** CAL_mean ####
    pdf("Figure_S4D.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,DM_MORT_stat==1)~rcs(CAL_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.54
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,CAL_mean,ref.zero=TRUE,fun=exp)
    violet <- "#F39B7FFF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$CAL_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean clinical attachment loss",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue+1, 2, paste0("ref value = ","1.54")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    colnames(RCS)
    setwd("G:/paper_2_PD&DM/data_DC")
    limUp<-3*IQR(RCS[,"PPD_mean"],na.rm = T)+quantile(RCS[,"PPD_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"PPD_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"PPD_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"PPD_mean"]>=limUp&!RCS[,"PPD_mean"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    pdf("Figure_S4H.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,DM_MORT_stat==1)~rcs(PPD_mean,3)+Age_grade+Sex+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_grade+Medicine_use+HbA1c_status,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.37
    ddist<-datadist(RCS)
    ddist$limits$BMI[2]<-refvalue
    options(datadist="ddist")
    pred_OR<-Predict(fit,PPD_mean,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$PPD_mean)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "Mean periodontal probing depth",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue, 1.37, paste0("ref value = ","1.37")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
}

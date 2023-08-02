# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
library(caret)
library(car)
  library(dplyr)
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
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 16. Multiple interpolation <<<<< ####
{#* section 16.1 Multiple interpolation ####
  load(file="G:/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Characters.Rdata")
  Complete_data<-na.omit(Characters)
  rownames(Characters)<-Characters$ID
  mice_data<-Characters[,c("Age","BMI","HbA1c","Gender","Race_ethnicity",
                           "Education_levels","PIR","Health_insurance","SEI",
                           "Smoking_status","Drinking_status","HEI","Medicine_use",
                           "sdmvpsu","sdmvstra","weight")]
  mice_data$sdmvpsu<-Clearn_data$sdmvpsu
  mice_data$sdmvstra<-Clearn_data$sdmvstra
  mice_data$weight<-Clearn_data$weight
  #methods(mice)
  mice::md.pattern(mice_data)
  sapply(data, function(x) sum(is.na(mice_data)))
  miss <- function(x){sum(is.na(x))/length(x)*100}
  apply(mice_data,2,miss)
  init = mice(mice_data,maxit= 10, m=5,seed = 500) 
  stripplot(init)
  save(init,file="G:/paper_2_PD&DM/data_DC/mice_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/mice_data.Rdata")
  Interpolation_data <- complete(init,5)
  Interpolation_data$ID<-Clearn_data$ID
  save(Complete_data,file="G:/paper_2_PD&DM/data_DC/Complete_data.Rdata")
  save(Interpolation_data,file="G:/paper_2_PD&DM/data_DC/Interpolation_data.Rdata")
}
{#* section 16.2 Data restoration ####
  load(file="G:/paper_2_PD&DM/data_DC/Complete_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
  {#** section 16.2.1 Original data ####
    #AGE
    Character<-as.data.frame(Clearn_data$ID)
    colnames(Character)<-"ID"
    Character$Age_grade[Clearn_data$Age<40]<-"<40"
    Character$Age_grade[Clearn_data$Age>=40&Clearn_data$Age<50]<-"[40,50)"
    Character$Age_grade[Clearn_data$Age>=50&Clearn_data$Age<60]<-"[50,60)"
    Character$Age_grade[Clearn_data$Age>=60&Clearn_data$Age<70]<-"[60,70)"
    Character$Age_grade[Clearn_data$Age>=70&Clearn_data$Age<80]<-"[70,80)"
    Character$Age_grade[Clearn_data$Age>=80]<-">=80"
    Character$Age_grade<-factor(Character$Age_grade,
                                             levels = c("<40","[40,50)","[50,60)","[60,70)","[70,80)",">=80"))
    table(Character$Age_grade,useNA = "ifany")
    
    #SEX
    table(Clearn_data$Gender)
    Character$Sex<-Clearn_data$Gender
    
    #Race_ethnicity
    table(Clearn_data$Race_ethnicity)
    Character$Race_ethnicity<-Clearn_data$Race_ethnicity
    
    #Education_levels
    table(Clearn_data$Education_levels)
    Character$Education_levels<-Clearn_data$Education_levels
    # PIR 
    table(Clearn_data$PIR)
    Character$PIR<-Clearn_data$PIR
    table(Character$PIR)
    
    #Health_insurance
    table(Clearn_data$Health_insurance)
    Character$Health_insurance<-Clearn_data$Health_insurance
    
    #SEI
    table(Clearn_data$SEI)
    Character$SEI<-Clearn_data$SEI
    
    #Smoking_status
    table(Clearn_data$Smoking_status)
    Character$Smoking_status<-Clearn_data$Smoking_status
    
    #Drinking_status
    table(Clearn_data$Drinking_status)
    Character$Drinking_status<-Clearn_data$Drinking_status
    
    #HEI
    table(Clearn_data$HEI)
    Character$HEI<-Clearn_data$HEI
    
    #HEI
    table(Clearn_data$BMI_grade)
    Character$BMI_grade<-Clearn_data$BMI_grade
 
    #BMI_grade
    Character$BMI_grade[Clearn_data$BMI<25]<-'(0,25)'
    Character$BMI_grade[Clearn_data$BMI>=25&Clearn_data$BMI<30]<-'[25.0-30)'
    Character$BMI_grade[Clearn_data$BMI>=30]<-'[30,inf)' 
    Character$BMI_grade<-factor(Character$BMI_grade,
                                    levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Character$BMI_grade)
    #Medicine_use 
    table(Clearn_data$Medicine_use)
    Character$Medicine_use<-Clearn_data$Medicine_use
    
    #HbA1c_status
    table(Clearn_data$HbA1c_status)
    Character$HbA1c_status<-Clearn_data$HbA1c_status
    
    #weight
    Character$sdmvpsu<-Clearn_data$sdmvpsu
    Character$sdmvstra<-Clearn_data$sdmvstra
    Character$weight<-Clearn_data$weight
    Character$weight<-Clearn_data$weight
    Original<-Character
    save(Original,file="G:/paper_2_PD&DM/data_DC/Original.Rdata")
  }
   {#** section 16.2.1 Interpolation data ####
     Interpolation<-as.data.frame(Clearn_data$ID)
     colnames(Interpolation)<-"ID"
     #AGE
     Interpolation$Age_grade[Interpolation_data$Age<40]<-"<40"
     Interpolation$Age_grade[Interpolation_data$Age>=40&Interpolation_data$Age<50]<-"[40,50)"
     Interpolation$Age_grade[Interpolation_data$Age>=50&Interpolation_data$Age<60]<-"[50,60)"
     Interpolation$Age_grade[Interpolation_data$Age>=60&Interpolation_data$Age<70]<-"[60,70)"
     Interpolation$Age_grade[Interpolation_data$Age>=70&Interpolation_data$Age<80]<-"[70,80)"
     Interpolation$Age_grade[Interpolation_data$Age>=80]<-">=80"
     Interpolation$Age_grade<-factor(Interpolation$Age_grade,
                                 levels = c("<40","[40,50)","[50,60)","[60,70)","[70,80)",">=80"))
     table(Interpolation$Age_grade,useNA = "ifany")
     
     #Gender
     table(Interpolation_data$Gender)
     
     Interpolation$Sex<-case_match(Interpolation_data$Gender,'1'~'Male','2'~'Female')  
     Interpolation$Sex<-factor(Interpolation$Sex,
                                     levels = c('Male','Female'))
     table(Interpolation$Sex,useNA = "ifany")
     #Race_ethnicity
     table(Interpolation_data$Race_ethnicity)
     Interpolation$Race_ethnicity<-case_match(Interpolation_data$Race_ethnicity,
                                          '1'~'Non-Hispanic White','2'~'Non-Hispanic Black','3'~'Hispanic','4'~'Other Race')  
     Interpolation$Race_ethnicity<-factor(Interpolation$Race_ethnicity,
                                          levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other Race")) 
     table(Interpolation$Race_ethnicity,useNA = "ifany")
     #Education_levels
     table(Interpolation_data$Education_levels)
     Interpolation$Education_levels<-case_match(Interpolation_data$Education_levels,
                                            '1'~'Less than high school','2'~'High school or Equivalent','3'~'College or above')  
     Interpolation$Education_levels<-factor(Interpolation$Education_levels,
                                            levels = c("Less than high school","High school or Equivalent","College or above")) 
     table(Interpolation$Education_levels)
     
     # PIR 
     table(Interpolation_data$PIR)
     Interpolation$PIR<-case_match(Interpolation_data$PIR,
                               '1'~'(0, 1]','2'~'(1,4)','3'~'[4,inf)')  
     Interpolation$PIR<-factor(Interpolation$PIR,
                               levels = c("(0, 1]","(1,4)","[4,inf)")) 
     table(Interpolation$PIR)
     
     #Health_insurance
     table(Interpolation_data$Health_insurance)
     Interpolation$Health_insurance<-case_match(Interpolation_data$Health_insurance,
                                            '1'~'No insurance','2'~'Public insurance','3'~'Private insurance')  
     Interpolation$Health_insurance<-factor(Interpolation$Health_insurance,
                                            levels = c("No insurance","Public insurance","Private insurance")) 
     table(Interpolation$Health_insurance)
     
     #SEI
     table(Interpolation_data$SEI)
     Interpolation$SEI<-case_match(Interpolation_data$SEI,
                                            '1'~'Unemployment','2'~'Lower','3'~'Upper')  
     Interpolation$SEI<-factor(Interpolation$SEI,
                                            levels = c("Unemployment","Lower","Upper")) 
     table(Interpolation$SEI)
     
     #Smoking_status
     table(Interpolation_data$Smoking_status)
     Interpolation$Smoking_status<-case_match(Interpolation_data$Smoking_status,
                                          '1'~'Never smoker','2'~'Former smoker','3'~'Current smoker')  
     Interpolation$Smoking_status<-factor(Interpolation$Smoking_status,
                                          levels = c("Never smoker","Former smoker","Current smoker")) 
     table(Interpolation$Smoking_status)
     
     #Drinking_status
     table(Interpolation_data$Drinking_status)
     Interpolation$Drinking_status<-case_match(Interpolation_data$Drinking_status,
                                           '1'~'Non-drinker','2'~'Light/moderate drinker','3'~'Heavier drinker')  
     Interpolation$Drinking_status<-factor(Interpolation$Drinking_status,
                                           levels = c("Non-drinker","Light/moderate drinker","Heavier drinker")) 
     table(Interpolation$Drinking_status)
     
     
     #HEI
     table(Interpolation_data$HEI)
     Interpolation$HEI<-case_match(Interpolation_data$HEI,
                               '1'~'Quintile 1','2'~'Quintile 2','3'~'Quintile 3','4'~'Quintile 4','5'~'Quintile 5')  
     Interpolation$HEI<-factor(Interpolation$HEI,
                               levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
     table(Interpolation$HEI)
     
     #BMI_grade
     Interpolation$BMI_grade[Interpolation_data$BMI<25]<-'(0,25)'
     Interpolation$BMI_grade[Interpolation_data$BMI>=25&Interpolation_data$BMI<30]<-'[25.0-30)'
     Interpolation$BMI_grade[Interpolation_data$BMI>=30]<-'[30,inf)' 
     Interpolation$BMI_grade<-factor(Interpolation$BMI_grade,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
     table(Interpolation$BMI_grade)
    
     
     #Medicine_use 
     table(Interpolation_data$Medicine_use)
     Interpolation$Medicine_use<-case_match(Interpolation_data$Medicine_use,
                                        '1'~'NO','2'~'YES')  
     Interpolation$Medicine_use<-factor(Interpolation$Medicine_use,
                                        levels = c("NO","YES")) 
     table(Interpolation$Medicine_use)
     
     #HbA1c_status
     Interpolation$HbA1c_status[Interpolation_data$HbA1c<7]<-"HbA1c<7"
     Interpolation$HbA1c_status[Interpolation_data$HbA1c>=7]<-"HbA1c>=7"
     Interpolation$HbA1c_status<-factor(Interpolation$HbA1c_status,
                                        levels = c("HbA1c<7","HbA1c>=7")) 
     table(Interpolation$HbA1c_status)
     
     #weight
     Interpolation$sdmvpsu<-Interpolation_data$sdmvpsu
     Interpolation$sdmvstra<-Interpolation_data$sdmvstra
     Interpolation$weight<-Interpolation_data$weight
     save(Interpolation,file="G:/paper_2_PD&DM/data_DC/Interpolation.Rdata")
   }
   {#** section 16.2.2 Complete data ####
     Complete<-na.omit(Original)
     save(Complete,file="G:/paper_2_PD&DM/data_DC/Complete.Rdata")
   }
}

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
  # >>>>> section 18 latent class analysis (Figure S3) ####
  load(file="G:/paper_2_PD&DM/data_DC/Original.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Complete.Rdata")
  Interpolation_SES<-Interpolation[,c("ID","Education_levels","PIR","Health_insurance","SEI")]
  Interpolation_SES$Education_levels<-as.numeric(Interpolation_SES$Education_levels)
  Interpolation_SES$PIR<-as.numeric(Interpolation_SES$PIR)
  Interpolation_SES$Health_insurance<-as.numeric(Interpolation_SES$Health_insurance)
  Interpolation_SES$SEI<-as.numeric(Interpolation_SES$SEI)
  set.seed(0)
  M1 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 1, maxiter = 10000, nrep = 10, graph = TRUE)
  M2 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 2, maxiter = 10000, nrep = 10, graph = TRUE)
  M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
  M4 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 4, maxiter = 10000, nrep = 10, graph = TRUE)
  M5 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 5, maxiter = 10000, nrep = 10, graph = F)
  Aic_bic<-as.data.frame(rbind(c(M1$aic,M1$bic),c(M2$aic,M2$bic),c(M3$aic,M3$bic),c(M4$aic,M4$bic),c(M5$aic,M5$bic)))
  colnames(Aic_bic)<-c("AIC","BIC")
  rownames(Aic_bic)<-c("nclass = 1","nclass = 2","nclass = 3","nclass = 4","nclass = 5")
  TableS1<-Aic_bic
  posterior <- data.frame(M3$posterior)
  posterior$label <- Interpolation_SES$ID
  posterior$class <- as.character(M3$predclass)
  write.table(TableS1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS11.csv")
  names(posterior)[1:2] <- c('class1_probabilities', 'class2_probabilities')
  Figure_S3A<-ggplot(posterior,max.overlaps = Inf) +
    geom_point(aes(class1_probabilities, class2_probabilities, color = class),size=2.5,alpha=0.5) +
    theme_bw()+  scale_color_nejm()+ scale_fill_nejm()+ 
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  Figure_S3A
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_S3A.pdf",Figure_S3A, device = "pdf",width = 8, height = 6, units ="in",
         dpi = 300, limitsize = TRUE)
  #M3 <- poLCA(formula = cbind(Education_levels,PIR,Health_insurance,SEI)~1, data = Interpolation_SES, nclass = 3, maxiter = 10000, nrep = 10, graph = TRUE)
  M3_probs <- melt(M3$probs, level = 2)
  Figure_S2B<-  ggplot(M3_probs,aes(x = value, y =L2 , fill = Var2)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
    facet_grid(Var1~.) +
    scale_fill_brewer(type = 'seq', palette = 'Red') +
    theme_bw() +
    labs(x = '', fill = 'probabilities') +
    guides(fill = guide_legend(reverse = TRUE))
  Figure_S2B
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_S3B.pdf",Figure_S2B,device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  Interpolation$SES[M3$predclass==1]<-"high"
  Interpolation$SES[M3$predclass==2]<-"medium"
  Interpolation$SES[M3$predclass==3]<-"low"
  Interpolation$SES<-factor(Interpolation$SES,
                                     levels = c("low","medium","high"))
  table(Interpolation$SES,Interpolation$Education_levels)
  Original$SES<-Interpolation$SES
  SES<-Interpolation[,c("ID","SES")]
  Complete<-merge(Complete,SES,by = "ID",all.x = T)
  Original$Education_levels<-NULL
  Interpolation$Education_levels<-NULL
  Complete$Education_levels<-NULL
  
  Original$PIR<-NULL
  Interpolation$PIR<-NULL
  Complete$PIR<-NULL
  
  Complete$Health_insurance<-NULL
  Original$Health_insurance<-NULL
  Interpolation$Health_insurance<-NULL
  
  
  
  save(Original,file="G:/paper_2_PD&DM/data_DC/Original.Rdata")
  save(Interpolation,file="G:/paper_2_PD&DM/data_DC/Interpolation.Rdata")
  save(Complete,file="G:/paper_2_PD&DM/data_DC/Complete.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 17 Processed based on reviewer comments ####
  load(file="G:/paper_2_PD&DM/data_DC/Original.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Complete.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_data.Rdata")
  Mort_data<-Clearn_data[,c(1:12,26:29)]
  Mort_data$Period<-substr(Mort_data$ID, 1, 11)
  Mort_data$Period[Mort_data$Period=='NHANES_III_']<-"NHANES_III"
  Mort_data$Period<-factor(Mort_data$Period,levels = c("NHANES_III","NHANES_CON1","NHANES_CON2"))
  table(Mort_data$Period)
  Mort_data$MORT_stat<-case_match(Mort_data$MORT_stat,
                                           'Alive'~'0','Deceased'~'1')
  
  Mort_data$CVD_MORT_stat[Mort_data$ucod_leading=="CVD"]<-1
  Mort_data$CVD_MORT_stat[Mort_data$ucod_leading!="CVD"&Mort_data$MORT_stat==1]<-2
  Mort_data$CVD_MORT_stat[Mort_data$MORT_stat==0]<-0
  
  Mort_data$Cancer_MORT_stat[Mort_data$ucod_leading=="Cancer"]<-1
  Mort_data$Cancer_MORT_stat[Mort_data$ucod_leading!="Cancer"&Mort_data$MORT_stat==1]<-2
  Mort_data$Cancer_MORT_stat[Mort_data$MORT_stat==0]<-0
  
  Mort_data$DM_MORT_stat[Mort_data$ucod_leading=="DM"]<-1
  Mort_data$DM_MORT_stat[Mort_data$ucod_leading!="DM"&Mort_data$MORT_stat==1]<-2
  Mort_data$DM_MORT_stat[Mort_data$MORT_stat==0]<-0
  Original_weighted<-merge(Mort_data,Original,by = "ID",all.y = T)
  Interpolation_weighted<-merge(Mort_data,Interpolation,by = "ID",all.y = T)
  Complete_weighted<-merge(Mort_data,Complete,by = "ID",all.y = T)
  Interpolation_weighted$Age<-Interpolation_data$Age
  Interpolation_weighted$BMI<-Interpolation_data$BMI
  Interpolation_weighted$HbA1c<-Interpolation_data$HbA1c
  save(Original_weighted,file="G:/paper_2_PD&DM/data_DC/Original_weighted.Rdata")
  save(Interpolation_weighted,file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  save(Complete_weighted,file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
    

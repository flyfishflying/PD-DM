
library(Matching)
library(tableone)
library(survey)
library(plyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(mice)
library(survminer)
library(survival)
library(ggplot2)
library(ggsci)
library(survminer)
library(survival)
library(ggplot2)
library(ggsci)
library(car)
library(survival)
library(cmprsk)
library(splines)


library(ggplot2)
library(ggrepel)
library(foreign)

library(prodlim)
library(lava)
library(timeROC)
library(pec)
library(riskRegression)
library(survey)
#install.packages("poLCA", dependencies = TRUE)
library(poLCA)
library(reshape2)
library(ggplot2)
as.num
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 16. Multiple interpolation <<<<< ####
{#* section 16.1 Multiple interpolation ####
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Characters.Rdata")
  Complete_data<-na.omit(Characters)
  rownames(Characters)<-Characters$ID
  mice_data<-Characters
  #methods(mice)
  mice::md.pattern(mice_data)
  sapply(data, function(x) sum(is.na(mice_data)))
  miss <- function(x){sum(is.na(x))/length(x)*100}
  apply(mice_data,2,miss)
  init = mice(mice_data,maxit= 10, m=10,seed = 500) 
  stripplot(init)
  save(init,file="G:/paper_2_PD&DM/data_DC/mice_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/mice_data.Rdata")
  Interpolation_data <- complete(init,10)
  Interpolation_data$ID<-rownames(Interpolation_data)
  save(Complete_data,file="G:/paper_2_PD&DM/data_DC/Complete_data.Rdata")
  save(Interpolation_data,file="G:/paper_2_PD&DM/data_DC/Interpolation_data.Rdata")

}
{#* section 16.2 Data restoration ####
  load(file="G:/paper_2_PD&DM/data_DC/Complete_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_data.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Characters.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
  {#** section 16.2.2 Complete data ####
    colApply <- function(dat, cols = colnames, func = as.numeric) {
      dat[cols] <- lapply(dat[cols], func)
      return(dat)
    }
    colnames<-colnames(Characters)
    Character_data<-colApply(Characters,colnames, as.numeric)
    
    library(car)  
    Character<-cbind(rownames(Character_data),
                    Character_data[,colnames(Character_data)[c(1:4)]])
    colnames(Character)[1]<-"ID"
    
    #AGE
    table(Clearn_data$Age_status)
    Character$Age_status[Character_data$Age<45]<-"<45"
    Character$Age_status[Character_data$Age>=45&Character$Age<65]<-"[45,65)"
    Character$Age_status[Character_data$Age>=65]<-">=65"
    Character$Age_status<-factor(Character$Age_status,
                                levels = c("<45","[45,65)",">=65"))
    table(Character$Age_status)
    
    #Gender
    table(Clearn_data$Gender)
    Character$Gender<-recode(Character_data$Gender,"1='Male';2='Female'")  
    Character$Gender<-factor(Character$Gender,
                            levels = c("Male","Female"))
    table(Character$Gender)
    colnames(Character_data)
    
    #Race_ethnicity
    table(Clearn_data$Race_ethnicity)
    Character$Race_ethnicity<-recode(Character_data$Race_ethnicity,
                                    "1='Non-Hispanic White';2='Non-Hispanic Black';3='Hispanic';4='Other Race'")  
    Character$Race_ethnicity<-factor(Character$Race_ethnicity,
                                    levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other Race")) 
    table(Character$Race_ethnicity)
    
    #Education_levels
    table(Clearn_data$Education_levels)
    Character$Education_levels<-recode(Character_data$Education_levels,
                                      "1='Less than high school';2='High school or Equivalent';3='College or above'")  
    Character$Education_levels<-factor(Character$Education_levels,
                                      levels = c("Less than high school","High school or Equivalent","College or above")) 
    table(Character$Education_levels)
    
    # PIR 
    table(Clearn_data$PIR)
    Character$PIR<-recode(Character_data$PIR,
                         "1='(0, 1]';2='(1,4)';3='[4,inf)'")  
    Character$PIR<-factor(Character$PIR,
                         levels = c("(0, 1]","(1,4)","[4,inf)")) 
    table(Character$PIR)
    
    #Health_insurance
    table(Clearn_data$Health_insurance)
    Character$Health_insurance<-recode(Character_data$Health_insurance,
                                      "1='No insurance';2='Public insurance';3='Private insurance'")  
    Character$Health_insurance<-factor(Character$Health_insurance,
                                      levels = c("No insurance","Public insurance","Private insurance")) 
    table(Character$Health_insurance)
    
    #SEI
    table(Clearn_data$SEI)
    Character$SEI<-recode(Character_data$SEI,
                                       "1='Unemployment';2='Lower';3='Upper'")  
    Character$SEI<-factor(Character$SEI,
                                       levels = c("Unemployment","Lower","Upper")) 
    table(Character$SEI)
    
    #Smoking_status
    table(Clearn_data$Smoking_status)
    Character$Smoking_status<-recode(Character_data$Smoking_status,
                                    "1='Never smoker';2='Former smoker';3='Current smoker'")  
    Character$Smoking_status<-factor(Character$Smoking_status,
                                    levels = c("Never smoker","Former smoker","Current smoker")) 
    table(Character$Smoking_status)
    
    #Drinking_status
    table(Clearn_data$Drinking_status)
    Character$Drinking_status<-recode(Character_data$Drinking_status,
                                     "1='Non-drinker';2='Light/moderate drinker';3='Heavier drinker'")  
    Character$Drinking_status<-factor(Character$Drinking_status,
                                     levels = c("Non-drinker","Light/moderate drinker","Heavier drinker")) 
    table(Character$Drinking_status)
    
    #Physical_status
    table(Clearn_data$Physical_status)
    Character$Physical_status<-recode(Character_data$Physical_status,
                                     "1='Inactive';2='Insufficient';3='Recommended'")  
    Character$Physical_status<-factor(Character$Physical_status,
                                     levels = c("Inactive","Insufficient","Recommended")) 
    table(Character$Physical_status)
    
    #HEI
    table(Clearn_data$HEI)
    Character$HEI<-recode(Character_data$HEI,
                         "1='Quintile 1';2='Quintile 2';3='Quintile 3';4='Quintile 4';5='Quintile 5'")  
    Character$HEI<-factor(Character$HEI,
                         levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
    table(Character$HEI)
    
    #HEI
    table(Clearn_data$BMI_Grade)
    Character$BMI_Grade[Character$BMI<25]<-'(0,25)'
    Character$BMI_Grade[Character$BMI>=25&Character$BMI<30]<-'[25.0-30)'
    Character$BMI_Grade[Character$BMI>=30]<-'[30,inf)' 
    Character$BMI_Grade<-factor(Character$BMI_Grade,
                               levels = c("(0,25)","[25.0-30)","[30,inf)")) 
    table(Character$BMI_Grade)
    
    #CVD_status
    table(Clearn_data$CVD_status)
    Character$CVD_status<-recode(Character_data$CVD_status,
                                "1='NO';2='YES'")  
    Character$CVD_status<-factor(Character$CVD_status,
                                levels = c("NO","YES")) 
    table(Character$CVD_status)
    
    #Cancer_status
    table(Clearn_data$Cancer_status)
    Character$Cancer_status<-recode(Character_data$Cancer_status,
                                   "1='NO';2='YES'")  
    Character$Cancer_status<-factor(Character$Cancer_status,
                                   levels = c("NO","YES")) 
    table(Character$Cancer_status)
    
    #Br_diabetes_status
    table(Clearn_data$Br_diabetes_status)
    Character$Br_diabetes_status<-recode(Character_data$Br_diabetes_status,
                                        "1='NO';2='YES'")  
    Character$Br_diabetes_status<-factor(Character$Br_diabetes_status,
                                        levels = c("NO","YES")) 
    table(Character$Br_diabetes_status)
    
    #Medicine_use 
    table(Clearn_data$Medicine_use)
    Character$Medicine_use<-recode(Character_data$Medicine_use,
                                  "1='NO';2='YES'")  
    Character$Medicine_use<-factor(Character$Medicine_use,
                                  levels = c("NO","YES")) 
    table(Character$Medicine_use)
    
    #HbA1c_status
    table(Clearn_data$HbA1c_status)
    Character$HbA1c_status[Character$HbA1c<7]<-"HbA1c<7"
    Character$HbA1c_status[Character$HbA1c>=7]<-"HbA1c>=7"
    Character$HbA1c_status<-factor(Character$HbA1c_status,
                                  levels = c("HbA1c<7","HbA1c>=7")) 
    table(Character$HbA1c_status)
    
    #duration_status
    table(Clearn_data$duration_status)
    Character$duration_status<-recode(Character_data$duration_status,
                                     "1='less than 3 years';2='more than 3 years'")  
    Character$duration_status<-factor(Character$duration_status,
                                     levels = c("less than 3 years","more than 3 years")) 
    table(Character$duration_status)
    
    #weight
    Character$sdmvpsu<-Character_data$sdmvpsu
    Character$sdmvstra<-Character_data$sdmvstra
    Character$weight<-Character_data$weight
    save(Character,file="G:/paper_2_PD&DM/data_DC/Character.Rdata")
  }
   {#** section 16.2.1 Interpolation data ####
     colApply <- function(dat, cols = colnames, func = as.numeric) {
       dat[cols] <- lapply(dat[cols], func)
       return(dat)
     }
     colnames<-colnames(Interpolation_data)
     Interpolation_data<-colApply(Interpolation_data,colnames, as.numeric)
     Interpolation_data$ID<-Clearn_data$ID
      
     Interpolation<-Interpolation_data[,colnames(Interpolation_data)[c(23,1:4)]]
     
     #AGE
     table(Clearn_data$Age_status)
     Interpolation$Age_status[Interpolation_data$Age<45]<-"<45"
     Interpolation$Age_status[Interpolation_data$Age>=45&Interpolation$Age<65]<-"[45,65)"
     Interpolation$Age_status[Interpolation_data$Age>=65]<-">=65"
     Interpolation$Age_status<-factor(Interpolation$Age_status,
                                      levels = c("<45","[45,65)",">=65"))
     table(Interpolation$Age_status)
     
     #Gender
     table(Clearn_data$Gender)
     Interpolation$Gender<-recode(Interpolation_data$Gender,"1='Male';2='Female'")  
     Interpolation$Gender<-factor(Interpolation$Gender,
                                  levels = c("Male","Female"))
     table(Interpolation$Gender)
     colnames(Interpolation_data)
     #Race_ethnicity
     table(Clearn_data$Race_ethnicity)
     Interpolation$Race_ethnicity<-recode(Interpolation_data$Race_ethnicity,
                                          "1='Non-Hispanic White';2='Non-Hispanic Black';3='Hispanic';4='Other Race'")  
     Interpolation$Race_ethnicity<-factor(Interpolation$Race_ethnicity,
                                          levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other Race")) 
     table(Interpolation$Race_ethnicity)
     #Education_levels
     table(Clearn_data$Education_levels)
     Interpolation$Education_levels<-recode(Interpolation_data$Education_levels,
                                            "1='Less than high school';2='High school or Equivalent';3='College or above'")  
     Interpolation$Education_levels<-factor(Interpolation$Education_levels,
                                            levels = c("Less than high school","High school or Equivalent","College or above")) 
     table(Interpolation$Education_levels)
     
     # PIR 
     table(Clearn_data$PIR)
     Interpolation$PIR<-recode(Interpolation_data$PIR,
                               "1='(0, 1]';2='(1,4)';3='[4,inf)'")  
     Interpolation$PIR<-factor(Interpolation$PIR,
                               levels = c("(0, 1]","(1,4)","[4,inf)")) 
     table(Interpolation$PIR)
     
     #Health_insurance
     table(Clearn_data$Health_insurance)
     Interpolation$Health_insurance<-recode(Interpolation_data$Health_insurance,
                                            "1='No insurance';2='Public insurance';3='Private insurance'")  
     Interpolation$Health_insurance<-factor(Interpolation$Health_insurance,
                                            levels = c("No insurance","Public insurance","Private insurance")) 
     table(Interpolation$Health_insurance)
     
     #SEI
     table(Clearn_data$SEI)
     Interpolation$SEI<-recode(Interpolation_data$SEI,
                                            "1='Unemployment';2='Lower';3='Upper'")  
     Interpolation$SEI<-factor(Interpolation$SEI,
                                            levels = c("Unemployment","Lower","Upper")) 
     table(Interpolation$SEI)
     
     #Smoking_status
     table(Clearn_data$Smoking_status)
     Interpolation$Smoking_status<-recode(Interpolation_data$Smoking_status,
                                          "1='Never smoker';2='Former smoker';3='Current smoker'")  
     Interpolation$Smoking_status<-factor(Interpolation$Smoking_status,
                                          levels = c("Never smoker","Former smoker","Current smoker")) 
     table(Interpolation$Smoking_status)
     
     #Drinking_status
     table(Clearn_data$Drinking_status)
     Interpolation$Drinking_status<-recode(Interpolation_data$Drinking_status,
                                           "1='Non-drinker';2='Light/moderate drinker';3='Heavier drinker'")  
     Interpolation$Drinking_status<-factor(Interpolation$Drinking_status,
                                           levels = c("Non-drinker","Light/moderate drinker","Heavier drinker")) 
     table(Interpolation$Drinking_status)
     
     #Physical_status
     table(Clearn_data$Physical_status)
     Interpolation$Physical_status<-recode(Interpolation_data$Physical_status,
                                           "1='Inactive';2='Insufficient';3='Recommended'")  
     Interpolation$Physical_status<-factor(Interpolation$Physical_status,
                                           levels = c("Inactive","Insufficient","Recommended")) 
     table(Interpolation$Physical_status)
     
     #HEI
     table(Clearn_data$HEI)
     Interpolation$HEI<-recode(Interpolation_data$HEI,
                               "1='Quintile 1';2='Quintile 2';3='Quintile 3';4='Quintile 4';5='Quintile 5'")  
     Interpolation$HEI<-factor(Interpolation$HEI,
                               levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
     table(Interpolation$HEI)
     
     #BMI_Grade
     table(Clearn_data$BMI_Grade)
     Interpolation$BMI_Grade[Interpolation$BMI<25]<-'(0,25)'
     Interpolation$BMI_Grade[Interpolation$BMI>=25&Interpolation$BMI<30]<-'[25.0-30)'
     Interpolation$BMI_Grade[Interpolation$BMI>=30]<-'[30,inf)' 
     Interpolation$BMI_Grade<-factor(Interpolation$BMI_Grade,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
     table(Interpolation$BMI_Grade)
     
     #CVD_status
     table(Clearn_data$CVD_status)
     Interpolation$CVD_status<-recode(Interpolation_data$CVD_status,
                                      "1='NO';2='YES'")  
     Interpolation$CVD_status<-factor(Interpolation$CVD_status,
                                      levels = c("NO","YES")) 
     table(Interpolation$CVD_status)
     
     #Cancer_status
     table(Clearn_data$Cancer_status)
     Interpolation$Cancer_status<-recode(Interpolation_data$Cancer_status,
                                         "1='NO';2='YES'")  
     Interpolation$Cancer_status<-factor(Interpolation$Cancer_status,
                                         levels = c("NO","YES")) 
     table(Interpolation$Cancer_status)
     
     #Br_diabetes_status
     table(Clearn_data$Br_diabetes_status)
     Interpolation$Br_diabetes_status<-recode(Interpolation_data$Br_diabetes_status,
                                              "1='NO';2='YES'")  
     Interpolation$Br_diabetes_status<-factor(Interpolation$Br_diabetes_status,
                                              levels = c("NO","YES")) 
     table(Interpolation$Br_diabetes_status)
     
     #Medicine_use 
     table(Clearn_data$Medicine_use)
     Interpolation$Medicine_use<-recode(Interpolation_data$Medicine_use,
                                        "1='NO';2='YES'")  
     Interpolation$Medicine_use<-factor(Interpolation$Medicine_use,
                                        levels = c("NO","YES")) 
     table(Interpolation$Medicine_use)
     
     #HbA1c_status
     table(Clearn_data$HbA1c_status)
     Interpolation$HbA1c_status[Interpolation$HbA1c<7]<-"HbA1c<7"
     Interpolation$HbA1c_status[Interpolation$HbA1c>=7]<-"HbA1c>=7"
     Interpolation$HbA1c_status<-factor(Interpolation$HbA1c_status,
                                        levels = c("HbA1c<7","HbA1c>=7")) 
     table(Interpolation$HbA1c_status)
     
     #duration_status
     table(Clearn_data$duration_status)
     Interpolation$duration_status<-recode(Interpolation_data$duration_status,
                                           "1='less than 3 years';2='more than 3 years'")  
     Interpolation$duration_status<-factor(Interpolation$duration_status,
                                           levels = c("less than 3 years","more than 3 years")) 
     table(Interpolation$duration_status)
     
     #weight
     Interpolation$sdmvpsu<-Interpolation_data$sdmvpsu
     Interpolation$sdmvstra<-Interpolation_data$sdmvstra
     Interpolation$weight<-Interpolation_data$weight
     save(Interpolation,file="G:/paper_2_PD&DM/data_DC/Interpolation.Rdata")
   }
   {#** section 16.2.2 Complete data ####
     colApply <- function(dat, cols = colnames, func = as.numeric) {
       dat[cols] <- lapply(dat[cols], func)
       return(dat)
     }
     colnames<-colnames(Complete_data)
     Complete_data<-colApply(Complete_data,colnames, as.numeric)
     
     library(car)  
     Complete<-cbind(rownames(Complete_data),
                     Complete_data[,colnames(Complete_data)[c(1:4)]])
     colnames(Complete)[1]<-"ID"
     
     #AGE
     table(Clearn_data$Age_status)
     Complete$Age_status[Complete_data$Age<45]<-"<45"
     Complete$Age_status[Complete_data$Age>=45&Complete$Age<65]<-"[45,65)"
     Complete$Age_status[Complete_data$Age>=65]<-">=65"
     Complete$Age_status<-factor(Complete$Age_status,
                                 levels = c("<45","[45,65)",">=65"))
     table(Complete$Age_status)
     
     #Gender
     table(Clearn_data$Gender)
     Complete$Gender<-recode(Complete_data$Gender,"1='Male';2='Female'")  
     Complete$Gender<-factor(Complete$Gender,
                             levels = c("Male","Female"))
     table(Complete$Gender)
     colnames(Complete_data)
     
     #Race_ethnicity
     table(Clearn_data$Race_ethnicity)
     Complete$Race_ethnicity<-recode(Complete_data$Race_ethnicity,
                                     "1='Non-Hispanic White';2='Non-Hispanic Black';3='Hispanic';4='Other Race'")  
     Complete$Race_ethnicity<-factor(Complete$Race_ethnicity,
                                     levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other Race")) 
     table(Complete$Race_ethnicity)
     
     #Education_levels
     table(Clearn_data$Education_levels)
     Complete$Education_levels<-recode(Complete_data$Education_levels,
                                       "1='Less than high school';2='High school or Equivalent';3='College or above'")  
     Complete$Education_levels<-factor(Complete$Education_levels,
                                       levels = c("Less than high school","High school or Equivalent","College or above")) 
     table(Complete$Education_levels)
     
     # PIR 
     table(Clearn_data$PIR)
     Complete$PIR<-recode(Complete_data$PIR,
                          "1='(0, 1]';2='(1,4)';3='[4,inf)'")  
     Complete$PIR<-factor(Complete$PIR,
                          levels = c("(0, 1]","(1,4)","[4,inf)")) 
     table(Complete$PIR)
     
     #Health_insurance
     table(Clearn_data$Health_insurance)
     Complete$Health_insurance<-recode(Complete_data$Health_insurance,
                                       "1='No insurance';2='Public insurance';3='Private insurance'")  
     Complete$Health_insurance<-factor(Complete$Health_insurance,
                                       levels = c("No insurance","Public insurance","Private insurance")) 
     table(Complete$Health_insurance)
     
     #SEI
     table(Clearn_data$SEI)
     Complete$SEI<-recode(Complete_data$Health_insurance,
                                       "1='Unemployment';2='Lower';3='Upper'")  
     Complete$SEI<-factor(Complete$Health_insurance,
                                       levels = c("Unemployment","Lower","Upper")) 
     table(Complete$SEI)
     
     #Smoking_status
     table(Clearn_data$Smoking_status)
     Complete$Smoking_status<-recode(Complete_data$Smoking_status,
                                     "1='Never smoker';2='Former smoker';3='Current smoker'")  
     Complete$Smoking_status<-factor(Complete$Smoking_status,
                                     levels = c("Never smoker","Former smoker","Current smoker")) 
     table(Complete$Smoking_status)
     
     #Drinking_status
     table(Clearn_data$Drinking_status)
     Complete$Drinking_status<-recode(Complete_data$Drinking_status,
                                      "1='Non-drinker';2='Light/moderate drinker';3='Heavier drinker'")  
     Complete$Drinking_status<-factor(Complete$Drinking_status,
                                      levels = c("Non-drinker","Light/moderate drinker","Heavier drinker")) 
     table(Complete$Drinking_status)
     
     #Physical_status
     table(Clearn_data$Physical_status)
     Complete$Physical_status<-recode(Complete_data$Physical_status,
                                      "1='Inactive';2='Insufficient';3='Recommended'")  
     Complete$Physical_status<-factor(Complete$Physical_status,
                                      levels = c("Inactive","Insufficient","Recommended")) 
     table(Complete$Physical_status)
     
     #HEI
     table(Clearn_data$HEI)
     Complete$HEI<-recode(Complete_data$HEI,
                          "1='Quintile 1';2='Quintile 2';3='Quintile 3';4='Quintile 4';5='Quintile 5'")  
     Complete$HEI<-factor(Complete$HEI,
                          levels = c("Quintile 1","Quintile 2","Quintile 3","Quintile 4","Quintile 5")) 
     table(Complete$HEI)
     
     #BMI_Grade
     table(Clearn_data$BMI_Grade)
     Complete$BMI_Grade[Complete$BMI<25]<-'(0,25)'
     Complete$BMI_Grade[Complete$BMI>=25&Complete$BMI<30]<-'[25.0-30)'
     Complete$BMI_Grade[Complete$BMI>=30]<-'[30,inf)' 
     Complete$BMI_Grade<-factor(Complete$BMI_Grade,
                                     levels = c("(0,25)","[25.0-30)","[30,inf)")) 
     table(Complete$BMI_Grade)
     
     #CVD_status
     table(Clearn_data$CVD_status)
     Complete$CVD_status<-recode(Complete_data$CVD_status,
                                 "1='NO';2='YES'")  
     Complete$CVD_status<-factor(Complete$CVD_status,
                                 levels = c("NO","YES")) 
     table(Complete$CVD_status)
     
     #Cancer_status
     table(Clearn_data$Cancer_status)
     Complete$Cancer_status<-recode(Complete_data$Cancer_status,
                                    "1='NO';2='YES'")  
     Complete$Cancer_status<-factor(Complete$Cancer_status,
                                    levels = c("NO","YES")) 
     table(Complete$Cancer_status)
     
     #Br_diabetes_status
     table(Clearn_data$Br_diabetes_status)
     Complete$Br_diabetes_status<-recode(Complete_data$Br_diabetes_status,
                                         "1='NO';2='YES'")  
     Complete$Br_diabetes_status<-factor(Complete$Br_diabetes_status,
                                         levels = c("NO","YES")) 
     table(Complete$Br_diabetes_status)
     
     #Medicine_use 
     table(Clearn_data$Medicine_use)
     Complete$Medicine_use<-recode(Complete_data$Medicine_use,
                                   "1='NO';2='YES'")  
     Complete$Medicine_use<-factor(Complete$Medicine_use,
                                   levels = c("NO","YES")) 
     table(Complete$Medicine_use)
     
     #HbA1c_status
     table(Clearn_data$HbA1c_status)
     Complete$HbA1c_status[Complete$HbA1c<7]<-"HbA1c<7"
     Complete$HbA1c_status[Complete$HbA1c>=7]<-"HbA1c>=7"
     Complete$HbA1c_status<-factor(Complete$HbA1c_status,
                                   levels = c("HbA1c<7","HbA1c>=7")) 
     table(Complete$HbA1c_status)
     
     #duration_status
     table(Clearn_data$duration_status)
     Complete$duration_status<-recode(Complete_data$duration_status,
                                      "1='less than 3 years';2='more than 3 years'")  
     Complete$duration_status<-factor(Complete$duration_status,
                                      levels = c("less than 3 years","more than 3 years")) 
     table(Complete$duration_status)
     
     #weight
     Complete$sdmvpsu<-Complete_data$sdmvpsu
     Complete$sdmvstra<-Complete_data$sdmvstra
     Complete$weight<-Complete_data$weight
     save(Complete,file="G:/paper_2_PD&DM/data_DC/Complete.Rdata")
   }

   {#** section 16.2.3 Combine data
     Character_weighted<-merge(Clearn_data[,1:9],Character,by = "ID",all.y = T)
     Interpolation_weighted<-merge(Clearn_data[,1:9],Interpolation,by = "ID",all.y = T)
     Complete_weighted<-merge(Clearn_data[,1:9],Complete,by = "ID",all.y = T)
     save(Character_weighted,file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
     save(Interpolation_weighted,file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
     save( Complete_weighted,file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
     record<-ls()
     rm(list=record[-which(record=='Character_weighted'|record=='Interpolation_weighted'|
                             record=='Complete_weighted')])
   }
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 17 Processed based on reviewer comments ####
  load(file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
  
  Character_weighted$HPL_status<-NULL
  Interpolation_weighted$HPL_status<-NULL
  Complete_weighted$HPL_status<-NULL
  
  Character_weighted$HTN_status<-NULL
  Interpolation_weighted$HTN_status<-NULL
  Complete_weighted$HTN_status<-NULL
  
  Character_weighted$CKD_status<-NULL
  Interpolation_weighted$CKD_status<-NULL
  Complete_weighted$CKD_status<-NULL
  
  Interpolation_weighted$MORT_stat<-recode(Interpolation_weighted$MORT_stat,
                                       "'Alive'=0;'Deceased'=1")
  
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading=="CVD"]<-1
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$ucod_leading!="CVD"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$CVD_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading=="Cancer"]<-1
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$ucod_leading!="Cancer"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$Cancer_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$ucod_leading=="DM"]<-1
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$ucod_leading!="DM"&Interpolation_weighted$MORT_stat==1]<-2
  Interpolation_weighted$DM_MORT_stat[Interpolation_weighted$MORT_stat==0]<-0
  
  Character_weighted$MORT_stat<-recode(Character_weighted$MORT_stat,
                                           "'Alive'=0;'Deceased'=1")
  Character_weighted$CVD_MORT_stat[Character_weighted$ucod_leading=="CVD"]<-1
  Character_weighted$CVD_MORT_stat[Character_weighted$ucod_leading!="CVD"&Character_weighted$MORT_stat==1]<-2
  Character_weighted$CVD_MORT_stat[Character_weighted$MORT_stat==0]<-0
  
  Character_weighted$Cancer_MORT_stat[Character_weighted$ucod_leading=="Cancer"]<-1
  Character_weighted$Cancer_MORT_stat[Character_weighted$ucod_leading!="Cancer"&Character_weighted$MORT_stat==1]<-2
  Character_weighted$Cancer_MORT_stat[Character_weighted$MORT_stat==0]<-0
  
  Character_weighted$DM_MORT_stat[Character_weighted$ucod_leading=="DM"]<-1
  Character_weighted$DM_MORT_stat[Character_weighted$ucod_leading!="DM"&Character_weighted$MORT_stat==1]<-2
  Character_weighted$DM_MORT_stat[Character_weighted$MORT_stat==0]<-0
  
  Complete_weighted$MORT_stat<-recode(Complete_weighted$MORT_stat,
                                       "'Alive'=0;'Deceased'=1")
  Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading=="CVD"]<-1
  Complete_weighted$CVD_MORT_stat[Complete_weighted$ucod_leading!="CVD"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$CVD_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading=="Cancer"]<-1
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$ucod_leading!="Cancer"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$Cancer_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  Complete_weighted$DM_MORT_stat[Complete_weighted$ucod_leading=="DM"]<-1
  Complete_weighted$DM_MORT_stat[Complete_weighted$ucod_leading!="DM"&Complete_weighted$MORT_stat==1]<-2
  Complete_weighted$DM_MORT_stat[Complete_weighted$MORT_stat==0]<-0
  
  
  save(Character_weighted,file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
  save(Interpolation_weighted,file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  save( Complete_weighted,file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")

  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
  # >>>>> section 18 latent class analysis  ####
  load(file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
  Interpolation_SES<-Interpolation_weighted[,c("ID","Education_levels","PIR","Health_insurance","SEI")]
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
  Figure_S2A<-ggplot(posterior,max.overlaps = Inf) +
  geom_point(aes(class1_probabilities, class2_probabilities, color = class),size=2.5,alpha=0.5) +
  theme_bw()+  scale_color_nejm()+ scale_fill_nejm()+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_S2A.pdf",Figure_S2A, device = "pdf",width = 8, height = 6, units ="in",
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
  table()
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_S2B.pdf",Figure_S2B,device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  Interpolation_weighted$SES[M3$predclass==1]<-"high"
  Interpolation_weighted$SES[M3$predclass==2]<-"medium"
  Interpolation_weighted$SES[M3$predclass==3]<-"low"
  table(Interpolation_weighted$PIR,Interpolation_weighted$SES)
  Interpolation_weighted$SES<-factor(Interpolation_weighted$SES,
                             levels = c("low","medium","high"))
  save(Interpolation_weighted,file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Multiple interpolation data (Table1)  ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
         "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
         "BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
         "HbA1c_status","duration_status","Medicine_use")
VAR_ALL<-c("MORT_stat","WBC","CAL_mean","PPD_mean","DMFT","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
             "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
             "BMI","BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
             "HbA1c","HbA1c_status","duration_status","duration_status","Medicine_use")
{ #** section 19.1 OVER ALL ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
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
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
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
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,2),
                          'SE' = round(svymean$SE*100,2) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,2),
                          'SE' = round(svymean$SE,2))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR_ALL, model))
}
Table1<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
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
  Table1<-merge(Table1,test_data,by="Covariates",all = T)
  write.table(Table1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table1.csv")

  
  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 Original data (Table S2)  ####
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
  Character_weighted$SES<-Interpolation_weighted$SES
  table(Character_weighted$PD_diagnosis)
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Character_weighted,strata=~sdmvstra,weights = ~ weight)
  var<-c("MORT_stat","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
         "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
         "BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
         "HbA1c_status","duration_status","Medicine_use")
  VAR_ALL<-c("MORT_stat","WBC","CAL_mean","PPD_mean","DMFT","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
             "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
             "BMI","BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
             "HbA1c","HbA1c_status","duration_status","duration_status","Medicine_use")
  { #** section 19.1 OVER ALL ####
    model<- function(x){
      
      if( x %in% var ) {
        Covariates<-as.formula(paste0("~",x))
        unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
        svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
        model <- data.frame('Covariates'=x,
                            'grade' = gsub(x,"",rownames(svymean)),
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
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
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
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
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
                            'Mean' =round(svymean$mean,2),
                            'SE' = round(svymean$SE,2))
        return(model)
      }
    }  
    PD<- ldply(lapply(VAR_ALL, model))
  }
  TableS1<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
  save(TableS1,file = "G:/paper_2_PD&DM/data_DC/TableS1_Rdata")  
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
  load(file = "G:/paper_2_PD&DM/data_DC/TableS1_Rdata")
  TableS1<-merge(TableS1,test_data,by="Covariates",all = T)
  write.table(TableS1,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS1.csv")  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
  # >>>>> section 21 Complete data (Table S2)   ####
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
  SES<-Interpolation_weighted[,c("ID","SES")]
  Complete_weightedSES<-merge(Complete_weighted,SES,by = "ID",all.x = T)
  table(Complete_weightedSES$PD_diagnosis)
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Complete_weightedSES,strata=~sdmvstra,weights = ~ weight)
  var<-c("MORT_stat","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
         "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
         "BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
         "HbA1c_status","duration_status","Medicine_use")
  VAR_ALL<-c("MORT_stat","WBC","CAL_mean","PPD_mean","DMFT","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
             "Health_insurance","SES","Smoking_status","Drinking_status","Physical_status","HEI",
             "BMI","BMI_Grade","CVD_status","Cancer_status","Br_diabetes_status",
             "HbA1c","HbA1c_status","duration_status","duration_status","Medicine_use")
  { #** section 19.1 OVER ALL ####
    model<- function(x){
      
      if( x %in% var ) {
        Covariates<-as.formula(paste0("~",x))
        unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
        svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
        model <- data.frame('Covariates'=x,
                            'grade' = gsub(x,"",rownames(svymean)),
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
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
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
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
                            'counts'=unwtd_count[2],
                            'Mean' = round(svymean$mean*100,2),
                            'SE' = round(svymean$SE*100,2) )
        return(model)
      } else {
        
        Covariates<-as.formula(paste0("~",x))
        svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
        colnames(svymean)[2]<-"SE"
        model <- data.frame('Covariates'=x,
                            'grade' ="Mean ± SE",
                            'counts'=' ',
                            'Mean' =round(svymean$mean,2),
                            'SE' = round(svymean$SE,2))
        return(model)
      }
    }  
    PD<- ldply(lapply(VAR_ALL, model))
  }
  TableS2<-cbind(Over_all,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
  save(TableS2,file = "G:/paper_2_PD&DM/data_DC/TableS2_Rdata")  
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
  load(file = "G:/paper_2_PD&DM/data_DC/TableS2_Rdata")
  TableS2<-merge(TableS2,test_data,by="Covariates",all = T)
  write.table(TableS2,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS2.csv")  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 Kaplan-Meier (Figure 1) ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
{ #* ALL Mortality Figure 1A ####
  FIT<-svykm(Surv(peryear,MORT_stat==1)~PD_diagnosis, design=rhcSvy)
  FIT_ALL_PD<-survfit(Surv(peryear, MORT_stat==1) ~ PD_diagnosis, Interpolation_weighted,weights = Interpolation_weighted$weight)
  Figure_1A<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                   
                   break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                   xlab ="Time in Year",  pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
  Figure_1A$plot = Figure_1A$plot +ggplot2::annotate("text",x =4, y = 0.2,label = "P < 0.001",size=5)
  Figure_1A
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1A.pdf",  device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
} 
{ #* CVD Mortality Figure 1B #####
  rhcSvy_CVD<-subset(rhcSvy,CVD_status=="NO")
  FIT_ALL_PD<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ PD_diagnosis, Interpolation_weighted, weights = Interpolation_weighted$weight)
  FIT_ALL_PD
  Figure_1B<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                   
                   break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                   xlab ="Time in Year", pval = , legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
  Figure_1B$plot = Figure_1B$plot +ggplot2::annotate("text",x =4, y = 0.2,label = "P = 0.058",size=5)
  Figure_1B
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1B.pdf",  device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
  
}  
{ #* CANCER Mortality Figure 1C #####
  #CANCER
  FIT_ALL_PD<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ PD_diagnosis, Interpolation_weighted, weights = Interpolation_weighted$weight)
  Figure_1C<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                   
                   break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                   xlab ="Time in Year", pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
  Figure_1C$plot = Figure_1C$plot +ggplot2::annotate("text",x =4, y = 0.2,label = "P = 0.170",size=5)
  Figure_1C
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1C.pdf", device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
}   
{ #* DM Mortality #####  
  FIT_ALL_PD<-survfit(Surv(peryear, DM_MORT_stat==1) ~ PD_diagnosis,Interpolation_weighted, weights =Interpolation_weighted$weight)
  FIT_ALL_PD
  Figure_1D<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                   
                   break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                   xlab ="Time in Year", pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
  Figure_1D$plot = Figure_1D$plot +ggplot2::annotate("text",x =4, y = 0.2,label = "P = 0.420",size=5)
  Figure_1D
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1D.pdf",device = "pdf", width = 8, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
}
  { #* CIF ##### 

  library(cmprsk)
  load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==0]<-"0"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$MORT_stat==1]<-"4"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$CVD_MORT_stat==1]<-"1"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$Cancer_MORT_stat==1]<-"2"
  Interpolation_weighted$ALL_MORT_stat[Interpolation_weighted$DM_MORT_stat==1]<-"3"
  CIF<-cuminc(Interpolation_weighted$peryear,Interpolation_weighted$ALL_MORT_stat,Interpolation_weighted$PD_diagnosis,0,strata=Interpolation_weighted$sdmvstra,rho =Interpolation_weighted$weight) 
  #CIF<-cuminc(ftime=time,fstatus=cause,group=stage,cencode=0)
  
  CIF #print(CIF)
  NM_periodontitis_CVD<- as.data.frame(CIF[["No/Mild periodontitis 1"]])
  NM_periodontitis_CVD$type<-"No/Mild periodontitis 1"
  NM_periodontitis_CVD$group<-"CVD"
  NM_periodontitis_CVD$line<-"No/Mild periodontitis"
  MS_periodontitis_CVD<-as.data.frame(CIF[["Moderate/Severe periodontitis 1"]])
  MS_periodontitis_CVD$type<-"Moderate/Severe periodontitis 1"
  MS_periodontitis_CVD$group<-"CVD"
  MS_periodontitis_CVD$line<-"Moderate/Severe periodontitis"
  NM_periodontitis_Cancer<-as.data.frame(CIF[["No/Mild periodontitis 2"]])
  NM_periodontitis_Cancer$type<-"No/Mild periodontitis 2"
  NM_periodontitis_Cancer$group<-"Cancer"
  NM_periodontitis_Cancer$line<-"No/Mild periodontitis"
  MS_periodontitis_Cancer<-as.data.frame(CIF[["Moderate/Severe periodontitis 2"]])
  MS_periodontitis_Cancer$type<-"Moderate/Severe periodontitis 2"
  MS_periodontitis_Cancer$group<-"Cancer"
  MS_periodontitis_Cancer$line<-"Moderate/Severe periodontitis"
  NM_periodontitis_DM<-as.data.frame(CIF[["No/Mild periodontitis 3"]])
  NM_periodontitis_DM$type<-"No/Mild periodontitis 3"
  NM_periodontitis_DM$group<-"Diabetes"
  NM_periodontitis_DM$line<-"No/Mild periodontitis"
  MS_periodontitis_DM<-as.data.frame(CIF[["Moderate/Severe periodontitis 3"]])
  MS_periodontitis_DM$type<-"Moderate/Severe periodontitis 3"
  MS_periodontitis_DM$group<-"Diabetes"
  MS_periodontitis_DM$line<-"Moderate/Severe periodontitis"
  NM_periodontitis_Other<-as.data.frame(CIF[["No/Mild periodontitis 4"]])
  NM_periodontitis_Other$type<-"No/Mild periodontitis 4"
  NM_periodontitis_Other$group<-"Others"
  NM_periodontitis_Other$line<-"No/Mild periodontitis"
  MS_periodontitis_Other<-as.data.frame(CIF[["Moderate/Severe periodontitis 4"]])
  MS_periodontitis_Other$type<-"Moderate/Severe periodontitis 4"
  MS_periodontitis_Other$line<-"Moderate/Severe periodontitis"
  MS_periodontitis_Other$group<-"Others"
  data<-rbind(NM_periodontitis_CVD,MS_periodontitis_CVD,NM_periodontitis_Cancer,MS_periodontitis_Cancer,
              NM_periodontitis_DM,MS_periodontitis_DM,NM_periodontitis_Other,MS_periodontitis_Other)
   #图例命名
  Figure_1E<- ggplot(data, aes(x=time, y=est, group=type,colour=group)) + geom_line(aes(linetype=line), size=1)+ theme_bw(base_size = 12)+
    theme(axis.text = element_text(size=12),
          axis.title=element_text(size=15),
          legend.text=element_text(size=12),
      panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
    scale_color_manual(values=c("#0073C2FF", "#EFC000FF", "#CD534CFF","#8686864C"))+ 
    ylab("Cumulative incidence function") + xlab("Time in year") 
  Figure_1E
  ggsave("G:/paper_2_PD&DM/data_DC/Figure_1E.pdf",Figure_1E,device = "pdf", width = 12, height = 6, units ="in",
         dpi = 600, limitsize = TRUE)
}
  mypal <- pal_jco("default", alpha = 0.3)(9)
  mypal
  library("scales")
  show_col(mypal)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 All-cause and cause-specific mortality  ####  
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD

}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
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
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table3<-result.all.cause
  write.table(Table3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table3.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative mortality rates (Table S3) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
Interpolation_weighted<-subset(Interpolation_weighted,Gender=="Male")


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
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_stat=="NO",useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,Interpolation_weighted$CVD_stat=="NO",useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_stat=="NO",useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,Interpolation_weighted$Cancer_stat=="NO",useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
#DM.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))

TableS3_1<-c("Outcome",NA,"No/Mild periodontitis",NA,"Moderate/Severe periodontitis")
TableS3_2<-c(NA,"Events, n/N","Incidence Rate (95% CI)","Events, n/N", "Incidence Rate (95% CI)")
TableS3_3<-c("All-cause mortality",total.counts[1,1],noPD_ALL_Incidence,total.counts[1,2],PD_ALL_Incidence)
TableS3_4<-c("CVD mortality",total.counts[2,1],noPD_CVD_Incidence,total.counts[2,2],PD_CVD_Incidence)
TableS3_5<-c("Cancer mortality",total.counts[3,1],noPD_Cancer_Incidence,total.counts[3,2],PD_Cancer_Incidence)
TableS3_6<-c("DM mortality",total.counts[4,1],noPD_DM_Incidence,total.counts[2,2],PD_DM_Incidence)
TableS3<-as.data.frame(rbind(TableS3_1,TableS3_2,TableS3_3,TableS3_4,TableS3_5,TableS3_6))
TableS3
write.table(TableS3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS3.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 Stratified analyses (Table 3) ####  
{ #* Data Collation #####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
ALL_Weight_plus<-Interpolation_weighted[,c("PD_diagnosis","MORT_stat","CVD_MORT_stat","CVD_status","peryear",
                                           "Age_status","Gender","Race_ethnicity","SES",
                                           "BMI_Grade","Smoking_status","Drinking_status","HEI")]
table(ALL_Weight_plus$Age_status,useNA = "ifany")
ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age_status=="<45"]<-"<45"
ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age_status=="[45,65)"|ALL_Weight_plus$Age_status==">=65"]<-">=45"

ALL_Weight_plus$Age_sub<-factor(ALL_Weight_plus$Age_sub,
       levels = c("<45",">=45")) 
table(ALL_Weight_plus$Age_sub)
#RACE
table(ALL_Weight_plus$Race_ethnicity)
ALL_Weight_plus$Race_sub<-"Other_Race"
ALL_Weight_plus$Race_sub[ALL_Weight_plus$Race_ethnicity=="Non-Hispanic White"]<-"Non-Hispanic white"
table(ALL_Weight_plus$Race_sub)
ALL_Weight_plus$Race_sub<-as.factor(ALL_Weight_plus$Race_sub)
ALL_Weight_plus$Race_sub<-factor(ALL_Weight_plus$Race_sub,
                                levels = c("Non-Hispanic white","Other_Race")) 
table(ALL_Weight_plus$Race_sub)
#BMI
table(ALL_Weight_plus$BMI_Grade)
ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI_Grade=="(0,25)"|ALL_Weight_plus$BMI_Grade=="[25.0-30)"]<-"<30"
ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI_Grade=="[30,inf)"]<-">=30"
ALL_Weight_plus$BMI_sub<-factor(ALL_Weight_plus$BMI_sub,
                                 levels = c("<30",">=30"))
table(ALL_Weight_plus$BMI_sub)
#smoke
ALL_Weight_plus$Smoking_sub<-"Former/Current smoker"
ALL_Weight_plus$Smoking_sub[ALL_Weight_plus$Smoking_status=="Never smoker"]<-"Never smoker"
ALL_Weight_plus$Smoking_sub<-factor(ALL_Weight_plus$Smoking_sub,
                                levels = c("Never smoker","Former/Current smoker"))
table(ALL_Weight_plus$Smoking_sub)

#Drinking_status
table(ALL_Weight_plus$Drinking_status)
ALL_Weight_plus$Drinking_sub<-"drinker"
ALL_Weight_plus$Drinking_sub[ALL_Weight_plus$Drinking_status=="Non-drinker"]<-"Non-drinker"
ALL_Weight_plus$Drinking_sub<-factor(ALL_Weight_plus$Drinking_sub,
                                    levels = c("Non-drinker","drinker"))
table(ALL_Weight_plus$Drinking_sub)
#SES
table(ALL_Weight_plus$SES)
ALL_Weight_plus$SES_sub<-"medium/high"
ALL_Weight_plus$SES_sub[ALL_Weight_plus$SES=="low"]<-"low"
ALL_Weight_plus$SES_sub<-factor(ALL_Weight_plus$SES_sub,
                                  levels = c("low","medium/high"))
table(ALL_Weight_plus$SES_sub)

#HEI
table(ALL_Weight_plus$HEI)
ALL_Weight_plus$HEI_sub[ALL_Weight_plus$HEI=="Quintile 1"|ALL_Weight_plus$HEI=="Quintile 2"]<-"Quintile 1-2"
ALL_Weight_plus$HEI_sub[ALL_Weight_plus$HEI=="Quintile 3"|ALL_Weight_plus$HEI=="Quintile 4"|
                          ALL_Weight_plus$HEI=="Quintile 5"]<-"Quintile 3-5"
ALL_Weight_plus$HEI_sub<-factor(ALL_Weight_plus$HEI_sub,
                                levels = c("Quintile 1-2","Quintile 3-5"))
ALL_Weight_plus$sdmvpsu<-Interpolation_weighted$sdmvpsu
ALL_Weight_plus$sdmvstra<-Interpolation_weighted$sdmvstra
ALL_Weight_plus$weight<-Interpolation_weighted$weight
}
{ #* AGE #####
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_CVD<-subset(rhcSvy,CVD_status=="NO")
rhcSvy_45<-subset(rhcSvy,Age_sub=="<45")
rhcSvy_45_CVD<-subset(rhcSvy,Age_sub=="<45"&CVD_status=="NO")
rhcSvy_80<-subset(rhcSvy,Age_sub==">=45")
rhcSvy_80_CVD<-subset(rhcSvy,Age_sub==">=45"&CVD_status=="NO")
svytable(~CVD_status+Age_sub,rhcSvy_80)
colnames(ALL_Weight_plus)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_45)
Age45<-summary(MODEL_ALL_inter)
P<-Age45[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age45[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Age","group"="<45",'status'="all cause")
Age1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_45_CVD)
Age45<-summary(MODEL_ALL_inter)
P<-Age45[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age45[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"="<45",'status'="CVD cause")
Age.all<-rbind(Age1,Age2)
Age.all
#45-80
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_80)
Age80<-summary(MODEL_ALL_inter)
P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Age","group"=">=45",'status'="all cause")
Age3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_80_CVD)
Age80<-summary(MODEL_ALL_inter)
P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"=">=45",'status'="CVD cause")
Age.all<-rbind(Age.all,Age3,Age4)
Age.all

#epiR
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_sub+PD_diagnosis*Age_sub+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Age5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"="interaction",'status'="all cause")
Age.all<-rbind(Age.all,Age5)
Age.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_sub+PD_diagnosis*Age_sub+Gender+
                            Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Age6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"="interaction",'status'="CVD cause")
Age.all<-rbind(Age.all,Age6)
Age.all
}
{ #* RACE #####
table(ALL_Weight_plus$Race_sub)
rhcSvy_white<-subset(rhcSvy,Race_sub=="Non-Hispanic white")
rhcSvy_white_CVD<-subset(rhcSvy,Race_sub=="Non-Hispanic white"&CVD_status=="NO")
rhcSvy_Other<-subset(rhcSvy,Race_sub=="Other_Race")
rhcSvy_Other_CVD<-subset(rhcSvy,Race_sub=="Other_Race"&CVD_status=="NO")

MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_white)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Race","group"="white",'status'="all cause")
Race1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_white_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="white",'status'="CVD cause")
Race2
Race.all<-rbind(Race1,Race2)
Race.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Other)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Race","group"="Other",'status'="all cause")
Race3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Other_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="Other",'status'="CVD cause")
Race4
Race.all<-rbind(Race.all,Race3,Race4)
Race.all
#epiR
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Race_sub+PD_diagnosis*Race_sub+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Race5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="interaction",'status'="all cause")
Race.all<-rbind(Race.all,Race5)
Race.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Race_sub+PD_diagnosis*Race_sub+Age_status+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Race6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="interaction",'status'="CVD cause")
Race.all<-rbind(Race.all,Race6)
Race.all
}

{#* gender #####
table(ALL_Weight_plus$Gender)
rhcSvy_Female<-subset(rhcSvy,Gender=="Female")
rhcSvy_Female_CVD<-subset(rhcSvy,Gender=="Female"&CVD_status=="NO")
rhcSvy_Male<-subset(rhcSvy,Gender=="Male")
rhcSvy_Male_CVD<-subset(rhcSvy,Gender=="Male"&CVD_status=="NO")

MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Female)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Gender","group"="Female",'status'="all cause")
Gender1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Female_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Gender","group"="Female",'status'="CVD cause")
Gender2
Gender.all<-rbind(Gender1,Gender2)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Male)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="Gender","group"="Male",'status'="all cause")
Gender3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_Male_CVD)
Genderwhite<-summary(MODEL_ALL_inter)
P<-Genderwhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Genderwhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Gender","group"="Male",'status'="CVD cause")
Gender4
Gender.all<-rbind(Gender.all,Gender3,Gender4)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Gender+PD_diagnosis*Gender+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Gender5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Gender","group"="interaction",'status'="all cause")
Gender.all<-rbind(Gender.all,Gender5)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Gender+PD_diagnosis*Gender+Age_status+Race_ethnicity+
                            SES+
                            Smoking_status+Drinking_status+HEI+
                            BMI_Grade, design =rhcSvy_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Gender6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Gender","group"="interaction",'status'="CVD cause")
Gender.all<-rbind(Gender.all,Gender6)
Gender.all
}
{#* BMI #####

colnames(ALL_Weight_plus)
table(ALL_Weight_plus$BMI_sub)
rhcSvy_low0<-subset(rhcSvy,BMI_sub=="<30")
rhcSvy_low0_CVD<-subset(rhcSvy,BMI_sub=="<30"&CVD_status=="NO")
rhcSvy_high0<-subset(rhcSvy,BMI_sub==">=30")
rhcSvy_high0_CVD<-subset(rhcSvy,BMI_sub==">=30"&CVD_status=="NO")
svytable(~CVD_MORT_stat+BMI_sub,rhcSvy_low0_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI
                            , design =rhcSvy_low0)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="BMI","group"="<30",'status'="all cause")
BMI1
svytable(~CVD_MORT_stat+BMI_sub,rhcSvy_low0_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI, design =rhcSvy_low0_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="BMI","group"="<30",'status'="CVD cause")
BMI2
BMI.all<-rbind(BMI1,BMI2)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI, design =rhcSvy_high0)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="BMI","group"=">=30",'status'="all cause")
BMI3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI, design =rhcSvy_high0_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="BMI","group"=">=30",'status'="CVD cause")
BMI4
BMI.all<-rbind(BMI.all,BMI3,BMI4)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+BMI_sub+PD_diagnosis*BMI_sub+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI, design =rhcSvy)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
BMI5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="BMI","group"="interaction",'status'="all cause")
BMI.all<-rbind(BMI.all,BMI5)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+BMI_sub+PD_diagnosis*BMI_sub+Age_status+Race_ethnicity+Gender+
                            SES+
                            Smoking_status+Drinking_status+HEI, design =rhcSvy_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
BMI6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="BMI","group"="interaction",'status'="CVD cause")
BMI.all<-rbind(BMI.all,BMI6)
BMI.all
}

{#* Smoking_sub #####
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$Smoking_sub)
rhcSvy_Current<-subset(rhcSvy,Smoking_sub=="Former/Current smoker")
rhcSvy_Current_CVD<-subset(rhcSvy,Smoking_sub=="Former/Current smoker"&CVD_status=="NO")
rhcSvy_Never<-subset(rhcSvy,Smoking_sub=="Never smoker")
rhcSvy_Never_CVD<-subset(rhcSvy,Smoking_sub=="Never smoker"&CVD_status=="NO")
svytable(~CVD_MORT_stat+Smoking_sub,rhcSvy_Current)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_Current)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Smoke","group"="Current",'status'="all cause")
Smoke1

MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_Current_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Smoke","group"="Current",'status'="CVD cause")
Smoke2
Smoke.all<-rbind(Smoke1,Smoke2)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_Never)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="Smoke","group"="Never",'status'="all cause")
Smoke3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_Never_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Smoke","group"="Never",'status'="CVD cause")
Smoke4
Smoke.all<-rbind(Smoke.all,Smoke3,Smoke4)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Smoking_sub+PD_diagnosis*Smoking_sub+Race_ethnicity+Gender+
                            Age_status+SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Smoke5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Smoke","group"="interaction",'status'="all cause")
Smoke.all<-rbind(Smoke.all,Smoke5)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Smoking_sub+PD_diagnosis*Smoking_sub+Race_ethnicity+Gender+
                            Age_status+SES+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Smoke6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Smoke","group"="interaction",'status'="CVD cause")
Smoke.all<-rbind(Smoke.all,Smoke6)
Smoke.all
}
{#* Drinking_sub #####
  colnames(ALL_Weight_plus)
  table(ALL_Weight_plus$Drinking_sub)
  rhcSvy_nondrinker<-subset(rhcSvy,Drinking_sub=='Non-drinker')
  rhcSvy_nondrinker_CVD<-subset(rhcSvy,Drinking_sub=='Non-drinker'&CVD_status=="NO")
  rhcSvy_drinker<-subset(rhcSvy,Drinking_sub=='drinker')
  rhcSvy_drinker_CVD<-subset(rhcSvy,Drinking_sub=='drinker'&CVD_status=="NO")
  svytable(~CVD_MORT_stat+Drinking_sub,rhcSvy_nondrinker_CVD)
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                              SES+BMI_Grade+HEI+Smoking_status, design =rhcSvy_nondrinker)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drinking_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'subgroup'="Drinking_sub","group"="YES",'status'="all cause")
  Drinking_sub1
  svytable(~CVD_MORT_stat+Drinking_sub,rhcSvy_nondrinker)
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                              SES+
                              BMI_Grade+HEI+Smoking_status, design =rhcSvy_nondrinker_CVD)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drinking_sub2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Drinking_sub","group"="YES",'status'="CVD cause")
  Drinking_sub2
  Drinking_sub.all<-rbind(Drinking_sub1,Drinking_sub2)
  Drinking_sub.all
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                              SES+
                              BMI_Grade+HEI+Smoking_status, design =rhcSvy_drinker)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drinking_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'subgroup'="Drinking_sub","group"="NO",'status'="all cause")
  Drinking_sub3
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                              SES+BMI_Grade+HEI+Smoking_status, design =rhcSvy_drinker_CVD)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  Drinking_sub4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Drinking_sub","group"="NO",'status'="CVD cause")
  Drinking_sub4
  Drinking_sub.all<-rbind(Drinking_sub.all,Drinking_sub3,Drinking_sub4)
  Drinking_sub.all
  MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Drinking_sub+PD_diagnosis*Drinking_sub+Race_ethnicity+Gender+
                              Age_status+SES+BMI_Grade+HEI+Smoking_status, design =rhcSvy)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Drinking_sub5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Drinking_sub","group"="interaction",'status'="all cause")
  Drinking_sub.all<-rbind(Drinking_sub.all,Drinking_sub5)
  Drinking_sub.all
  MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Drinking_sub+PD_diagnosis*Drinking_sub+Race_ethnicity+Gender+
                              Age_status+SES+
                              BMI_Grade+HEI+Smoking_status, design =rhcSvy_CVD)
  MODEL<-summary(MODEL_ALL_inter)
  P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
  HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
  Drinking_sub6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Drinking_sub","group"="interaction",'status'="CVD cause")
  Drinking_sub.all<-rbind(Drinking_sub.all,Drinking_sub6)
  Drinking_sub.all
}

{#* SES #####
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$SES_sub)
rhcSvy_low<-subset(rhcSvy,SES_sub=="low")
low<-subset(rhcSvy,SES_sub=="low"&CVD_status=="NO")
rhcSvy_high<-subset(rhcSvy,SES_sub=="medium/high")
rhcSvy_high_CVD<-subset(rhcSvy,SES_sub=="medium/high"&CVD_status=="NO")
svytable(~CVD_MORT_stat+SES_sub,rhcSvy_low)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            
                            BMI_Grade+Drinking_status+HEI+Smoking_status, design =rhcSvy_low)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
SES1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="SES","group"="low",'status'="all cause")
SES1
svytable(~CVD_MORT_stat+SES_sub,rhcSvy_low)
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            Smoking_status+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_low)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
SES2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="low",'status'="CVD cause")
SES2
SES.all<-rbind(SES1,SES2)
SES.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            Smoking_status+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_high)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
SES3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'subgroup'="SES","group"="high",'status'="all cause")
SES3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            Smoking_status+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_high_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
SES4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="high",'status'="CVD cause")
SES4
SES.all<-rbind(SES.all,SES3,SES4)
SES.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+SES_sub+PD_diagnosis*SES_sub+Race_ethnicity+Gender+
                            Age_status+Smoking_status+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
SES5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="interaction",'status'="all cause")
SES.all<-rbind(SES.all,SES5)
SES.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+SES_sub+PD_diagnosis*SES_sub+Race_ethnicity+Gender+
                            Age_status+Smoking_status+
                            BMI_Grade+Drinking_status+HEI, design =rhcSvy_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
SES6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'subgroup'="SES","group"="interaction",'status'="CVD cause")
SES.all<-rbind(SES.all,SES6)
SES.all
}

{#*HEI #####
rhcSvy_HEI1<-subset(rhcSvy,HEI_sub=="Quintile 1-2")
rhcSvy_HEI1_CVD<-subset(rhcSvy,HEI_sub=="Quintile 1-2"&CVD_status=="NO")
rhcSvy_HEI2<-subset(rhcSvy,HEI_sub=="Quintile 3-5")
rhcSvy_HEI2_CVD<-subset(rhcSvy,HEI_sub=="Quintile 3-5"&CVD_status=="NO")
svytable(~CVD_MORT_stat+HEI_sub,rhcSvy_HEI1_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy_HEI1)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HEI_sub1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="HEI_sub","group"="YES",'status'="all cause")
HEI_sub1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy_HEI1_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HEI_sub2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="YES",'status'="CVD cause")
HEI_sub2
HEI_sub.all<-rbind(HEI_sub1,HEI_sub2)
HEI_sub.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy_HEI2)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HEI_sub3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="HEI_sub","group"="NO",'status'="all cause")
HEI_sub3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+Age_status+Race_ethnicity+Gender+
                            SES+
                            BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy_HEI2_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HEI_sub4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="NO",'status'="CVD cause")
HEI_sub4
HEI_sub.all<-rbind(HEI_sub.all,HEI_sub3,HEI_sub4)
HEI_sub.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~PD_diagnosis+HEI_sub+PD_diagnosis*HEI_sub+Race_ethnicity+Gender+
                            Age_status+SES+
                            BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
HEI_sub5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="interaction",'status'="all cause")
HEI_sub.all<-rbind(HEI_sub.all,HEI_sub5)
HEI_sub.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~PD_diagnosis+HEI_sub+PD_diagnosis*HEI_sub+Race_ethnicity+Gender+
                            Age_status+SES+
                            BMI_Grade+Drinking_status+Smoking_status, design =rhcSvy_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
HEI_sub6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HEI_sub","group"="interaction",'status'="CVD cause")
HEI_sub.all<-rbind(HEI_sub.all,HEI_sub6)
HEI_sub.all
}


all.subgroup<-rbind(Age.all,Race.all,Gender.all,BMI.all,Smoke.all,Drinking_sub.all,SES.all,HEI_sub.all)
all.subgroup$HR<-round(all.subgroup$HR,2)
all.subgroup$lower..95<-round(all.subgroup$lower..95,2)
all.subgroup$upper..95<-round(all.subgroup$upper..95,2)
all.subgroup$P.value<-round(all.subgroup$P.value,3)
Table3<-all.subgroup
write.table(Table3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table3.csv")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

# >>>>> section 24 dose -response effect (Table S4) #### 
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$Periodontitis_diagnosis)
Interpolation_weighted$Periodontitis_diagnosis<-as.character(Interpolation_weighted$Periodontitis_diagnosis)
  Interpolation_weighted$Periodontitis[
    Interpolation_weighted$Periodontitis_diagnosis=="normal"|Interpolation_weighted$Periodontitis_diagnosis=="mild"
    ]<-"normal/mild"
  table(Interpolation_weighted$Periodontitis)
  Interpolation_weighted$Periodontitis<-factor(Interpolation_weighted$Periodontitis,
                                               levels = c("normal/mild","moderate","severe"))
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-result3
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.CVD <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-result3.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
  result3.Cancer <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-result3.Cancer
}

{ #* DM model #####
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1:2,"Pr(>|z|)"]
  HR<-model3_DM_result[["conf.int"]][1:2,c("exp(coef)","lower .95","upper .95")]
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
  TableS4<-result.all.cause
  write.table(TableS4,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS4.csv")
}
Interpolation_weighted$Periodontitis<-as.character(Interpolation_weighted$Periodontitis)
table(Interpolation_weighted$Periodontitis)
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="normal/mild"]<-0
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="moderate"]<-1
Interpolation_weighted$Periodontitis[Interpolation_weighted$Periodontitis=="severe"]<-2
Interpolation_weighted$Periodontitis<-as.numeric(Interpolation_weighted$Periodontitis)

{#* P for trend ####
  rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
  { #* all model #####
    model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                           Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
    model3_all_result<-summary(model3_all)
    
    P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="All cause")
    result.all<-result3
    result.all
  }
  
  { #* CVD model #####
    rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
    model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                           Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                           Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
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
                              Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                              Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
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
                          Periodontitis+Age_status+Gender+Race_ethnicity+SES+
                          Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
    model3_DM_result<-summary(model3_DM)
    
    P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
    HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
    result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
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
    TableS41<-result.all.cause
    write.table(Table3,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table3.csv")
  }
}

table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis)
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$MORT_stat)

table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_status=="NO")
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$CVD_MORT_stat,Interpolation_weighted$CVD_status=="NO")

table(Interpolation_weighted$Periodontitis,Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_status=="NO")
table(Interpolation_weighted$Periodontitis,Interpolation_weighted$Cancer_MORT_stat,Interpolation_weighted$Cancer_status=="NO")


table(Interpolation_weighted$Periodontitis,Interpolation_weighted$DM_MORT_stat)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

# >>>>> section 24 Sensitivity Analysis 1 cal(Table S5-1) ####  
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
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
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
                            CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        CAL_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        CAL_mean+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS5<-result.all.cause
  write.table(TableS5,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS5.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Sensitivity Analysis 1 ppd(Table S5-2) ####  
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
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
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
                            PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        PPD_mean+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PPD_mean+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS6<-result.all.cause
  write.table(TableS6,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS6.csv")
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 Sensitivity Analysis 1 dmft(Table S6) ####  
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all Crude
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model1
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT, design =rhcSvy_DM_CVD)
  model1_CVD_result<-summary(model1_CVD)
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result.CVD
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         DMFT+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
  svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT, design =rhcSvy_DM_Cancer)
  model1_Cancer_result<-summary(model1_Cancer)
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result.Cancer
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            DMFT+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
  result.Cancer
}

{ #* DM model #####
  #DM Crude
  model1_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        DMFT, design =rhcSvy)
  model1_DM_result<-summary(model1_DM)
  P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="DM cause")
  result
  #all model1
  model2_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        DMFT+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        DMFT+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  TableS6<-result.all.cause
  write.table(TableS6,sep = ",",file ="G:/paper_2_PD&DM/data_DC/TableS6.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 RCS(Figure S3) ####
library(survival)
library(survminer)
library(rms) #RCS
library(survminer)
library(caret)
setwd("G:/paper_2_PD&DM/data_DC")
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
RCS <- Interpolation_weighted
{#* All cause mortarity ####
  {#** DMFT ####
    pdf("DMFT_RCS.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"DMFT"],na.rm = T)+quantile(RCS[,"DMFT"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"DMFT"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"DMFT"],na.rm = T)
    RCS<- RCS[!RCS[,"DMFT"]>=limUp&!RCS[,"DMFT"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    RCS$MORT_stat<-as.character(RCS$MORT_stat)
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(DMFT,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(DMFT,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	11
    
    
    ddist$limits$BMI[2]<-refvalue
    
    pred_OR<-Predict(fit,DMFT,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$ DMFT)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "DMFT",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 1.5, paste0("ref value = ",refvalue)) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** CAL_mean ####
    pdf("CAL_RCS.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(CAL_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(CAL_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.60
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
    text(refvalue + 1, 1.5, paste0("ref value = ","1.60")) 
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
    
    pdf("PPD_RCS.pdf",width=6,height=5)
    colnames(RCS)
    RCS$MORT_stat<-as.numeric(RCS$MORT_stat)
    colnames(RCS)
    for (i in 3:7) {
      fit <- cph(Surv(peryear,MORT_stat) ~ rcs(PPD_mean,i)+Age_status+Gender+Race_ethnicity+SES+
                   Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS, weight= weight,family = binomial())
      tmp <- extractAIC(fit)
      if(i == 3) {AIC = tmp[2]; nk = 3}
      if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
    }
    nk
    i
    fit <-  cph(Surv(peryear,MORT_stat)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.58
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
    text(refvalue, 1.2, paste0("ref value = ","1.58")) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }

}
{#* CVD mortarity ####
  RCS<-subset(RCS,CVD_status=="NO")
  RCS$CVD_MORT_stat<-as.character(RCS$CVD_MORT_stat)
  RCS$CVD_MORT_stat<-as.numeric(RCS$CVD_MORT_stat)
  {#** DMFT CVD ####
    pdf("DMFT_RCS_CVD.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"DMFT"],na.rm = T)+quantile(RCS[,"DMFT"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"DMFT"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"DMFT"],na.rm = T)
    RCS<- RCS[!RCS[,"DMFT"]>=limUp&!RCS[,"DMFT"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]

    colnames(RCS)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(DMFT,4)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	11
    
    
    ddist$limits$BMI[2]<-refvalue
    
    pred_OR<-Predict(fit,DMFT,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$DMFT)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "DMFT",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 1.5, paste0("ref value = ",refvalue)) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** CAL_mean ####
    pdf("CAL_RCS_CVD.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(CAL_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.58
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
    text(refvalue, 0.7, paste0("ref value = ","1.60")) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  
  {#** PPD_mean ####
    RCS<-subset(Interpolation_weighted,CVD_status=="NO")
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
    
    pdf("CVD_PPD_RCS.pdf",width=6,height=5)

    fit <-  cph(Surv(peryear,CVD_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.60
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
    text(refvalue, 1.5, paste0("ref value = ","1.60")) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }

}
{#* Cancer mortarity ####
  RCS<-subset(Interpolation_weighted,Cancer_status=="NO")
  RCS$Cancer_MORT_stat<-as.character(RCS$Cancer_MORT_stat)
  RCS$Cancer_MORT_stat<-as.numeric(RCS$Cancer_MORT_stat)
  {#** DMFT ####
    pdf("DMFT_RCS_Cancer.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"DMFT"],na.rm = T)+quantile(RCS[,"DMFT"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"DMFT"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"DMFT"],na.rm = T)
    RCS<- RCS[!RCS[,"DMFT"]>=limUp&!RCS[,"DMFT"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    colnames(RCS)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(DMFT,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	11
    
    
    ddist$limits$BMI[2]<-refvalue
    
    pred_OR<-Predict(fit,DMFT,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$ DMFT)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "DMFT",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 1.5, paste0("ref value = ",refvalue)) 
    legend("topright",
           paste0(
                  "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** CAL_mean ####
    pdf("CAL_RCS_Cancer.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(CAL_mean,4)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.58
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
    text(refvalue, 0.7, paste0("ref value = ","1.60")) 
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
    
    pdf("Cancer_PPD_RCS.pdf",width=6,height=5)

    fit <-  cph(Surv(peryear,Cancer_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.60
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
    text(refvalue, 1.2, paste0("ref value = ","1.58")) 
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
  {#** DMFT ####
    pdf("DMFT_RCS_DM.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"DMFT"],na.rm = T)+quantile(RCS[,"DMFT"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"DMFT"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"DMFT"],na.rm = T)
    RCS<- RCS[!RCS[,"DMFT"]>=limUp&!RCS[,"DMFT"]<=limDown,]
    # limUp<-3*IQR(RCS[,18],na.rm = T)+quantile(RCS[,18],3/4,na.rm=T,names=F)
    # limDown<-quantile(RCS[,18],1/4,na.rm=T,names=F)-3*IQR(RCS[,18],na.rm = T)
    # RCS<- RCS[!RCS[,18]>=limUp&!RCS[,18]<=limDown,]
    
    colnames(RCS)
    ddist<-datadist(RCS)
    options(datadist="ddist")
    fit <-  cph(Surv(peryear,DM_MORT_stat==1)~rcs(DMFT,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	11
    
    
    ddist$limits$BMI[2]<-refvalue
    
    pred_OR<-Predict(fit,DMFT,ref.zero=TRUE,fun=exp)
    violet <- "#4DBBD5FF"
    # ????????????????????????baseplot
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd=NA)
    ylim.bot<-min(pred_OR[,"lower"])
    ylim.top<-max(pred_OR[,"upper"])
    # ??????????????????????????????????????????
    dens <- density(RCS$ DMFT)
    # ????????????
    plot(dens$x,dens$y, col=ggplot2::alpha(violet,0.5), type="l",xaxt="n",yaxt="n",xlab = "",ylab ="" )
    polygon(dens$x,dens$y,
            col = ggplot2::alpha(violet,0.5),border = ggplot2::alpha(violet,0.5)) # ??????????????????????????????
    axis(side = 4)
    par(new=TRUE)
    plot(pred_OR[,1],pred_OR[,"yhat"], 
         xlab = "DMFT",ylab = "HR",
         type = "l",ylim = c(ylim.bot,ylim.top),
         col="red",lwd=3) 
    lines(pred_OR[,1],pred_OR[,"lower"],lty=2,lwd=1.5)
    lines(pred_OR[,1],pred_OR[,"upper"],lty=2,lwd=1.5)
    lines(x=range(pred_OR[,1]),y=c(1,1),lty=3,col="grey40",lwd=3) #
    points(refvalue,1,pch=16,cex=1)
    text(refvalue + 1, 1.5, paste0("ref value = ",refvalue)) 
    legend("topright",
           paste0(
             "\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    
    dev.off()
  }
  {#** CAL_mean ####
    pdf("CAL_RCS_DM.pdf",width=6,height=5)
    limUp<-3*IQR(RCS[,"CAL_mean"],na.rm = T)+quantile(RCS[,"CAL_mean"],3/4,na.rm=T,names=F)
    limDown<-quantile(RCS[,"CAL_mean"],1/4,na.rm=T,names=F)-3*IQR(RCS[,"CAL_mean"],na.rm = T)
    RCS<- RCS[!RCS[,"CAL_mean"]>=limUp&!RCS[,"CAL_mean"]<=limDown,]
    fit <-  cph(Surv(peryear,DM_MORT_stat==1)~rcs(CAL_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 	1.58
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
    text(refvalue, 0.7, paste0("ref value = ","1.60")) 
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
    
    pdf("DM_PPD_RCS.pdf",width=6,height=5)
    
    fit <-  cph(Surv(peryear,DM_MORT_stat==1)~rcs(PPD_mean,3)+Age_status+Gender+Race_ethnicity+SES+
                  Smoking_status+Drinking_status+HEI+BMI_Grade,data=RCS,x=TRUE,y=TRUE)
    anova(fit)
    cox.zph(fit, "rank")   
    p <-round(anova(fit)[,3],3)
    refvalue <- 1.60
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
    text(refvalue, 1.2, paste0("ref value = ","1.58")) 
    legend("topright",
           paste0("\nP-non-linear = ",ifelse(round(p[2],3) < 0.001,"< 0.001",round(p[2],3))),
           bty="n",cex=1)
    dev.off()
  }
  
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 mediation(Table S7) ####
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
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
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S2<-result.all.cause
  write.table(Table_S2,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S2.csv")
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 Character_weighted(Table S8) ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Character_weighted.Rdata")
Character_weighted$SES<-Interpolation_weighted$SES
Interpolation_weighted<-Character_weighted
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
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
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S8<-result.all.cause
  write.table(Table_S8,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S8.csv")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Complete_weighted.Rdata")
SES<-Interpolation_weighted[,c("ID","SES")]
Complete_weightedSES<-merge(Complete_weighted,SES,by = "ID",all.x = T)

Interpolation_weighted<-Character_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Complete_weightedSES,strata=~sdmvstra,weights = ~ weight)
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model2
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
}

{ #* CVD model #####
  rhcSvy_DM_CVD<-subset(rhcSvy,CVD_status=="NO")
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
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_CVD)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result2.CVD)
  result.CVD
  #CVD model2
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                         Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy_DM_CVD)
  model3_CVD_result<-summary(model3_CVD)
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3.CVD)
  result.CVD
  
}

{ #* Cancer model #####
  rhcSvy_DM_Cancer<-subset(rhcSvy,Cancer_status=="NO")
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
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy_DM_Cancer)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result2.Cancer)
  result.Cancer
  #Cancer model2
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy_DM_Cancer)
  model3_Cancer_result<-summary(model3_Cancer)
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3.Cancer)
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
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES, design =rhcSvy)
  model2_DM_result<-summary(model2_DM)
  P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="DM cause")
  result.DM<-rbind(result,result2)
  #DM model2
  model3_DM<-svycoxph(Surv(peryear, DM_MORT_stat==1) ~
                        PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
                        Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy)
  model3_DM_result<-summary(model3_DM)
  
  P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="DM cause")
  result.DM<-rbind(result.DM,result3)
  result.DM
}

{ #* Combine #####
  result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
  result.all.cause$HR<-round(result.all.cause$HR,2)
  result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
  result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
  result.all.cause$P.value<-round(result.all.cause$P.value,3)
  Table_S9<-result.all.cause
  write.table(Table_S9,sep = ",",file ="G:/paper_2_PD&DM/data_DC/Table_S9.csv")
}
Interpolation_weighted<-Complete_weightedSES
#all.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
#CVD.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_stat=="NO",useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,Interpolation_weighted$CVD_stat=="NO",useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
#Cancer.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_stat=="NO",useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,Interpolation_weighted$Cancer_stat=="NO",useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
#DM.cause
PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$DM_MORT_stat,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))

total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause,DM.cause))

# >>>>> section 28 mediation(Table S7) ####
load(file="G:/paper_2_PD&DM/data_DC/PD_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/PD_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/PD_CON2.Rdata")
PD_III$ID<-paste0("NHANES_III_",PD_III$SEQN)
PD_CON1$ID<-paste0("NHANES_CON1_",PD_CON1$SEQN)
PD_CON2$ID<-paste0("NHANES_CON2_",PD_CON2$SEQN)
PD<-rbind(PD_III,PD_CON1,PD_CON2)
load(file="G:/paper_2_PD&DM/data_DC/Covariates_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Covariates_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Covariates_CON2.Rdata")
Covariates_III$ID<-paste0("NHANES_III_",Covariates_III$SEQN)
Covariates_CON1$ID<-paste0("NHANES_CON1_",Covariates_CON1$SEQN)
Covariates_CON2$ID<-paste0("NHANES_CON2_",Covariates_CON2$SEQN)
Covariates<-rbind(Covariates_III,Covariates_CON1,Covariates_CON2)
PD_Covariates<-merge(PD,Covariates,by = "ID",all.x = T)

load(file="G:/paper_2_PD&DM/data_DC/WBC_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/WBC_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/WBC_CON2.Rdata")
WBC_III$ID<-paste0("NHANES_III_",WBC_III$SEQN)
WBC_CON1$ID<-paste0("NHANES_CON1_",WBC_CON1$SEQN)
WBC_CON2$ID<-paste0("NHANES_CON2_",WBC_CON2$SEQN)
WBC<-rbind(WBC_III,WBC_CON1,WBC_CON2)
PD_WBC<-merge(PD_Covariates,WBC,by = "ID",all.x = T)
load(file="G:/paper_2_PD&DM/data_DC/CRP.Rdata")
PD_CRP<-merge(PD_WBC,CRP,by = "ID",all.x = T)
load(file="G:/paper_2_PD&DM/data_DC/Fb.Rdata")
PD_Fb<-merge(PD_CRP,Fb,by = "ID",all.x = T)
load(file="G:/paper_2_PD&DM/data_DC/MORT_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/MORT_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/MORT_CON2.Rdata")
MORT_III$ID<-paste0("NHANES_III_",MORT_III$SEQN)
MORT_CON1$ID<-paste0("NHANES_CON1_",MORT_CON1$SEQN)
MORT_CON2$ID<-paste0("NHANES_CON2_",MORT_CON2$SEQN)
MORT<-rbind(MORT_III,MORT_CON1,MORT_CON2)
PD_MORT<-merge(PD_Fb,MORT,by = "ID",all.x = T)
PD_MORT$SEQN.x<-NULL
PD_MORT$SEQN.y<-NULL
table(PD_MORT$MORT_stat)

load(file="G:/paper_2_PD&DM/data_DC/Weight_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Weight_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Weight_CON2.Rdata")
Weight_III$ID<-paste0("NHANES_III_",Weight_III$SEQN)
Weight_CON1$ID<-paste0("NHANES_CON1_",Weight_CON1$SEQN)
Weight_CON2$ID<-paste0("NHANES_CON2_",Weight_CON2$SEQN)
Weight<-rbind(Weight_III,Weight_CON1,Weight_CON2)
PD_Weight<-merge(PD_Fb,Weight,by = "ID",all.x = T)
PD_Weight$SEQN.x<-NULL
PD_Weight$SEQN.y<-NULL
PD_Weight$Age[PD_Weight$Age>=80]<-80
PD_Weight$Age_status[PD_Weight$Age<45]<-"<45"
PD_Weight$Age_status[PD_Weight$Age>=45&PD_Weight$Age<65]<-"[45,65)"
PD_Weight$Age_status[PD_Weight$Age>=65]<-">=65"
PD_Weight$Age_status<-factor(PD_Weight$Age_status,
                            levels = c("<45","[45,65)",">=65"))
PD_Weight$PD_diagnosis[PD_Weight$PD_diagnosis=="Moderate/Severe periodontitis"]<-1
PD_Weight$PD_diagnosis[PD_Weight$PD_diagnosis=="No/Mild periodontitis"]<-0
PD_Weight$PD_diagnosis<-as.factor(PD_Weight$PD_diagnosis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =PD_Weight,strata=~sdmvstra,weights = ~ weight)
b<-svyglm(WBC ~PD_diagnosis+Age_status+Gender+Race_ethnicity+
            Smoking_status+Drinking_status+HEI+BMI_Grade,design =rhcSvy)
summary(b)

#WBC
library(mediation)
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
Interpolation_weighted$MORT_stat<-as.character(Interpolation_weighted$MORT_stat)
Interpolation_weighted$MORT_stat<-as.numeric(Interpolation_weighted$MORT_stat)
Interpolation_weighted$PD_diagnosis<-as.character(Interpolation_weighted$PD_diagnosis)
Interpolation_weighted$PD_diagnosis[Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"]<-0
Interpolation_weighted$PD_diagnosis[Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"]<-1
Interpolation_weighted$PD_diagnosis<-as.factor(Interpolation_weighted$PD_diagnosis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
table(Interpolation_weighted$PD_diagnosis1)
Interpolation_weighted<-subset(Interpolation_weighted,peryear>0)
rhcSvy1<-subset(rhcSvy,peryear>0)
b<-svyglm( WBC ~PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
             Smoking_status+Drinking_status+HEI+BMI_Grade,  design =rhcSvy1)
c<-svysurvreg(Surv(peryear, MORT_stat) ~ PD_diagnosis+WBC+Age_status+Gender+Race_ethnicity+
                Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy1)
summary(c)
class(c)<-"survreg" 
class(b)<-"glm" 
contcont <- mediate(b,c, treat="PD_diagnosis", mediator="WBC",sims=50, outcome="MORT_stat")
summary(contcont)

#CRP
load(file="G:/paper_2_PD&DM/data_DC/CRP.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/Interpolation_weighted.Rdata")
Interpolation_weighted$MORT_stat<-as.numeric(Interpolation_weighted$MORT_stat)
Interpolation_weighted$PD_diagnosis<-as.character(Interpolation_weighted$PD_diagnosis)
Interpolation_weighted$PD_diagnosis1[Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"]<-0
Interpolation_weighted$PD_diagnosis1[Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"]<-1
Interpolation<-merge(Interpolation_weighted,CRP,by = 'ID',all = F)
Interpolation$logCRP<-log(Interpolation$CRP)
Interpolation$PD_diagnosis1<-as.factor(Interpolation$PD_diagnosis1)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation,strata=~sdmvstra,weights = ~ weight)
rhcSvy1<-subset(rhcSvy,peryear>0)
# c<-svysurvreg(Surv(peryear, MORT_stat) ~ PD_diagnosis1+logCRP+Age_status+Gender+Race_ethnicity+SES+
#                 Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy1)
b<-svyglm( logCRP ~PD_diagnosis1+Age_status+Gender+Race_ethnicity+SES+
             Smoking_status+Drinking_status+HEI+BMI_Grade,  design =rhcSvy1)
class(c)<-"survreg" 
class(b)<-"glm" 
contcont <- mediate(b,c, treat="PD_diagnosis1", mediator="logCRP",sims=50, outcome="MORT_stat")
summary(contcont)


load(file="G:/paper_2_PD&DM/data_DC/Fb.Rdata")
Interpolation_weighted$MORT_stat<-as.numeric(Interpolation_weighted$MORT_stat)
Interpolation_weighted$PD_diagnosis<-as.character(Interpolation_weighted$PD_diagnosis)
Interpolation_weighted$PD_diagnosis1[Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"]<-0
Interpolation_weighted$PD_diagnosis1[Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"]<-1
Interpolation<-merge(Interpolation_weighted,Fb,by = 'ID',all = F)
Interpolation$logFb<-log(Interpolation$Fb)
Interpolation$PD_diagnosis<-as.factor(Interpolation$PD_diagnosis)
#Interpolation$logCRP<-log(Interpolation$CRP)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation,strata=~sdmvstra,weights = ~ weight)
rhcSvy1<-subset(rhcSvy,peryear>0)
# c<-svysurvreg(Surv(peryear, MORT_stat) ~ PD_diagnosis1+Fb+Age_status+Gender+Race_ethnicity+SES+
#                 Smoking_status+Drinking_status+HEI+BMI_Grade, design =rhcSvy1)
b<-svyglm( Fb ~PD_diagnosis+Age_status+Gender+Race_ethnicity+SES+
             Smoking_status+Drinking_status+HEI+BMI_Grade,  design =rhcSvy1)
summary(b)

class(c)<-"survreg" 
class(b)<-"glm" 
contcont <- mediate(b,c, treat="PD_diagnosis1", mediator="Fb",sims=50, outcome="MORT_stat")
summary(contcont)
plot(contcont)
}

b<-svyglm(  PD_diagnosis1~WBC+Age_status+Gender+Race_ethnicity+SES+
             Smoking_status+Drinking_status+HEI+BMI_Grade,  design =rhcSvy1)
summary(b)

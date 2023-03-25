# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0.1 Packages ####
  library(foreign)
  library(dplyr)
  library(nhanesR)
  library(tidyverse)
  library(tidyr)
  library(PSCBS)
}
{#* section 0.2 Functions ####
  {#** section 0.2.1 multi_merge ####
    multimerge<-function(dat=list(),...){
      if(length(dat)<2)return(as.data.frame(dat))
      mergedat<-dat[[1]]
      dat[[1]]<-NULL
      for(i in dat){
        mergedat<-merge(mergedat,i,...)
      }
      return(mergedat)
    }
  }
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 8. Covariates of NHANES III <<<<< ####
{#** section 8.0 Lab,Exam,Adult data ####
exam=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/exam.dat",sep=",",fill=T)
exam$SEQN<-as.numeric(substring(exam$V1,1,5))
adult=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
adult$SEQN<-as.numeric(substring(adult$V1,1,5))
lab=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
lab$SEQN<-as.numeric(substring(lab$V1,1,5))
}
{#** section 8.1 Age,Gender,Race/ethnicity,Education_levels,PIR  ####
    adult_result<-as.data.frame(adult$SEQN)
    colnames(adult_result)<-"SEQN"
    adult_result$Age<-as.numeric(substring(adult$V1,18,19))
    adult_result$Age[adult_result$Age>=85]<-85
    #Year
    adult_result$Year<-as.numeric(substring(adult$V1,42,42))
    adult_result$Year[adult_result$Year==1]<-"1988-1991"
    adult_result$Year[adult_result$Year==2]<-"1991-1994"
    #Gender
    adult_result$Gender<-as.numeric(substring(adult$V1,15,15))
    adult_result$Gender[adult_result$Gender==1]<-"Male"
    adult_result$Gender[adult_result$Gender==2]<-"Female"
    #Race_ethnicity
    adult_result$Race_ethnicity_raw1<-as.numeric(substring(adult$V1,12,12))
    adult_result$Race_ethnicity_raw2<-as.numeric(substring(adult$V1,14,14))
    adult_result$Race_ethnicity<-"Other_Race"
    adult_result$Race_ethnicity[adult_result$Race_ethnicity_raw1==1]<-"Non-Hispanic white"
    adult_result$Race_ethnicity[adult_result$Race_ethnicity_raw1==2]<-"Non-Hispanic black"
    adult_result$Race_ethnicity[adult_result$Race_ethnicity_raw2==1]<-"Hispanic"
    adult_result$Race_ethnicity[adult_result$Race_ethnicity_raw2==2]<-"Hispanic"
    table(adult_result$Race_ethnicity)
    #Education_levels
    adult_result$Education_levels_raw<-as.numeric(substring(adult$V1,1256,1257))
    adult_result$Education_levels_raw[adult_result$Education_levels_raw==88]<-NA
    adult_result$Education_levels_raw[adult_result$Education_levels_raw==99]<-NA
    
    summary(adult_result$Education_levels_raw)
    adult_result$Education_levels[adult_result$Education_levels_raw<12]<-"Less_than_high_school"
    adult_result$Education_levels[adult_result$Education_levels_raw==12]<-"Less_than_high_school"
    adult_result$Education_levels[adult_result$Education_levels_raw>12]<-"College_or_above"
    adult_result$Education_levels[is.na(adult_result$Education_levels_raw)] <- NA
    table(adult_result$Education_levels)
    adult_result$PIR_raw<-as.numeric(substring(adult$V1,36,41))
    adult_result$PIR_raw[adult_result$PIR_raw==888888]<-NA
    adult_result$PIR[adult_result$PIR_raw<1.30]<-"<1.30"
    adult_result$PIR[adult_result$PIR_raw>=1.30&adult_result$PIR_raw<3]<-"1.30–2.99"
    adult_result$PIR[adult_result$PIR_raw>=3&adult_result$PIR_raw<5]<-"3.00–4.99"
    adult_result$PIR[adult_result$PIR_raw>=5]<-">=5.00"
    Covariates_III_1<-adult_result[,c("SEQN","Year","Age","Gender","Race_ethnicity","Education_levels","PIR_raw","PIR")]
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.2 health insurance  ####
    adult$SEQN<-as.numeric(substring(adult$V1,1,5))
    adult_Insurance<-as.data.frame(adult$SEQN)
    colnames(adult_Insurance)<-"SEQN"
    adult_Insurance$Insurance1<-as.numeric(substring(adult$V1,1261,1261))
    adult_Insurance$Insurance2<-as.numeric(substring(adult$V1,1262,1262))
    adult_Insurance$Insurance3<-as.numeric(substring(adult$V1,1263,1263))
    adult_Insurance$Insurance4<-as.numeric(substring(adult$V1,1264,1264))
    adult_Insurance$Insurance5<-as.numeric(substring(adult$V1,1265,1265))
    adult_Insurance$Insurance6<-as.numeric(substring(adult$V1,1266,1266))
    adult_Insurance$Insurance7<-as.numeric(substring(adult$V1,1268,1268))
    adult_Insurance$Insurance8<-as.numeric(substring(adult$V1,1270,1270))
    adult_Insurance$Insurance9<-as.numeric(substring(adult$V1,1271,1271))
    adult_Insurance$Insurance[adult_Insurance$Insurance1==2|adult_Insurance$Insurance2==2|
                                adult_Insurance$Insurance4==2|adult_Insurance$Insurance5==2|
                                adult_Insurance$Insurance6==2|adult_Insurance$Insurance7==2|
                                adult_Insurance$Insurance8==2|adult_Insurance$Insurance9==2]<-"No_insurance"
    adult_Insurance$Insurance[adult_Insurance$Insurance1==1|adult_Insurance$Insurance2==1|
                                adult_Insurance$Insurance4==1|adult_Insurance$Insurance5==1|
                                adult_Insurance$Insurance6==1|adult_Insurance$Insurance7==1|
                                adult_Insurance$Insurance3==1|adult_Insurance$Insurance3==2|
                                adult_Insurance$Insurance3==3]<-"Public_insurance"	
    adult_Insurance$Insurance[adult_Insurance$Insurance8==1|adult_Insurance$Insurance9==1]<-"Private_insurance"
    Insurance_III<-adult_Insurance[,c("SEQN","Insurance")]
    colnames(Insurance_III)[2]<-"Health_insurance"
    table(adult_Insurance$Insurance)
    Covariates_III_2<-merge(Covariates_III_1,Insurance_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
    record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.3 Smoking status  ####
    adult_Smoke<-as.data.frame(adult$SEQN)
    colnames(adult_Smoke)<-"SEQN"
    adult_Smoke$Smoke1<-as.numeric(substring(adult$V1,2281,2281))
    adult_Smoke$Smoke2<-as.numeric(substring(adult$V1,2285,2285))
    adult_Smoke$Smoking_status[adult_Smoke$Smoke1==2|adult_Smoke$Smoke2==2]<-
      "Never_smoker"
    adult_Smoke$Smoking_status[adult_Smoke$Smoke1==1&adult_Smoke$Smoke2==1]<-
      "Current_smoker"
    adult_Smoke$Smoking_status[adult_Smoke$Smoke1==1&adult_Smoke$Smoke2==2]<-
      "Former_smoker"
    SMQ_III<-adult_Smoke[,c("SEQN","Smoking_status")]
    Covariates_III_3<-merge(Covariates_III_2,SMQ_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                          record=='Covariates_III_3'|
                          record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.4 Drinking status  ####
    Drink_exam<-as.data.frame(exam$SEQN)
    colnames(Drink_exam)<-"SEQN"
    #In your entire life, have you had at least 12 drinks
    Drink_exam$Drink1<-as.numeric(substring(exam$V1,5109,5109))
    #In the past 12 months did you have at least 12 drinks
    Drink_exam$Drink2<-as.numeric(substring(exam$V1,5110,5110))
    #On the average, on the days that you drank alcohol, how many drinks
    Drink_exam$Drink3<-as.numeric(substring(exam$V1,5114,5116))
    #In the past 12 months, how many days
    Drink_exam$Drink4<-as.numeric(substring(exam$V1,5111,5113))
    Drink_III<-merge(Drink_exam,Covariates_III_3[,c("SEQN","Gender")],by = "SEQN",all.x = T)
    Drink_III$Drink1[Drink_III$Drink1==8]<-NA
    Drink_III$Drink1[Drink_III$Drink1==9]<-NA
    Drink_III$Drink2[Drink_III$Drink2==8]<-NA
    Drink_III$Drink2[Drink_III$Drink2==9]<-NA
    Drink_III$Drink3[Drink_III$Drink3==888]<-NA
    Drink_III$Drink3[Drink_III$Drink3==999]<-NA
    Drink_III$Drink4[Drink_III$Drink4==888]<-NA
    Drink_III$Drink4[Drink_III$Drink4==999]<-NA
    Drink_III$Drinking_status[Drink_III$Drink1==2|Drink_III$Drink2==2]<-"Nondrinker"
    Drink_III$Drinking_status[(Drink_III$Drink2==1|Drink_III$Drink1==1)]<-"Light/moderate_drinker"
    Drink_III$Drinking_status[((Drink_III$Drink3<=1&Drink_III$Gender=="Female")|
                                 (Drink_III$Drink3<=2&Drink_III$Gender=="Male"))]<-
      "Light/moderate_drinker"
    Drink_III$Drinking_status[((Drink_III$Drink3>1&Drink_III$Gender=="Female")|
                                 (Drink_III$Drink3>2&Drink_III$Gender=="Male"))&Drink_III$Drink2==1]<-
      "Heavier_drinker"
    table(Drink_III$Drinking_status)
    Drink_III<-Drink_III[,c("SEQN","Drinking_status")]
    Covariates_III_4<-merge(Covariates_III_3,Drink_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                          record=='Covariates_III_3'|record=='Covariates_III_4'|
                          record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.5 Physical_activity  ####
    adult_physical<-as.data.frame(adult$SEQN)
    colnames(adult_physical)<-"SEQN"
    #In the past month, walk a mile (MET=3.5)
    adult_physical$HAT1S<-as.numeric(substring(adult$V1,2389,2392))
    adult_physical$HAT1S[adult_physical$HAT1S==8888]<-NA
    adult_physical$HAT1S[adult_physical$HAT1S==9999]<-NA
    adult_physical$HAT1S[adult_physical$HAT1S==9998]<-NA
    #jog or run(8)
    adult_physical$HAT2S<-as.numeric(substring(adult$V1,2398,2401))
    adult_physical$HAT2S[adult_physical$HAT2S==8888]<-NA
    adult_physical$HAT2S[adult_physical$HAT2S==9999]<-NA
    adult_physical$HAT2S[adult_physical$HAT2S==9998]<-NA
    #ride a bicycle(5)
    adult_physical$HAT3S<-as.numeric(substring(adult$V1,2404,2407))
    adult_physical$HAT3S[adult_physical$HAT3S==8888]<-NA
    adult_physical$HAT3S[adult_physical$HAT3S==9999]<-NA
    #swim(6)
    adult_physical$HAT4S<-as.numeric(substring(adult$V1,2410,2413))
    adult_physical$HAT4S[adult_physical$HAT4S==8888]<-NA
    adult_physical$HAT4S[adult_physical$HAT4S==9999]<-NA
    #do aerobics(6)
    adult_physical$HAT5S<-as.numeric(substring(adult$V1,2416,2419))
    adult_physical$HAT5S[adult_physical$HAT5S==8888]<-NA
    adult_physical$HAT5S[adult_physical$HAT5S==9999]<-NA
    #other dancing(4.5)
    adult_physical$HAT6S<-as.numeric(substring(adult$V1,2424,2427))
    adult_physical$HAT6S[adult_physical$HAT6S==8888]<-NA
    adult_physical$HAT6S[adult_physical$HAT6S==9999]<-NA
    #exercises(4.5)
    adult_physical$HAT7S<-as.numeric(substring(adult$V1,2432,2435))
    adult_physical$HAT7S[adult_physical$HAT7S==8888]<-NA
    adult_physical$HAT7S[adult_physical$HAT7S==9999]<-NA
    #yard work(5)
    adult_physical$HAT8S<-as.numeric(substring(adult$V1,2438,2441))
    adult_physical$HAT8S[adult_physical$HAT8S==8888]<-NA
    adult_physical$HAT8S[adult_physical$HAT8S==9999]<-NA
    adult_physical$HAT8S[adult_physical$HAT8S==9998]<-NA
    #lift weights(3)
    adult_physical$HAT9S<-as.numeric(substring(adult$V1,2444,2447))
    adult_physical$HAT9S[adult_physical$HAT9S==8888]<-NA
    adult_physical$HAT9S[adult_physical$HAT9S==9999]<-NA
    #other exercises
    adult_physical$HATOM<-as.numeric(substring(adult$V1,2454,2457))
    adult_physical$HATOS<-as.numeric(substring(adult$V1,2458,2461))
    adult_physical$HATOS[adult_physical$HATOS==8888]<-NA
    adult_physical$HATOM[adult_physical$HATOM==8888]<-NA
    #other exercises2
    adult_physical$HATOM2<-as.numeric(substring(adult$V1,2467,2469))
    adult_physical$HATOS2<-as.numeric(substring(adult$V1,2470,2473))
    adult_physical$HATOS2[adult_physical$HATOS2==8888]<-NA
    #other exercises3
    adult_physical$HATOM3<-as.numeric(substring(adult$V1,2479,2481))
    adult_physical$HATOS3<-as.numeric(substring(adult$V1,2482,2485))
    adult_physical$HATOS3[adult_physical$HATOS3==8888]<-NA
    #other exercises4
    adult_physical$HATOM4<-as.numeric(substring(adult$V1,2491,2493))
    adult_physical$HATOS4<-as.numeric(substring(adult$V1,2494,2497))
    adult_physical$HATOS4[adult_physical$HATOS4==8888]<-NA
    #
    adult_physical$HAT2<-as.numeric(substring(adult$V1,2396,2396))
    adult_physical$HAT2[adult_physical$HAT2==8]<-NA
    adult_physical$HAT3<-as.numeric(substring(adult$V1,2402,2402))
    adult_physical$HAT3[adult_physical$HAT3==8]<-NA
    adult_physical$HAT4<-as.numeric(substring(adult$V1,2408,2408))
    adult_physical$HAT4[adult_physical$HAT4==8]<-NA
    adult_physical$HAT5<-as.numeric(substring(adult$V1,2414,2414))
    adult_physical$HAT5[adult_physical$HAT5==8]<-NA
    adult_physical$HAT6<-as.numeric(substring(adult$V1,2420,2420))
    adult_physical$HAT6[adult_physical$HAT6==8]<-NA
    adult_physical$HAT7<-as.numeric(substring(adult$V1,2428,2428))
    adult_physical$HAT7[adult_physical$HAT7==8]<-NA
    adult_physical$HAT8<-as.numeric(substring(adult$V1,2436,2436))
    adult_physical$HAT8[adult_physical$HAT8==8]<-NA
    adult_physical$HAT9<-as.numeric(substring(adult$V1,2442,2442))
    adult_physical$HAT9[adult_physical$HAT9==8]<-NA
    adult_physical$HATO<-as.numeric(substring(adult$V1,2448,2448))
    adult_physical$HATO[adult_physical$HATO==8]<-NA
    
    
    
    adult_physical$na[is.na(adult_physical$HAT1)&is.na(adult_physical$HAT2)&is.na(adult_physical$HAT3)&
                        is.na(adult_physical$HAT4)&is.na(adult_physical$HAT5)&is.na(adult_physical$HAT6)&
                        is.na(adult_physical$HAT7)&is.na(adult_physical$HAT8)&is.na(adult_physical$HAT9)&
                        is.na(adult_physical$HATO)]<-"na"
    
    adult_physical$Physical_status[(adult_physical$HAT1==0|is.na(adult_physical$HAT1))&
                                     (adult_physical$HAT2==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT3==2|is.na(adult_physical$HAT3))&
                                     (adult_physical$HAT4==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT5==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT6==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT7==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT8==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HAT9==2|is.na(adult_physical$HAT2))&
                                     (adult_physical$HATO==2|is.na(adult_physical$HAT2))]<-"Inactive"
    
    adult_physical$Physical_status[(adult_physical$HAT1>=1|is.na(adult_physical$HAT1))|
                                     (adult_physical$HAT2==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT3==1|is.na(adult_physical$HAT3))|
                                     (adult_physical$HAT4==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT5==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT6==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT7==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT8==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HAT9==1|is.na(adult_physical$HAT2))|
                                     (adult_physical$HATO==1|is.na(adult_physical$HAT2))]<-"Insufficient"                                 
    
    adult_physical <- adult_physical %>% mutate(METU5O1 = ifelse(HATOM>6,HATOS,0))
    adult_physical <- adult_physical %>% mutate(METL5O1 = ifelse(HATOM<=6&HATOM>=3,HATOS,0))
    adult_physical <- adult_physical %>% mutate(METU5O2 = ifelse(HATOM2>6,HATOS2,0))
    adult_physical <- adult_physical %>% mutate(METL5O2 = ifelse(HATOM<=6&HATOM>=3,HATOS2,0))
    adult_physical <- adult_physical %>% mutate(METU5O3 = ifelse(HATOM3>6,HATOS3,0))
    adult_physical <- adult_physical %>% mutate(METL5O3 = ifelse(HATOM<=6&HATOM>=3,HATOS3,0))
    adult_physical <- adult_physical %>% mutate(METU5O4 = ifelse(HATOM4>6,HATOS4,0))
    adult_physical <- adult_physical %>% mutate(METL5O4 = ifelse(HATOM<=6&HATOM>=3,HATOS4,0))
    adult_physical[is.na(adult_physical)]<-0
    adult_physical$METU5=(adult_physical$HAT2S+
                            adult_physical$METU5O1+adult_physical$METU5O2+adult_physical$METU5O3+adult_physical$METU5O4)
    adult_physical$METL5=(adult_physical$HAT1S+adult_physical$HAT3S+adult_physical$HAT4S+
                            adult_physical$HAT5S+adult_physical$HAT6S+adult_physical$HAT7S+
                            adult_physical$HAT8S+adult_physical$HAT9S+
                            adult_physical$METL5O1+adult_physical$METL5O2+adult_physical$METL5O3+adult_physical$METL5O4)
    
    adult_physical$Physical_status[(adult_physical$METU5/30*7)>=3|(adult_physical$METL5/30*7)>=5]<-"Recommended"
    adult_physical$Physical_status[adult_physical$na=="na"]<-NA
    adult_physical$Physical_status[adult_physical$Physical_status==0]<-NA
    table(adult_physical$Physical_status,useNA = "ifany")
    Physical_III<-adult_physical[,c("SEQN","Physical_status")]
    Covariates_III_5<-merge(Covariates_III_4,Physical_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.6 HEI  ####
    HEI_weight<-as.data.frame(exam$SEQN)
    colnames(HEI_weight)<-c("SEQN")
    HEI_weight$MEC_weight<-as.numeric(substring(exam$V1,59,67))
    HEI=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/hei.dat",sep=",",fill=T)
    HEI$SEQN<-as.numeric(substring(HEI$V1,1,5))
    HEI_data<-as.data.frame(HEI$SEQN)
    colnames(HEI_data)<-c("SEQN")
    HEI_data$HEI_Score<-as.numeric(substring(HEI$V1,92,96))
    HEI_III<-merge(HEI_weight,HEI_data,by="SEQN",all.y  = T)
    load("/media/lee/paper_record/paper_2_PD&DM/data_DC/III_baseline.Rdata")
    HEI_III<-merge(HEI_III,III_baseline[,c("SEQN","ID")],by="SEQN",all.y = T)
    Quantile<-weightedQuantile(HEI_III$HEI_Score,weights =HEI_III$MEC_weight, probs=c(0.2,0.4,0.6,0.8),  na.rm=TRUE)
    HEI_III$HEI[HEI_III$HEI_Score<=Quantile[1]]<-"Quintile 1"
    HEI_III$HEI[HEI_III$HEI_Score<=Quantile[2]&HEI_III$HEI_Score>Quantile[1]]<-"Quintile 2"
    HEI_III$HEI[HEI_III$HEI_Score<=Quantile[3]&HEI_III$HEI_Score>Quantile[2]]<-"Quintile 3"
    HEI_III$HEI[HEI_III$HEI_Score<=Quantile[4]&HEI_III$HEI_Score>Quantile[3]]<-"Quintile 4"
    HEI_III$HEI[HEI_III$HEI_Score>Quantile[4]]<-"Quintile 5"
    table(HEI_III$HEI)
    HEI_III<-HEI_III[,c("SEQN","HEI_Score","HEI")]
    Covariates_III_6<-merge(Covariates_III_5,HEI_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|record=='Covariates_III_6'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.7 BMI ####
    BMI_III<-as.data.frame(exam$SEQN)
    colnames(BMI_III)<-c("SEQN")
    BMI_III$BMI<-as.numeric(substring(exam$V1,1524,1527))
    BMI_III$BMI_Grade[BMI_III$BMI<18.5]<-"<18.5"
    BMI_III$BMI_Grade[BMI_III$BMI>=18.5&BMI_III$BMI<25]<-"[18.5,25.0)"
    BMI_III$BMI_Grade[BMI_III$BMI>=25&BMI_III$BMI<30]<-"[25.0-30)"
    BMI_III$BMI_Grade[BMI_III$BMI>=30]<-">=30"
    table(BMI_III$BMI_Grade)
    Covariates_III_7<-merge(Covariates_III_6,BMI_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|record=='Covariates_III_6'|
                            record=='Covariates_III_7'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.8 Hypertension ####
    HTN_DATA<-as.data.frame(exam$SEQN)
    colnames(HTN_DATA)<-c("SEQN")
    HTN_DATA$systolic<-as.numeric(substring(exam$V1,1423,1425))
    HTN_DATA$systolic[HTN_DATA$systolic==888]<-NA
    HTN_DATA$diastolic<-as.numeric(substring(exam$V1,1428,1430))
    HTN_DATA$diastolic[HTN_DATA$diastolic==888]<-NA
    HTN_DATA$HTN_exam_status[ HTN_DATA$systolic>=140|HTN_DATA$diastolic>=90]<-"YES"
    HTN_DATA$HTN_exam_status[ HTN_DATA$systolic<140&HTN_DATA$diastolic<90]<-"NO"
    adult=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
    adult$SEQN<-as.numeric(substring(adult$V1,1,5))
    adult_HTN<-as.data.frame(adult$SEQN)
    colnames(adult_HTN)<-"SEQN"
    #Have you ever been told by a doctor or other health professional  that you had hypertension
    adult_HTN$told1<-as.numeric(substring(adult$V1,1598,1598))
    adult_HTN$told1[adult_HTN$told1==8]<-NA
    adult_HTN$told1[adult_HTN$told1==9]<-NA
    #Were you told on 2 or more different visits that you had hypertension
    adult_HTN$told2<-as.numeric(substring(adult$V1,1599,1599))
    adult_HTN$told2[adult_HTN$told2==8]<-NA
    adult_HTN$told2[adult_HTN$told2==9]<-NA
    #been told by a doctor or other health professional to take prescribed medicine?
    adult_HTN$told3<-as.numeric(substring(adult$V1,1600,1600))
    adult_HTN$told3[adult_HTN$told3==8]<-NA
    adult_HTN$told3[adult_HTN$told3==9]<-NA
    # control your weight or lose weight?
    adult_HTN$told4<-as.numeric(substring(adult$V1,1601,1601))
    adult_HTN$told4[adult_HTN$told4==8]<-NA
    adult_HTN$told4[adult_HTN$told4==9]<-NA
    # cut down on  salt or sodium in your diet?
    adult_HTN$told5<-as.numeric(substring(adult$V1,1602,1602))
    adult_HTN$told5[adult_HTN$told5==8]<-NA
    adult_HTN$told5[adult_HTN$told5==9]<-NA
    # do anything else?
    adult_HTN$told6<-as.numeric(substring(adult$V1,1603,1603))
    adult_HTN$told6[adult_HTN$told6==8]<-NA
    adult_HTN$told6[adult_HTN$told6==9]<-NA
    #  exercise more?
    adult_HTN$told7<-as.numeric(substring(adult$V1,1604,1604))
    adult_HTN$told7[adult_HTN$told7==8]<-NA
    adult_HTN$told7[adult_HTN$told7==9]<-NA
    #  restrict alcohol?
    adult_HTN$told8<-as.numeric(substring(adult$V1,1605,1605))
    adult_HTN$told8[adult_HTN$told8==8]<-NA
    adult_HTN$told8[adult_HTN$told8==9]<-NA
    #  stop smoking?
    adult_HTN$told9<-as.numeric(substring(adult$V1,1606,1606))
    adult_HTN$told9[adult_HTN$told9==8]<-NA
    adult_HTN$told9[adult_HTN$told9==9]<-NA
    #  reduce tension?
    adult_HTN$told10<-as.numeric(substring(adult$V1,1607,1607))
    adult_HTN$told10[adult_HTN$told10==8]<-NA
    adult_HTN$told10[adult_HTN$told10==9]<-NA
    #  l to change your diet?
    adult_HTN$told11<-as.numeric(substring(adult$V1,1608,1608))
    adult_HTN$told11[adult_HTN$told11==8]<-NA
    adult_HTN$told11[adult_HTN$told11==9]<-NA
    #   make other changes?
    adult_HTN$told12<-as.numeric(substring(adult$V1,1609,1609))
    adult_HTN$told12[adult_HTN$told12==8]<-NA
    adult_HTN$told12[adult_HTN$told12==9]<-NA
    adult_HTN$HTN_self_status[adult_HTN$told1==2]<-"NO"
    adult_HTN$HTN_self_status[adult_HTN$told1==1|adult_HTN$told2==1|adult_HTN$told3==1|
                                adult_HTN$told4==1|adult_HTN$told5==1|adult_HTN$told6==1|
                                adult_HTN$told7==1|adult_HTN$told8==1|adult_HTN$told9==1|
                                adult_HTN$told10==1|adult_HTN$told11==1|adult_HTN$told12==1]<-"YES"
    HTN_III<-merge(HTN_DATA,adult_HTN,by = "SEQN",all = T)
    HTN_III$HTN_status[HTN_III$HTN_exam_status=="NO"|HTN_III$HTN_self_status=="NO"]<-"NO"
    HTN_III$HTN_status[HTN_III$HTN_exam_status=="YES"|HTN_III$HTN_self_status=="YES"]<-"YES"
    HTN_III<-HTN_III[,c("SEQN","HTN_status")]
    Covariates_III_8<-merge(Covariates_III_7,HTN_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|record=='Covariates_III_6'|
                            record=='Covariates_III_7'|record=='Covariates_III_8'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.9 hyperlipoidemia  ####
    HPL_exam_III<-as.data.frame(lab$SEQN)
    colnames(HPL_exam_III)<-c("SEQN")
    HPL_exam_III$TCP<-as.numeric(substring(lab$V1,1598,1600))
    HPL_exam_III$TCP[HPL_exam_III$TCP==888]=NA
    HPL_exam_III$HPL_exam_status[HPL_exam_III$TCP>=200]<-"YES"
    HPL_exam_III$HPL_exam_status[HPL_exam_III$TCP<200]<-"NO"
    #Have you ever been told by a doctor or other health professional that you had hypertension
    adult_HPL<-as.data.frame(adult$SEQN)
    colnames(adult_HPL)<-"SEQN"
    adult_HPL$told1<-as.numeric(substring(adult$V1,1620,1620))
    adult_HPL$told1[adult_HPL$told1==8]<-NA
    adult_HPL$told1[adult_HPL$told1==9]<-NA
    #eat fewer high fat or high cholesterol foods?
    adult_HPL$told2<-as.numeric(substring(adult$V1,1621,1621))
    adult_HPL$told2[adult_HPL$told2==8]<-NA
    adult_HPL$told2[adult_HPL$told2==9]<-NA
    # lose weight?
    adult_HPL$told3<-as.numeric(substring(adult$V1,1622,1622))
    adult_HPL$told3[adult_HPL$told3==8]<-NA
    adult_HPL$told3[adult_HPL$told3==9]<-NA
    # exercise more?
    adult_HPL$told4<-as.numeric(substring(adult$V1,1623,1623))
    adult_HPL$told4[adult_HPL$told4==8]<-NA
    adult_HPL$told4[adult_HPL$told4==9]<-NA
    # medicine?
    adult_HPL$told5<-as.numeric(substring(adult$V1,1624,1624))
    adult_HPL$told5[adult_HPL$told5==8]<-NA
    adult_HPL$told5[adult_HPL$told5==9]<-NA
    adult_HPL$HPL_self_status[adult_HPL$told1==2]<-"NO"
    adult_HPL$HPL_self_status[adult_HPL$told1==1|adult_HPL$told2==1|adult_HPL$told3==1|
                                adult_HPL$told4==1|adult_HPL$told5==1]<-"YES"
    HPL_III<-merge(HPL_exam_III,adult_HPL,by = "SEQN",all = T)
    HPL_III$HPL_status[HPL_III$HPL_exam_status=="NO"|HPL_III$HPL_self_status=="NO"]<-"NO"
    HPL_III$HPL_status[HPL_III$HPL_exam_status=="YES"|HPL_III$HPL_self_status=="YES"]<-"YES"
    HPL_III<-HPL_III[,c("SEQN","HPL_status")]
    Covariates_III_9<-merge(Covariates_III_8,HPL_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|record=='Covariates_III_6'|
                            record=='Covariates_III_7'|record=='Covariates_III_8'|
                            record=='Covariates_III_9'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 3.10 CVD ####
    Ex_adult<-as.data.frame(adult$SEQN)
    colnames(Ex_adult)<-c("SEQN")
    #Heart disease
    Ex_adult$Heart_disease<-as.numeric(substring(adult$V1,1972,1972))
    Ex_adult$Heart_disease[Ex_adult$Heart_disease==1]<-"YES"
    Ex_adult$Heart_disease[Ex_adult$Heart_disease==2]<-"NO"
    Ex_adult$Heart_disease[Ex_adult$Heart_disease==8]<-NA
    Ex_adult$Heart_disease[Ex_adult$Heart_disease==9]<-NA
    #congestive heart failure
    Ex_adult$heart_failure<-as.numeric(substring(adult$V1,1467,1467))
    Ex_adult$heart_failure[Ex_adult$heart_failure==1]<-"YES"
    Ex_adult$heart_failure[Ex_adult$heart_failure==2]<-"NO"
    Ex_adult$heart_failure[Ex_adult$heart_failure==8]<-NA
    Ex_adult$heart_failure[Ex_adult$heart_failure==9]<-NA
    #heart attack
    Ex_adult$heart_attack<-as.numeric(substring(adult$V1,1648,1648))
    Ex_adult$heart_attack[Ex_adult$heart_attack==1]<-"YES"
    Ex_adult$heart_attack[Ex_adult$heart_attack==2]<-"NO"
    Ex_adult$heart_attack[Ex_adult$heart_attack==8]<-NA
    Ex_adult$heart_attack[Ex_adult$heart_attack==9]<-NA
    # stroke
    Ex_adult$stroke<-as.numeric(substring(adult$V1,1468,1468))
    Ex_adult$stroke[Ex_adult$stroke==1]<-"YES"
    Ex_adult$stroke[Ex_adult$stroke==2]<-"NO"
    Ex_adult$stroke[Ex_adult$stroke==8]<-NA
    Ex_adult$stroke[Ex_adult$stroke==9]<-NA
    Ex_adult$CVD[Ex_adult$Heart_disease=="NO"|Ex_adult$heart_failure=="NO"|
                   Ex_adult$heart_attack=="NO"|Ex_adult$stroke=="NO"]<-"NO"
    Ex_adult$CVD[Ex_adult$Heart_disease=="YES"|Ex_adult$heart_failure=="YES"|
                   Ex_adult$heart_attack=="YES"|Ex_adult$stroke=="YES"]<-"YES"
    colnames(Ex_adult)[1]<-"SEQN"
    CVD_III<-Ex_adult[,c("SEQN","CVD")]
    Covariates_III_10<-merge(Covariates_III_9,CVD_III,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                            record=='Covariates_III_3'|record=='Covariates_III_4'|
                            record=='Covariates_III_5'|record=='Covariates_III_6'|
                            record=='Covariates_III_7'|record=='Covariates_III_8'|
                            record=='Covariates_III_9'|record=='Covariates_III_10'|
                            record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.11 CKD ####
    {#*** section 8.11.1 eGFR ####
      adult=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
      adult$SEQN<-as.numeric(substring(adult$V1,1,5))
      lab=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
      lab$SEQN<-as.numeric(substring(lab$V1,1,5))
      adult_CKD<-as.data.frame(adult$SEQN)
      colnames(adult_CKD)<-"SEQN"
      adult_CKD$age<-as.numeric(substring(adult$V1,18,19))
      adult_CKD$age[adult_CKD$age>=85]<-85
      #Gender
      adult_CKD$sex<-as.numeric(substring(adult$V1,15,15))
      adult_CKD$sex[adult_CKD$sex==1]<-"male"
      adult_CKD$sex[adult_CKD$sex==2]<-"female"
      #Race_ethnicity
      adult_CKD$eth1<-as.numeric(substring(adult$V1,12,12))
      adult_CKD$eth2<-as.numeric(substring(adult$V1,13,13))
      adult_CKD$black<-"no"
      adult_CKD$black[adult_CKD$eth1==2|adult_CKD$eth2==2]<-"black"
      lab_CKD<-as.data.frame(lab$SEQN)
      colnames(lab_CKD)<-c("SEQN")
      lab_CKD$scr1<-as.numeric(substring(lab$V1,1784,1787))
      lab_CKD$scr1[lab_CKD$scr1==8888]<-NA
      lab_CKD$scr=lab_CKD$scr1*0.960-0.184
      data1<-merge(adult_CKD,lab_CKD,by="SEQN",all = T)
      data0<-na.omit(data1)
      
      a <- rep(NA, length(data0$black))
      a[data0$black == "black" & data0$sex == "female"] <- 166
      a[data0$black == "black" & data0$sex == "male"] <- 163
      a[data0$black != "black" & data0$sex == "female"] <- 144
      a[data0$black != "black" & data0$sex == "male"] <- 141
      b <- ifelse(data0$sex == "female", 0.7, 0.9)
      c <- rep(NA, length(data0$black))
      c[data0$sex == "female" & data0$scr <= 0.7] <- -0.329
      c[data0$sex == "female" & data0$scr > 0.7] <- -1.209
      c[data0$sex == "male" & data0$scr <= 0.9] <- -0.411
      c[data0$sex == "male" & data0$scr > 0.9] <- -1.209
      data0$eGFR <- a * ((data0$scr/b)^c) * (0.993^data0$age)
      var <- c("SEQN", "eGFR")
      eGFR_III <- data0[, var]
    }
    {#*** section 8.11.2 uACR ####
      
      lab_uACR<-as.data.frame(lab$SEQN)
      colnames(lab_uACR)<-c("SEQN")
      lab_uACR$cr<-as.numeric(substring(lab$V1,1956,1960))
      lab_uACR$cr[lab_uACR$cr<10]<-NA
      lab_uACR$cr[lab_uACR$cr==88888]<-NA
      lab_uACR$alb<-as.numeric(substring(lab$V1,1965,1970))
      lab_uACR$alb[lab_uACR$alb==888888]<-NA
      lab_uACR$alb[lab_uACR$alb<0.5]<-NA
      lab_uACR$alb=lab_uACR$alb
      lab_uACR$uACR <- lab_uACR$alb/lab_uACR$cr * 100
      uACR_III <- lab_uACR[, c("SEQN","uACR")]
    }
    {#*** section 8.11.3 Conbine ####
      CKD_dia_III<-merge(eGFR_III,uACR_III,by="SEQN",all=T)
      CKD_dia_III$CKD[CKD_dia_III$eGFR>=60|CKD_dia_III$uACR<=30]<-"NO"
      CKD_dia_III$CKD[CKD_dia_III$eGFR<60|CKD_dia_III$uACR>30]<-"YES"
      CKD_III<-CKD_dia_III[,c("SEQN","CKD")]
      Covariates_III_11<-merge(Covariates_III_10,CKD_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|
                              record=='adult'|record=='exam'|record=='lab')])
    }
}  
{#** section 8.12 Caner ####
      # skin cancer
      cancer<-as.data.frame(adult$SEQN)
      colnames(cancer)<-c("SEQN")
      cancer$skin_ca<-as.numeric(substring(adult$V1,1478,1478))
      cancer$skin_ca[cancer$skin_ca==1]<-"YES"
      cancer$skin_ca[cancer$skin_ca==2]<-"NO"
      cancer$skin_ca[cancer$skin_ca==8]<-NA
      cancer$skin_ca[cancer$skin_ca==9]<-NA
      # other_cancer
      cancer$other_ca<-as.numeric(substring(adult$V1,1479,1479))
      cancer$other_ca[cancer$other_ca==1]<-"YES"
      cancer$other_ca[cancer$other_ca==2]<-"NO"
      cancer$other_ca[cancer$other_ca==8]<-NA
      cancer$other_ca[cancer$other_ca==9]<-NA
      cancer$Cancer[cancer$skin_ca=="YES"|cancer$other_ca=="YES"]<-"YES"
      cancer$Cancer[cancer$skin_ca=="NO"&cancer$other_ca=="NO"]<-"NO"
      cancer_III<-cancer[,c("SEQN","Cancer")]
      Covariates_III_12<-merge(Covariates_III_11,cancer_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|record=='Covariates_III_12'|
                              record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.13 HbA1c  ####
      Hb_data<-as.data.frame(lab$SEQN)
      colnames(Hb_data)<-c("SEQN")
      Hb_data$HbA1c<-as.numeric(substring(lab$V1,1861,1864))
      Hb_data$HbA1c[Hb_data$HbA1c==8888]<-NA
      HbA1c_III<-Hb_data
      Covariates_III_13<-merge(Covariates_III_12,HbA1c_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|record=='Covariates_III_12'|
                              record=='Covariates_III_13'|
                              record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.14 Blood relatives have diabetes  ####
      Br_diabetes_III<-as.data.frame(adult$SEQN)
      colnames(Br_diabetes_III)<-c("SEQN")
      Br_diabetes_III$mcq250a<-as.numeric(substring(adult$V1,1529,1529))
      Br_diabetes_III$mcq250a[Br_diabetes_III$mcq250a==1]<-"YES"
      Br_diabetes_III$mcq250a[Br_diabetes_III$mcq250a==2]<-"NO"
      Br_diabetes_III$mcq250a[Br_diabetes_III$mcq250a==8]<-NA
      Br_diabetes_III$mcq250a[Br_diabetes_III$mcq250a==9]<-NA
      colnames(Br_diabetes_III)<-c("SEQN","Br_diabetes_status")
      Covariates_III_14<-merge(Covariates_III_13,Br_diabetes_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|record=='Covariates_III_12'|
                              record=='Covariates_III_13'|record=='Covariates_III_14'|
                              record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.15 Time since diabetes diagnosis  ####
      duration_year_III<-as.data.frame(adult$SEQN)
      colnames(duration_year_III)<-c("SEQN")
      duration_year_III$dia<-as.numeric(substring(adult$V1,1565,1567))
      duration_year_III$dia[duration_year_III$dia==999]<-NA
      duration_year_III$dia[duration_year_III$dia==888]<-NA
      duration_year_III$Age<-as.numeric(substring(adult$V1,18,19))
      duration_year_III$duration=duration_year_III$Age-duration_year_III$dia
      duration_year_III$duration[duration_year_III$Age==90]<-NA
      duration_III<-duration_year_III[,c("SEQN","duration")]
      duration_III$duration[is.na(duration_III$duration)]<-0.5
      duration_III$duration[duration_III$duration==0]<-0.5
      Covariates_III_15<-merge(Covariates_III_14,duration_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|record=='Covariates_III_12'|
                              record=='Covariates_III_13'|record=='Covariates_III_14'|
                              record=='Covariates_III_15'|
                              record=='adult'|record=='exam'|record=='lab')])
}
{#** section 8.16 diabetes medicine  ####
      medicine_III<-as.data.frame(adult$SEQN)
      colnames(medicine_III)<-c("SEQN")
      medicine_III$insuli<-as.numeric(substring(adult$V1,1568,1568))
      medicine_III$pills<-as.numeric(substring(adult$V1,1578,1578))
      medicine_III$medicine<-"NO"
      medicine_III$medicine[medicine_III$insuli==1|medicine_III$pills==1]<-"YES"
      medicine_III<-medicine_III[,c("SEQN","medicine")]
      Covariates_III_16<-merge(Covariates_III_15,medicine_III,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                              record=='Covariates_III_3'|record=='Covariates_III_4'|
                              record=='Covariates_III_5'|record=='Covariates_III_6'|
                              record=='Covariates_III_7'|record=='Covariates_III_8'|
                              record=='Covariates_III_9'|record=='Covariates_III_10'|
                              record=='Covariates_III_11'|record=='Covariates_III_12'|
                              record=='Covariates_III_13'|record=='Covariates_III_14'|
                              record=='Covariates_III_15'|record=='Covariates_III_16'|
                              record=='adult'|record=='exam'|record=='lab')])
     
}
{#** section 8.17 occupation  ####
  adult_occupation<-as.data.frame(adult$SEQN)
  colnames(adult_occupation)<-"SEQN"
  #occupation_status
  adult_occupation$status<-as.numeric(substring(adult$V1,2371,2372))
  adult_occupation$status[adult_occupation$status==88]<-NA
  adult_occupation$status[adult_occupation$status==1]<-"Working"
  adult_occupation$status[adult_occupation$status==2]<-"Retired"
  adult_occupation$status[adult_occupation$status==3]<-"Keeping house"
  adult_occupation$status[adult_occupation$status==4]<-"Going to school"
  adult_occupation$status[adult_occupation$status==5]<-"Something else"
  adult_occupation$status[adult_occupation$status==7]<-"Disabled"
  adult_occupation$status[adult_occupation$status==8]<-"Unemployment"
  
  adult_occupation$job<-as.numeric(substring(adult$V1,2345,2346))
  
  AA<-as.data.frame(table(adult_occupation$status,adult_occupation$job,useNA="ifany"))
  adult_occupation$job_status[adult_occupation$status=="Retired"]<-"Retired"
  adult_occupation$job_status[adult_occupation$status=="Keeping house"]<-"Unemployment"
  adult_occupation$job_status[adult_occupation$status=="Going to school"]<-"Going to school"
  adult_occupation$job_status[adult_occupation$status=="Something else"]<-"Unemployment"
  adult_occupation$job_status[adult_occupation$status=="Disabled"]<-"Unemployment"
  adult_occupation$job_status[adult_occupation$status=="Unemployment"]<-"Unemployment"
  adult_occupation$job_status[adult_occupation$job==1]<-"Executive/administrators/managers"
  adult_occupation$job_status[adult_occupation$job==2]<-"Management related"
  adult_occupation$job_status[adult_occupation$job==3]<-"Engineers and scientists"
  adult_occupation$job_status[adult_occupation$job==4]<-"Health diagnosing/assessment/treating occupations"
  adult_occupation$job_status[adult_occupation$job==5]<-"Teachers"
  adult_occupation$job_status[adult_occupation$job==6]<-"Writers/artists/entertainers/athletes"
  adult_occupation$job_status[adult_occupation$job==7]<-"Other professional specialty occupations"
  adult_occupation$job_status[adult_occupation$job==8]<-"Technicians and related support occupations"
  adult_occupation$job_status[adult_occupation$job==9]<-"Supervisors/proprietors/sales occupations"
  adult_occupation$job_status[adult_occupation$job==10]<-"Sales representatives/finance/business/commodities except retail"
  adult_occupation$job_status[adult_occupation$job==11]<-"Sales workers/retail/personal services"
  adult_occupation$job_status[adult_occupation$job==12]<-"Secretaries/stenographers/typists"
  adult_occupation$job_status[adult_occupation$job==13]<-"Information clerks"
  adult_occupation$job_status[adult_occupation$job==14]<-"Records processing occupations"
  adult_occupation$job_status[adult_occupation$job==15]<-"Material recording/scheduling/distributing clerks"
  adult_occupation$job_status[adult_occupation$job==16]<-"Miscellaneous administrative support occupations"
  adult_occupation$job_status[adult_occupation$job==17]<-"Private household occupations"
  adult_occupation$job_status[adult_occupation$job==18]<-"Protective service occupations"
  adult_occupation$job_status[adult_occupation$job==19]<-"Waiters and waitresses"
  adult_occupation$job_status[adult_occupation$job==20]<-"Cooks"
  adult_occupation$job_status[adult_occupation$job==21]<-"Miscellaneous food preparation/service occupations"
  adult_occupation$job_status[adult_occupation$job==22]<-"Health service occupations"
  adult_occupation$job_status[adult_occupation$job==23]<-"Cleaning/building serviceoccupations"
  adult_occupation$job_status[adult_occupation$job==24]<-"Personal service occupations"
  adult_occupation$job_status[adult_occupation$job==25]<-"Farm operators/managers/supervisors"
  adult_occupation$job_status[adult_occupation$job==26]<-"Farm and nursery workers"
  adult_occupation$job_status[adult_occupation$job==27]<-"Related agricultural/forestry/fishing occupations"
  adult_occupation$job_status[adult_occupation$job==28]<-"Vehicle and mobile equipment mechanics and repairers"
  adult_occupation$job_status[adult_occupation$job==29]<-"Other mechanics and repairers"
  adult_occupation$job_status[adult_occupation$job==30]<-"Construction trades"
  adult_occupation$job_status[adult_occupation$job==31]<-"Extractive and precision production occupations"
  adult_occupation$job_status[adult_occupation$job==32]<-"Textile/apparel/furnishings machine operators"
  adult_occupation$job_status[adult_occupation$job==33]<-"Machine operators/assorted materials"
  adult_occupation$job_status[adult_occupation$job==34]<-"Fabricators/assemblers/inspectors/samplers"
  adult_occupation$job_status[adult_occupation$job==35]<-"Motor vehicle operators"
  adult_occupation$job_status[adult_occupation$job==36]<-"Other transportation and material moving occupations"
  adult_occupation$job_status[adult_occupation$job==37]<-"Construction laborers"
  adult_occupation$job_status[adult_occupation$job==38]<-"Laborers/except construction"
  adult_occupation$job_status[adult_occupation$job==39]<-"Freight/stock/material movers/hand"
  adult_occupation$job_status[adult_occupation$job==40]<-"Other handlers/equipment cleaners/handlers"
  
  adult_occupation$SEI[adult_occupation$job_status=="Executive/administrators/managers"|
                         adult_occupation$job_status=="Management related"|  
                         adult_occupation$job_status=="Engineers and scientists"|
                         adult_occupation$job_status=="Health diagnosing/assessment/treating occupations"|
                         adult_occupation$job_status=="Teachers"|
                         adult_occupation$job_status=="Writers/artists/entertainers/athletes"|
                         adult_occupation$job_status=="Other professional specialty occupations"|
                         adult_occupation$job_status=="Technicians and related support occupations"|
                         adult_occupation$job_status=="Supervisors/proprietors/sales occupations"|
                         adult_occupation$job_status=="Sales representatives/finance/business/commodities except retail"
  ]<-"Upper"
  adult_occupation$SEI[adult_occupation$job_status=="Sales workers/retail/personal services"|
                         adult_occupation$job_status=="Secretaries/stenographers/typists"|
                         adult_occupation$job_status=="Information clerks"|
                         adult_occupation$job_status=="Records processing occupations"|
                         adult_occupation$job_status=="Material recording/scheduling/distributing clerks"|
                         adult_occupation$job_status=="Private household occupations"|
                         adult_occupation$job_status=="Protective service occupations"|
                         adult_occupation$job_status=="Waiters and waitresses"|
                         adult_occupation$job_status=="Miscellaneous administrative support occupations"|
                         adult_occupation$job_status=="Cooks"|
                         adult_occupation$job_status=="Miscellaneous food preparation/service occupations"|
                         adult_occupation$job_status=="Health service occupations"|
                         adult_occupation$job_status=="Cleaning/building serviceoccupations"|
                         adult_occupation$job_status=="Personal service occupations"|
                         adult_occupation$job_status=="Farm operators/managers/supervisors"|
                         adult_occupation$job_status=="Farm and nursery workers"|
                         adult_occupation$job_status=="Related agricultural/forestry/fishing occupations"|
                         adult_occupation$job_status=="Vehicle and mobile equipment mechanics and repairers"|
                         adult_occupation$job_status=="mechanics and repairers"|
                         adult_occupation$job_status=="Other mechanics and repairers"|
                         adult_occupation$job_status=="Construction trades"|
                         adult_occupation$job_status=="Extractive and precision production occupations"|
                         adult_occupation$job_status=="Textile/apparel/furnishings machine operators"|
                         adult_occupation$job_status=="Machine operators/assorted materials"|
                         adult_occupation$job_status=="Fabricators/assemblers/inspectors/samplers"|
                         adult_occupation$job_status=="Motor vehicle operators"|
                         adult_occupation$job_status=="Other transportation and material moving occupations"|
                         adult_occupation$job_status=="Construction laborers"|
                         adult_occupation$job_status=="Laborers/except construction"|
                         adult_occupation$job_status=="Freight/stock/material movers/hand"|
                         adult_occupation$job_status=="Other handlers/equipment cleaners/handlers"]<-"Lower"
  adult_occupation$SEI[adult_occupation$job_status=="Retired"]<-"Lower"
  adult_occupation$SEI[adult_occupation$job_status=="Unemployment"]<-"Unemployment"
  adult_occupation$SEI[adult_occupation$job_status=="Going to school"]<-"Lower"
  adult_occupation$SEI[adult_occupation$job_status=="Unemployment"]<-"Unemployment"
  adult_occupation$SEI[adult_occupation$job_status=="Unemployment"]<-"Unemployment"
  Occupation_III<-adult_occupation[,c("SEQN","SEI")]
  Covariates_III_17<-merge(Covariates_III_16,Occupation_III,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_III_1'|record=='Covariates_III_2'|
                          record=='Covariates_III_3'|record=='Covariates_III_4'|
                          record=='Covariates_III_5'|record=='Covariates_III_6'|
                          record=='Covariates_III_7'|record=='Covariates_III_8'|
                          record=='Covariates_III_9'|record=='Covariates_III_10'|
                          record=='Covariates_III_11'|record=='Covariates_III_12'|
                          record=='Covariates_III_13'|record=='Covariates_III_14'|
                          record=='Covariates_III_15'|record=='Covariates_III_16'|
                          record=='Covariates_III_17'|record=='adult'|record=='exam'|record=='lab')])
}
Covariates_III<-Covariates_III_17
record<-ls()
rm(list=record[-which(record=='Covariates_III')])
save(Covariates_III,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/covariates_III.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 9. Covariates of NHANES 1999-2004 <<<<< ####
{#** section 9.1 Age,Gender,Race/ethnicity,Education_levels,PIR  ####
    nhs_tsv('demo')[1:3]
    DEMO_DATA<-nhs_read(nhs_tsv('demo')[1:3],"RIDRETH2:Race_ethnicity","RIAGENDR:Gender",
                        "RIDAGEYR:Age","DMDEDUC2:Education_levels","INDFMPIR:INDFMPIR")
    DEMO_CON<-DEMO_DATA[,c("seqn","Year","Age","Gender","Race_ethnicity","Education_levels","INDFMPIR")]
    colnames(DEMO_CON)[1]<-"SEQN"
    table(DEMO_CON$Education_levels)
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less Than 9th Grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th Grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High School Grad/GED or Equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some College or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College Graduate or above"]<-"College_or_above"
    table(DEMO_CON$`Race_ethnicity`)
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Race - Including Multi-Racial"]<-"Other_Race"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Mexican American"]<-"Hispanic"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Hispanic"]<-"Hispanic"
    DEMO_CON$PIR_raw=DEMO_CON$INDFMPIR
    DEMO_CON$PIR[DEMO_CON$INDFMPIR<1.30]<-"<1.30"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=1.30&DEMO_CON$INDFMPIR<3]<-"1.30–2.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=3&DEMO_CON$INDFMPIR<5]<-"3.00–4.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=5]<-">=5.00"
    DEMO_CON$INDFMPIR<-NULL
    Covariates_CON1_1<-DEMO_CON
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1')])
}
{#** section 9.2 health insurance  ####
    nhs_tsv('HIQ')[1:3]
    Insurance_DATA<-nhs_read(nhs_tsv('HIQ')[1:3],"HID010:insurance","HID030A:Private","HID030B:Medicare",
                             "HID030C:Medicaid","HID030D:Others","HID030E:Other",codebook = F)
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==2]<-"No_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&(Insurance_DATA$Medicare==1|
                                                                   Insurance_DATA$Medicaid==1|Insurance_DATA$Others==1|Insurance_DATA$Other==1)]<-"Public_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&Insurance_DATA$Private==1]<-"Private_insurance"
    
    Insurance_CON<-Insurance_DATA[,c("seqn","Health_insurance")]
    table(Insurance_DATA$Health_insurance)
    colnames(Insurance_CON)[1]<-"SEQN"
    Covariates_CON1_2<-merge(Covariates_CON1_1,Insurance_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2')])
}
{#** section 9.3 Smoking status  ####
    nhs_tsv('smq')[c(1,4,7)]
    #SMQ020 - Smoked at least 100 cigarettes in life
    #SMQ040 - Do you now smoke cigarettes
    SMQ_DATA<-nhs_read(nhs_tsv('smq')[c(1,4,7)],"SMQ020","SMQ040",Year = F,codebook = F)
    table(SMQ_DATA$SMQ040)
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==1|SMQ_DATA$SMQ040==2)]<-
      "Current_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==3)]<-
      "Former_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==2]<-
      "Never_smoker"
    SMQ_CON<-SMQ_DATA[,c("seqn","Smoking_status")]
    colnames(SMQ_CON)[1]<-"SEQN"
    Covariates_CON1_3<-merge(Covariates_CON1_2,SMQ_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3')])
}
{#** section 9.4 Drinking status  ####
    Drink_DATA<-nhs_read(nhs_tsv('alq')[1:3],"ALQ110","ALQ100","ALD100:ALQ100","ALQ101:ALQ100","ALQ120Q","ALQ120U","ALQ130","ALQ140Q","ALQ140U","ALQ150",Year = F,codebook = F)
    #ALQ100 - Had at least 12 alcohol drinks/1 yr?
    #ALQ110 - Had at least 12 alcohol drinks/lifetime?
    #ALQ120Q - How often drink alcohol over past 12 mos
    #ALQ130 - Avg # alcoholic drinks/day -past 12 mos
    #days have 5 or more drinks/past 12 mos
    #Ever have 5 or more drinks every day?
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==777]<-NA
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==999]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==77]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==99]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==777]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==999]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==7]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==7]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==1]<-(365/7)
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==2]<-12
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==3]<-1
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==7]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==9]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==1]<-(365/7)
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==2]<-12
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==3]<-1
    gender_DATA<-nhs_read(nhs_tsv('demo')[1:3],"RIAGENDR:Gender",Year = F,codebook = T)
    Drink_DATA<-merge(Drink_DATA,gender_DATA,by = "seqn",all.x = T)
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2|Drink_DATA$ALQ150==2]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|Drink_DATA$ALQ140Q>=3]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male")))&Drink_DATA$ALQ120Q>=1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ150==1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1)&Drink_DATA$Gender=="Female")|
                                  ((Drink_DATA$ALQ130<=2)&Drink_DATA$Gender=="Male"))&Drink_DATA$ALQ120Q>=1]<-
      "Light/moderate_drinker"
    
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2]<-"Nondrinker"
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|(Drink_DATA$ALQ120Q*Drink_DATA$ALQ130>=12)|Drink_DATA$ALQ150==1]<-"Drinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Nondrinker"]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Drinker"]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130<=2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Light/moderate_drinker"
    
    table(Drink_DATA$Drinking_status,useNA = "ifany")
    Drink_CON<-Drink_DATA[,c("seqn","Drinking_status")]
    colnames(Drink_CON)[1]<-"SEQN"
    Covariates_CON1_4<-merge(Covariates_CON1_3,Drink_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4')])
}
table(CON_baseline_4$Drinking_status,useNA = "ifany")
{#** section 9.5 Physical_activity  ####
    Physical_DATA<-nhs_read(nhs_tsv('paq')[c(1,3,5)],"PAD020","PAQ050Q","PAQ050U","PAQ100","PAD120","PAD200",
                            "PAD440","PAD460","PAQ560","PAD320","PAD440",
                            Year = F,codebook = F)
    # PAD020 - Walked or bicycled over past 30 days
    # PAQ050Q - # times walked or bicycled
    # PAQ050U - Unit of measure (day/week/month)
    # PAD080 - How long per day (minutes)
    # PAQ100 - Tasks around home/yard past 30 days
    # PAD120 - # of times past 30 days
    # PAD160 - How long each time (minutes)
    # PAQ180 - Avg level of physical activity each day
    # PAD200 - Vigorous activity over past 30 days
    # PAD320 - Moderate activity over past 30 days
    # PAD440 - Muscle strengthening activities
    # PAD460 - Number of times past 30 days
    # PAQ480 - Daily hours of TV, video or computer use
    # PAQ500 - Activity comparison last mo - last yr
    # PAQ520 - Compare activity w/others same age
    # PAQ540 - Compare activity with 10 years ago
    # PAQ560 - # time/week you play or exercise hard
    # PAD570 - # of hours watch TV or videos yesterday
    # PAQ580 - # hours use computer/games yesterday
    Physical_DATA$PAQ050Q[Physical_DATA$PAQ050Q==77777]<-NA
    Physical_DATA$PAQ050Q[Physical_DATA$PAQ050Q==99999]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==7]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==9]<-NA
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==1]<-30
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==2]<-(30/7)
    Physical_DATA$PAQ050U[Physical_DATA$PAQ050U==3]<-1
    Physical_DATA$PAD120[Physical_DATA$PAD120==77777]<-NA
    Physical_DATA$PAD120[Physical_DATA$PAD120==99999]<-NA
    Physical_DATA$PAD460[Physical_DATA$PAD460==77777]<-NA
    Physical_DATA$PAD460[Physical_DATA$PAD460==99999]<-NA
    Physical_DATA$PAQ560[Physical_DATA$PAQ560==77777]<-NA
    Physical_DATA$PAQ560[Physical_DATA$PAQ560==99999]<-NA
    Physical_DATA$Physical_status[Physical_DATA$PAD020==1|Physical_DATA$PAQ100==1|Physical_DATA$PAD200==1|
                                    Physical_DATA$PAD320==1|Physical_DATA$PAD440==1]<-"Insufficient"
    Physical_DATA$Physical_status[(Physical_DATA$PAD320==2|Physical_DATA$PAD320==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAQ100==2|Physical_DATA$PAQ100==3|is.na(Physical_DATA$PAQ100))&
                                    (Physical_DATA$PAD320==2|Physical_DATA$PAD320==3|is.na(Physical_DATA$PAD320))&
                                    (Physical_DATA$PAD440==2|Physical_DATA$PAD440==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAD200==2|Physical_DATA$PAD200==3|is.na(Physical_DATA$PAD020))&
                                    (Physical_DATA$PAD020==2|Physical_DATA$PAD020==3|is.na(Physical_DATA$PAD020))]<-"Inactive"
    Physical_DATA$Physical_status[is.na(Physical_DATA$PAD020)&is.na(Physical_DATA$PAD100)&is.na(Physical_DATA$PAD200)&is.na(Physical_DATA$PAD320)&is.na(Physical_DATA$PAD440)]<-NA
    Physical_DATA$PAQ050Q[is.na(Physical_DATA$PAQ050Q)]<-0
    Physical_DATA$PAQ050U[is.na(Physical_DATA$PAQ050U)]<-0    
    Physical_DATA$PAD120[is.na(Physical_DATA$PAQ050Q)]<-0    
    Physical_DATA$PAD460[is.na(Physical_DATA$PAD460)]<-0
    Physical_DATA$PAQ560[is.na(Physical_DATA$PAQ560)]<-0
    Physical_DATA$Physical_status[(((Physical_DATA$PAQ050Q*Physical_DATA$PAQ050U)/30*7)+(Physical_DATA$PAD120/30*7)+
                                     (Physical_DATA$PAD460/30*7)>=5)|(Physical_DATA$PAQ560>=3)]<-"Recommended"
    table(Physical_DATA$Physical_status,useNA = 'ifany')
    
    Physical_CON<-Physical_DATA[,c("seqn","Physical_status")]
    colnames(Physical_CON)[1]<-"SEQN"
    Covariates_CON1_5<-merge(Covariates_CON1_4,Physical_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5')])
}
{#** section 9.6 HEI  ####
    nhs_tsv("drxtot|dr1tot")[1:3]
    data_HEI<-dex_HEI(years = 1999:2004,version = "2015",method = "ssum",dietary = "tot",day = 1)
    HEI_data<-nhs_read(nhs_tsv("drxtot|dr1tot")[1:3],'WTDRD1:weight2','WTDR4YR:weight4',Year = T,codebook = F)
    CON_HEI<-merge(data_HEI[,c("seqn","hei2015_total_score")],HEI_data[,c("seqn","weight2","weight4",'Year')],by="seqn",all.x = T)
    CON_HEI <- CON_HEI %>% mutate(weight = ifelse(Year=="1999-2000"|Year=="2001-2002",weight4*2/3,weight2/3))
    colnames(CON_HEI)[1]<-"SEQN"
    load("/media/lee/paper_record/paper_2_PD&DM/data_DC/CON1_baseline.Rdata")
    CON_HEI<-merge(CON_HEI,CON1_baseline[,c("SEQN","ID")],by="SEQN",all.y = T)
    Quantile<-weightedQuantile(CON_HEI$hei2015_total_score,weights = CON_HEI$weight, probs=c(0.2,0.4,0.6,0.8),  na.rm=TRUE)
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[1]]<-"Quintile 1"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[2]&CON_HEI$hei2015_total_score>Quantile[1]]<-"Quintile 2"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[3]&CON_HEI$hei2015_total_score>Quantile[2]]<-"Quintile 3"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[4]&CON_HEI$hei2015_total_score>Quantile[3]]<-"Quintile 4"
    CON_HEI$HEI[CON_HEI$hei2015_total_score>Quantile[4]]<-"Quintile 5"
    table(CON_HEI$HEI)
    colnames(CON_HEI)[2]<-"HEI_Score"
    HEI_CON<-CON_HEI[,c("SEQN","HEI_Score","HEI")]
    Covariates_CON1_6<-merge(Covariates_CON1_5,HEI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6')])
}
{#** section 9.7 BMI  ####
    BMI_CON<-nhs_read(nhs_tsv("bmx")[1:3],'BMXBMI:BMI',Year = F,codebook = F)
    BMI_CON$BMI_Grade[BMI_CON$BMI<18.5]<-"<18.5"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=18.5&BMI_CON$BMI<25]<-"[18.5,25.0)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=25&BMI_CON$BMI<30]<-"[25.0-30)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=30]<-">=30"
    colnames(BMI_CON)[1]<-"SEQN"
    Covariates_CON1_7<-merge(Covariates_CON1_6,BMI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7')])
}
{#** section 9.8 Hypertension  ####
    HTN_exam_CON<-diag_Hypertension(told = F,drug = F,bpx = TRUE,method = c("mean"),years = 1999:2004,join = "left")
    colnames(HTN_exam_CON)<-c("SEQN","HTN_exam_status")
    nhs_tsv("bpq")[1:3]
    HTN_self_CON<-nhs_read(nhs_tsv("bpq")[1:3],'BPQ020','BPQ030','BPQ040A','BPQ040B','BPQ040C',
                           'BPQ040D','BPQ040E','BPQ040F','BPQ043A','BPQ043B','BPQ043C','BPQ043D',Year = F,codebook = F)
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==2]<-"NO"
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==1|HTN_self_CON$BPQ030==1|HTN_self_CON$BPQ040A==1|HTN_self_CON$BPQ040B==1|
                                   HTN_self_CON$BPQ040C==1|HTN_self_CON$BPQ040D==1|HTN_self_CON$BPQ040E==1|
                                   HTN_self_CON$BPQ040F==1|HTN_self_CON$BPQ043A==1|HTN_self_CON$BPQ043B==1|
                                   HTN_self_CON$BPQ043C==1|HTN_self_CON$BPQ043D==1]<-"YES"
    colnames(HTN_self_CON)[1]<-"SEQN"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="no"]<-"NO"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="yes"]<-"YES"
    HTN_CON<-merge(HTN_exam_CON,HTN_self_CON,by = "SEQN",all = T)
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="NO"|HTN_CON$HTN_self_status=="NO"]<-"NO"
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="YES"|HTN_CON$HTN_self_status=="YES"]<-"YES"
    HTN_CON<-HTN_CON[,c("SEQN","HTN_status")]
    table(HTN_CON$HTN_status)
    Covariates_CON1_8<-merge(Covariates_CON1_7,HTN_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8')])
}
{#** section 9.9 hyperlipoidemia  ####
    HPL_exam_CON<-HTN_self_CON<-nhs_read(nhs_tsv("lab13|l13")[c(1,4,6)],'LBXTC',Year = F,codebook = F)
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC>=200]<-"YES"
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC<200]<-"NO"
    colnames(HPL_exam_CON)[1]<-"SEQN"
    HPL_self_CON<-nhs_read(nhs_tsv("bpq")[1:3],'BPQ080','BPQ090A','BPQ090B','BPQ090C','BPQ090D',Year = F,codebook = F)
    HPL_self_CON$HPL_self_status<-NULL
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==2]<-"NO"
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==1|HPL_self_CON$BPQ090A==1|
                                   HPL_self_CON$BPQ090B==1|HPL_self_CON$BPQ090C==1|
                                   HPL_self_CON$BPQ090D==1]<-"YES"
    colnames(HPL_self_CON)[1]<-"SEQN"
    HPL_CON<-merge(HPL_exam_CON,HPL_self_CON,by = "SEQN",all = T)
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="NO"|HPL_CON$HPL_self_status=="NO"]<-"NO"
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="YES"|HPL_CON$HPL_self_status=="YES"]<-"YES"
    HPL_CON<-HPL_CON[,c("SEQN","HPL_status")]
    Covariates_CON1_9<-merge(Covariates_CON1_8,HPL_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9')])
}
{#** section 9.10 CVD ####
    #CVD
    CVD_CON<-diag_CVD(years = 1999:2004,join = "left")
    table(CVD_CON$CVD)
    CVD_CON$CVD[CVD_CON$CVD=="no"]<-"NO"
    CVD_CON$CVD[CVD_CON$CVD=="yes"]<-"YES"
    colnames(CVD_CON)[1]<-'SEQN'
    Covariates_CON1_10<-merge(Covariates_CON1_9,CVD_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10')])
}
{#** section 9.11 CKD ####
    {#*** section 9.11.1 eGFR ####
      years <- 1999:2004
      (demo <- nhs_tsv("demo", items = "demo", years = years, 
                       cat = FALSE))
      (bm <- nhs_tsv("bmx", items = "exam", years = years, cat = FALSE))
      (biopro <- nhs_tsv("lab18\\.|l40_b\\.|l40_c\\.|biopro", 
                         items = "lab", years = years, cat = FALSE))
      (cyst <- nhs_tsv("sscyst_", items = "lab", years = years, 
                       cat = FALSE))
      data0 <- nhs_read(demo, "ridageyr:age", "riagendr:sex", 
                        "ridreth1:eth1", "ridreth2:eth2", "ridreth3:eth3", bm, 
                        "bmxwt:weight", "bmxht:height", biopro, "lbxscr,lbdscr:scr", 
                        "lbxsal:alb", "lbxsbu:bun", cyst, "sscypc:SCysC", lower_cd = TRUE, 
                        cat = FALSE, Year = TRUE)
      data0$scr[data0$Year %in% "1999-2000"] <- 1.013 * data0$scr[data0$Year %in% 
                                                                    "1999-2000"] + 0.147
      data0$eth1 <- as.numeric(Recode(data0$eth1, "non-hispanic black::1", 
                                      "non-hispanic white::0", "other race - including multi-racial::0", 
                                      "mexican american::0", "other hispanic::0"))
      data0$eth2 <- as.numeric(Recode(data0$eth2, "non-hispanic black::1", 
                                      "non-hispanic white::0", "other race - including multi-racial::0", 
                                      "mexican american::0", "other hispanic::0"))
      data0$black <- ifelse(row.sums(data0[, c("eth1", "eth2")]) > 0, "black", "no")
      a <- rep(NA, length(data0$black))
      a[data0$black == "black" & data0$sex == "female"] <- 166
      a[data0$black == "black" & data0$sex == "male"] <- 163
      a[data0$black != "black" & data0$sex == "female"] <- 144
      a[data0$black != "black" & data0$sex == "male"] <- 141
      b <- ifelse(data0$sex == "female", 0.7, 0.9)
      c <- rep(NA, length(data0$black))
      c[data0$sex == "female" & data0$scr <= 0.7] <- -0.329
      c[data0$sex == "female" & data0$scr > 0.7] <- -1.209
      c[data0$sex == "male" & data0$scr <= 0.9] <- -0.411
      c[data0$sex == "male" & data0$scr > 0.9] <- -1.209
      data0$eGFR <- a * ((data0$scr/b)^c) * (0.993^data0$age)
      var <- c("seqn", "eGFR")
      eGFR_CON <- data0[, var]
    }
    {#*** section 9.11.2 uACR ####
      uACR_CON <- nhs_read(nhs_tsv("lab16\\.|l16_b\\.|l16_c\\."), "urxucr:cr", "urxumasi:alb", cat = F)
      uACR_CON$uACR <- uACR_CON$alb/uACR_CON$cr * 100
      uACR_CON <- uACR_CON[, c("seqn","uACR")]
    }
    {#*** section 9.11.3 Conbine ####
      CKD_dia_CON<-merge(eGFR_CON,uACR_CON,by="seqn",all=T)
      CKD_dia_CON$CKD[CKD_dia_CON$eGFR>=60|CKD_dia_CON$uACR<=30]<-"NO"
      CKD_dia_CON$CKD[CKD_dia_CON$eGFR<60|CKD_dia_CON$uACR>30]<-"YES"
      CKD_CON<-CKD_dia_CON[,c("seqn","CKD")]
      colnames(CKD_CON)[1]<-'SEQN'
      Covariates_CON1_11<-merge(Covariates_CON1_10,CKD_CON,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                              record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                              record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                              record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                              record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                              record=='Covariates_CON1_11')])
    }
}
{#** section 9.12 Caner ####
    #Cancer
    tsv<-nhs_tsv('mcq',years = 1999:2004)
    Cancer_CON<-nhs_read(tsv,'mcq220:Cancer',Year = F)
    Cancer_CON$Cancer[Cancer_CON$Cancer=="Yes"]<-"YES"
    Cancer_CON$Cancer[Cancer_CON$Cancer=="No"]<-"NO"
    colnames(Cancer_CON)[1]<-'SEQN'
    Covariates_CON1_12<-merge(Covariates_CON1_11,Cancer_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                            record=='Covariates_CON1_11'|record=='Covariates_CON1_12')])
}
{#** section 9.13 HbA1c  ####
    HbA1c_CON<-nhs_read(nhs_tsv("lab10|l10_b.|l10_c")[c(1,3,4)],'lbxgh',Year = F,codebook = F)
    colnames(HbA1c_CON)<-c("SEQN","HbA1c")
    Covariates_CON1_13<-merge(Covariates_CON1_12,HbA1c_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                            record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                            record=='Covariates_CON1_13')])
}
{#** section 9.14 Blood relatives have diabetes  ####
    nhs_tsv("mcq")[1:3]
    Br_diabetes_CON<-nhs_read(nhs_tsv("mcq")[1:3],'mcq250a',Year = F,codebook = F)
    Br_diabetes_CON$mcq250a[Br_diabetes_CON$mcq250a==1]<-"YES"
    Br_diabetes_CON$mcq250a[Br_diabetes_CON$mcq250a==2]<-"NO"
    Br_diabetes_CON$mcq250a[Br_diabetes_CON$mcq250a==7]<-NA
    Br_diabetes_CON$mcq250a[Br_diabetes_CON$mcq250a==9]<-NA
    colnames(Br_diabetes_CON)<-c("SEQN","Br_diabetes_status")
    Covariates_CON1_14<-merge(Covariates_CON1_13,Br_diabetes_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                            record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                            record=='Covariates_CON1_13'|record=='Covariates_CON1_14')])
}
{#** section 9.15 Time since diabetes diagnosis  ####
    {#*** section 9.15.1 Time since diabetes diagnosis-PFQ  ####
      #9900
      duration9900_CON<-nhs_read(nhs_tsv("PFQ")[1],"PFQ069fG","PFQ069fQ",'PFQ069fU',Year = F,codebook = F)
      duration9900_CON$dura=0
      duration9900_CON$PFQ069fG[duration9900_CON$PFQ069fG==7]<-NA
      duration9900_CON$PFQ069fG[duration9900_CON$PFQ069fG==9]<-NA
      duration9900_CON$dura[duration9900_CON$PFQ069fG==1]<-1
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==1]<-1
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==2]<-7
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==3]<-30
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==4]<-365
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==7]<-NA
      duration9900_CON$PFQ069fU[duration9900_CON$PFQ069fU==9]<-NA
      duration9900_CON$PFQ069fQ[duration9900_CON$PFQ069fQ==77777]<-NA
      duration9900_CON$PFQ069fQ[duration9900_CON$PFQ069fQ==99999]<-NA
      duration9900_CON$PFQ069fQ[is.na(duration9900_CON$PFQ069fQ)]<-0
      duration9900_CON$PFQ069fU[is.na(duration9900_CON$PFQ069fU)]<-0
      duration9900_CON$duration=(duration9900_CON$dura*duration9900_CON$PFQ069fU*duration9900_CON$PFQ069fQ)/365
      duration9900_CON<-na.omit(duration9900_CON)
      #01-04
      duration0104_CON<-nhs_read(nhs_tsv("PFQ")[2:3],'PFD069F',Year = F,codebook = F)
      colnames(duration0104_CON)[2]<-"duration"
      duration0104_CON$duration[duration0104_CON$duration==66666]<-NA
      duration0104_CON$duration[duration0104_CON$duration==77777]<-NA
      duration0104_CON$duration[duration0104_CON$duration==99999]<-NA
      duration0104_CON<-na.omit(duration0104_CON)
      duration0104_CON$duration<-duration0104_CON$duration/365
      #MERGE
      duration_day_CON<-rbind(duration9900_CON[,c("seqn","duration")],duration0104_CON)
    }
    {#*** section 9.15.2 Time since diabetes diagnosis-DIQ  ####
      #9900
      duration_year_CON<-nhs_read(nhs_tsv("DIQ")[1:3],"DIQ040/media/lee/paper_record/DIQ040G","DIQ040Q:DIQ040Q","DID040/media/lee/paper_record/DIQ040G","DID040Q:DIQ040Q",nhs_tsv("DEMO")[1:3],"RIDAGEYR:Age",Year = F,codebook = F)
      duration_year_CON$DIQ040G[duration_year_CON$DIQ040G==2]<-0
      duration_year_CON$DIQ040G[duration_year_CON$DIQ040G==7]<-NA
      duration_year_CON$DIQ040G[duration_year_CON$DIQ040G==9]<-NA
      duration_year_CON$DIQ040Q[duration_year_CON$DIQ040Q==99999]<-NA
      duration_year_CON$DIQ040Q[duration_year_CON$DIQ040Q==77777]<-NA
      duration_year_CON$dura<-0
      duration_year_CON$dura[duration_year_CON$DIQ040G==1]<-1
      duration_year_CON$DIQ040Q[is.na(duration_year_CON$DIQ040Q)]<-0
      duration_year_CON$Age[is.na(duration_year_CON$Age)]<-0
      duration_year_CON$DIQ040G[duration_year_CON$Age==85]<-NA
      duration_year_CON$duration<-duration_year_CON$dura*(duration_year_CON$Age-duration_year_CON$DIQ040Q)
      duration_year_CON$DIQ040Q[duration_year_CON$DIQ040Q==0]<-NA
      duration_year_CON<-na.omit(duration_year_CON)
    }
    {#*** section 9.15.3 Time since diabetes diagnosis-all  ####
      duration_CON<-merge(duration_day_CON,duration_year_CON,by = "seqn",all = T)
      rownames(duration_CON)<-duration_CON$seqn
      duration_CON<-duration_CON[,c("duration.x","duration.y")]
      duration_CON$duration<-rowMeans(duration_CON,na.rm = T)
      duration_CON$SEQN<-rownames(duration_CON)
      duration_CON<-duration_CON[,c("SEQN","duration")]
      duration_CON$duration[is.na(duration_CON$duration)]<-0.5
      duration_CON$duration[duration_CON$duration==0]<-0.5
      Covariates_CON1_15<-merge(Covariates_CON1_14,duration_CON,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                              record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                              record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                              record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                              record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                              record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                              record=='Covariates_CON1_13'|record=='Covariates_CON1_14'|
                              record=='Covariates_CON1_15')])
    }
}
{#** section 9.16 diabetes medicine  ####
    medicine_CON<-nhs_read(nhs_tsv("DIQ")[1:3],"DIQ050:insuli","DIQ070:pills",Year = F,codebook = F)
    medicine_CON$medicine<-"NO"
    medicine_CON$medicine[medicine_CON$insuli==1|medicine_CON$pills==1]<-"YES"
    medicine_CON<-medicine_CON[,c("seqn","medicine")]
    colnames(medicine_CON)[1]<-"SEQN"
    Covariates_CON1_16<-merge(Covariates_CON1_15,medicine_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                            record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                            record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                            record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                            record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                            record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                            record=='Covariates_CON1_13'|record=='Covariates_CON1_14'|
                            record=='Covariates_CON1_15'|record=='Covariates_CON1_16')])
}
{#** section 9.17 occupation  ####
  occupation_CON<-HTN_self_CON<-nhs_read(nhs_tsv("ocq")[1:3],'OCD240:job','OCQ380:status',Year = F,codebook = F)
  table(occupation_CON$job)
  occupation_CON$status[occupation_CON$status==1]<-"Keeping house"
  occupation_CON$status[occupation_CON$status==2]<-"Going to school"
  occupation_CON$status[occupation_CON$status==3]<-"Retired"
  occupation_CON$status[occupation_CON$status==4]<-"Disabled"
  occupation_CON$status[occupation_CON$status==5]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==6]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==7]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==77]<-NA
  occupation_CON$status[occupation_CON$status==99]<-NA
  occupation_CON$job_status[occupation_CON$status=="Retired"]<-"Retired"
  occupation_CON$job_status[occupation_CON$status=="Keeping house"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Going to school"]<-"Going to school"
  occupation_CON$job_status[occupation_CON$status=="Something else"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Disabled"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Unemployment"]<-"Unemployment"
  
  
  occupation_CON$job_status[occupation_CON$job==1]<-"Executive/administrators/managers"
  occupation_CON$job_status[occupation_CON$job==2]<-"Management related"
  occupation_CON$job_status[occupation_CON$job==3]<-"Engineers and scientists"
  occupation_CON$job_status[occupation_CON$job==4]<-"Health diagnosing/assessment/treating occupations"
  occupation_CON$job_status[occupation_CON$job==5]<-"Teachers"
  occupation_CON$job_status[occupation_CON$job==6]<-"Writers/artists/entertainers/athletes"
  occupation_CON$job_status[occupation_CON$job==7]<-"Other professional specialty occupations"
  occupation_CON$job_status[occupation_CON$job==8]<-"Technicians and related support occupations"
  occupation_CON$job_status[occupation_CON$job==9]<-"Supervisors/proprietors/sales occupations"
  occupation_CON$job_status[occupation_CON$job==10]<-"Sales representatives/finance/business/commodities except retail"
  occupation_CON$job_status[occupation_CON$job==11]<-"Sales workers/retail/personal services"
  occupation_CON$job_status[occupation_CON$job==12]<-"Secretaries/stenographers/typists"
  occupation_CON$job_status[occupation_CON$job==13]<-"Information clerks"
  occupation_CON$job_status[occupation_CON$job==14]<-"Records processing occupations"
  occupation_CON$job_status[occupation_CON$job==15]<-"Material recording/scheduling/distributing clerks"
  occupation_CON$job_status[occupation_CON$job==16]<-"Miscellaneous administrative support occupations"
  occupation_CON$job_status[occupation_CON$job==17]<-"Private household occupations"
  occupation_CON$job_status[occupation_CON$job==18]<-"Protective service occupations"
  occupation_CON$job_status[occupation_CON$job==19]<-"Waiters and waitresses"
  occupation_CON$job_status[occupation_CON$job==20]<-"Cooks"
  occupation_CON$job_status[occupation_CON$job==21]<-"Miscellaneous food preparation/service occupations"
  occupation_CON$job_status[occupation_CON$job==22]<-"Health service occupations"
  occupation_CON$job_status[occupation_CON$job==23]<-"Cleaning/building serviceoccupations"
  occupation_CON$job_status[occupation_CON$job==24]<-"Personal service occupations"
  occupation_CON$job_status[occupation_CON$job==25]<-"Farm operators/managers/supervisors"
  occupation_CON$job_status[occupation_CON$job==26]<-"Farm and nursery workers"
  occupation_CON$job_status[occupation_CON$job==27]<-"Related agricultural/forestry/fishing occupations"
  occupation_CON$job_status[occupation_CON$job==28]<-"Vehicle and mobile equipment mechanics and repairers"
  occupation_CON$job_status[occupation_CON$job==29]<-"Other mechanics and repairers"
  occupation_CON$job_status[occupation_CON$job==30]<-"Construction trades"
  occupation_CON$job_status[occupation_CON$job==31]<-"Extractive and precision production occupations"
  occupation_CON$job_status[occupation_CON$job==32]<-"Textile/apparel/furnishings machine operators"
  occupation_CON$job_status[occupation_CON$job==33]<-"Machine operators/assorted materials"
  occupation_CON$job_status[occupation_CON$job==34]<-"Fabricators/assemblers/inspectors/samplers"
  occupation_CON$job_status[occupation_CON$job==35]<-"Motor vehicle operators"
  occupation_CON$job_status[occupation_CON$job==36]<-"Other transportation and material moving occupations"
  occupation_CON$job_status[occupation_CON$job==37]<-"Construction laborers"
  occupation_CON$job_status[occupation_CON$job==38]<-"Laborers/except construction"
  occupation_CON$job_status[occupation_CON$job==39]<-"Freight/stock/material movers/hand"
  occupation_CON$job_status[occupation_CON$job==40]<-"Other handlers/equipment cleaners/handlers"
  occupation_CON$job_status[occupation_CON$job==41]<-"Military occupations"
  
  occupation_CON$SEI[occupation_CON$job_status=="Executive/administrators/managers"|
                       occupation_CON$job_status=="Management related"|  
                       occupation_CON$job_status=="Engineers and scientists"|
                       occupation_CON$job_status=="Health diagnosing/assessment/treating occupations"|
                       occupation_CON$job_status=="Teachers"|
                       occupation_CON$job_status=="Writers/artists/entertainers/athletes"|
                       occupation_CON$job_status=="Other professional specialty occupations"|
                       occupation_CON$job_status=="Technicians and related support occupations"|
                       occupation_CON$job_status=="Supervisors/proprietors/sales occupations"|
                       occupation_CON$job_status=="Sales representatives/finance/business/commodities except retail"|
                       occupation_CON$job_status=="Military occupations"
  ]<-"Upper"
  occupation_CON$SEI[occupation_CON$job_status=="Sales workers/retail/personal services"|
                       occupation_CON$job_status=="Secretaries/stenographers/typists"|
                       occupation_CON$job_status=="Information clerks"|
                       occupation_CON$job_status=="Records processing occupations"|
                       occupation_CON$job_status=="Material recording/scheduling/distributing clerks"|
                       occupation_CON$job_status=="Private household occupations"|
                       occupation_CON$job_status=="Protective service occupations"|
                       occupation_CON$job_status=="Waiters and waitresses"|
                       occupation_CON$job_status=="Miscellaneous administrative support occupations"|
                       occupation_CON$job_status=="Cooks"|
                       occupation_CON$job_status=="Miscellaneous food preparation/service occupations"|
                       occupation_CON$job_status=="Health service occupations"|
                       occupation_CON$job_status=="Cleaning/building serviceoccupations"|
                       occupation_CON$job_status=="Personal service occupations"|
                       occupation_CON$job_status=="Farm operators/managers/supervisors"|
                       occupation_CON$job_status=="Farm and nursery workers"|
                       occupation_CON$job_status=="Related agricultural/forestry/fishing occupations"|
                       occupation_CON$job_status=="Vehicle and mobile equipment mechanics and repairers"|
                       occupation_CON$job_status=="mechanics and repairers"|
                       occupation_CON$job_status=="Other mechanics and repairers"|
                       occupation_CON$job_status=="Construction trades"|
                       occupation_CON$job_status=="Extractive and precision production occupations"|
                       occupation_CON$job_status=="Textile/apparel/furnishings machine operators"|
                       occupation_CON$job_status=="Machine operators/assorted materials"|
                       occupation_CON$job_status=="Fabricators/assemblers/inspectors/samplers"|
                       occupation_CON$job_status=="Motor vehicle operators"|
                       occupation_CON$job_status=="Other transportation and material moving occupations"|
                       occupation_CON$job_status=="Construction laborers"|
                       occupation_CON$job_status=="Laborers/except construction"|
                       occupation_CON$job_status=="Freight/stock/material movers/hand"|
                       occupation_CON$job_status=="Other handlers/equipment cleaners/handlers"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Retired"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Going to school"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON<-occupation_CON[,c("seqn","SEI")]
  colnames(occupation_CON)[1]<-"SEQN"
  Covariates_CON1_17<-merge(Covariates_CON1_16,occupation_CON,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                          record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                          record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                          record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                          record=='Covariates_CON1_13'|record=='Covariates_CON1_14'|
                          record=='Covariates_CON1_15'|record=='Covariates_CON1_16'|
                          record=='Covariates_CON1_17')])
}
Covariates_CON1<-Covariates_CON1_17
record<-ls()
rm(list=record[-which(record=='Covariates_CON1')])
save(Covariates_CON1,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Covariates_CON1.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 10. Covariates of NHANES 2009-2014 <<<<< ####
{#** section 10.1 Age,Gender,Race/ethnicity,Education_levels,PIR  ####
    nhs_tsv('demo')[6:8]
    DEMO_DATA<-nhs_read(nhs_tsv('demo')[6:8],"RIDRETH1:Race_ethnicity","RIAGENDR:Gender",
                        "RIDAGEYR:Age","DMDEDUC2:Education_levels","INDFMPIR:INDFMPIR")
    DEMO_CON<-DEMO_DATA[,c("seqn","Year","Age","Gender","Race_ethnicity","Education_levels","INDFMPIR")]
    colnames(DEMO_CON)[1]<-"SEQN"
    table(DEMO_CON$Education_levels)
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less Than 9th Grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Less than 9th grade"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th Grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="9-11th grade (Includes 12th grade with no diploma)"]<-"Less_than_high_school"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High School Grad/GED or Equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="High school graduate/GED or equivalent"]<-"High_school_or_Equivalent"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some College or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="Some college or AA degree"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College Graduate or above"]<-"College_or_above"
    DEMO_CON$Education_levels[DEMO_CON$Education_levels=="College graduate or above"]<-"College_or_above"
    table(DEMO_CON$`Race_ethnicity`)
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Race - Including Multi-Racial"]<-"Other_Race"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Mexican American"]<-"Hispanic"
    DEMO_CON$Race_ethnicity[DEMO_CON$Race_ethnicity=="Other Hispanic"]<-"Hispanic"
    DEMO_CON$PIR_raw=DEMO_CON$INDFMPIR
    DEMO_CON$PIR[DEMO_CON$INDFMPIR<1.30]<-"<1.30"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=1.30&DEMO_CON$INDFMPIR<3]<-"1.30–2.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=3&DEMO_CON$INDFMPIR<5]<-"3.00–4.99"
    DEMO_CON$PIR[DEMO_CON$INDFMPIR>=5]<-">=5.00"
    DEMO_CON$INDFMPIR<-NULL
    Covariates_CON2_1<-DEMO_CON
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1')])
}
{#** section 10.2 health insurance  ####
    nhs_tsv('HIQ')[6:8]
    Insurance_DATA<-nhs_read(nhs_tsv('HIQ')[6:8],"HIQ011:insurance","HIQ031A:Private","HIQ031B:Medicare",
                             "HIQ031C:MediGap","HIQ031D:Medicaid","HIQ031E:SCHIP","HIQ031F:military","HIQ031H:state","HIQ031I:government",codebook = F)
    table(Insurance_DATA$insurance)
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==2]<-"No_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&(Insurance_DATA$Medicare==15|Insurance_DATA$MediGap==16|Insurance_DATA$Medicaid==17|
                                                                   Insurance_DATA$SCHIP==18|Insurance_DATA$military==19|
                                                                   Insurance_DATA$state==21|Insurance_DATA$government==22)]<-"Public_insurance"
    Insurance_DATA$Health_insurance[Insurance_DATA$insurance==1&Insurance_DATA$Private==14]<-"Private_insurance"
    
    Insurance_CON<-Insurance_DATA[,c("seqn","Health_insurance")]
    table(Insurance_DATA$Health_insurance)
    colnames(Insurance_CON)[1]<-"SEQN"
    Covariates_CON2_2<-merge(Covariates_CON2_1,Insurance_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2')])
}
{#** section 10.3 Smoking status  ####
    nhs_tsv('smq')[c(16,19,22)]
    #SMQ020 - Smoked at least 100 cigarettes in life
    #SMQ040 - Do you now smoke cigarettes
    SMQ_DATA<-nhs_read(nhs_tsv('smq')[c(16,19,22)],"SMQ020","SMQ040",Year = F,codebook = F)
    table(SMQ_DATA$SMQ040)
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==1|SMQ_DATA$SMQ040==2)]<-
      "Current_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==1&(SMQ_DATA$SMQ040==3)]<-
      "Former_smoker"
    SMQ_DATA$Smoking_status[SMQ_DATA$SMQ020==2]<-
      "Never_smoker"
    SMQ_CON<-SMQ_DATA[,c("seqn","Smoking_status")]
    colnames(SMQ_CON)[1]<-"SEQN"
    Covariates_CON2_3<-merge(Covariates_CON2_2,SMQ_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                          record=='Covariates_CON2_3')])
}
{#** section 10.4 Drinking status  ####
    nhs_tsv('alq')[c(6,8,9)]
    Drink_DATA<-nhs_read(nhs_tsv('alq')[c(6,8,9)],"ALQ110","ALQ100","ALD100:ALQ100","ALQ101:ALQ100","ALQ120Q","ALQ120U","ALQ130","ALQ140Q","ALQ141Q:ALQ140Q","ALQ140U","ALQ141U:ALQ140U","ALQ150","ALQ151:ALQ150",Year = F,codebook = F)
    #ALQ100 - Had at least 12 alcohol drinks/1 yr?
    #ALQ110 - Had at least 12 alcohol drinks/lifetime?
    #ALQ120Q - How often drink alcohol over past 12 mos
    #ALQ130 - Avg # alcoholic drinks/day -past 12 mos
    #days have 5 or more drinks/past 12 mos
    #Ever have 5 or more drinks every day?
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==777]<-NA
    Drink_DATA$ALQ120Q[Drink_DATA$ALQ120Q==999]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==77]<-NA
    Drink_DATA$ALQ130[Drink_DATA$ALQ130==99]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==777]<-NA
    Drink_DATA$ALQ140Q[Drink_DATA$ALQ140Q==999]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==7]<-NA
    Drink_DATA$ALQ150[Drink_DATA$ALQ150==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==7]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==9]<-NA
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==1]<-(365/7)
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==2]<-12
    Drink_DATA$ALQ140U[Drink_DATA$ALQ140U==3]<-1
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==7]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==9]<-NA
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==1]<-(365/7)
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==2]<-12
    Drink_DATA$ALQ120U[Drink_DATA$ALQ120U==3]<-1
    gender_DATA<-nhs_read(nhs_tsv('demo')[6:8],"RIAGENDR:Gender",Year = F,codebook = T)
    Drink_DATA<-merge(Drink_DATA,gender_DATA,by = "seqn",all.x = T)
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2|Drink_DATA$ALQ150==2]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|Drink_DATA$ALQ140Q>=3]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male")))&Drink_DATA$ALQ120Q>=1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[Drink_DATA$ALQ150==1]<-
      "Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1)&Drink_DATA$Gender=="Female")|
                                  ((Drink_DATA$ALQ130<=2)&Drink_DATA$Gender=="Male"))&Drink_DATA$ALQ120Q>=1]<-
      "Light/moderate_drinker"
    
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==2|Drink_DATA$ALQ100==2]<-"Nondrinker"
    Drink_DATA$Drink_status[Drink_DATA$ALQ110==1|Drink_DATA$ALQ100==1|(Drink_DATA$ALQ120Q*Drink_DATA$ALQ130>=12)|Drink_DATA$ALQ150==1]<-"Drinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Nondrinker"]<-"Nondrinker"
    Drink_DATA$Drinking_status[Drink_DATA$Drink_status=="Drinker"]<-"Light/moderate_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130>1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130>2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Heavier_drinker"
    Drink_DATA$Drinking_status[(((Drink_DATA$ALQ130<=1&Drink_DATA$Gender=="Female")|
                                   (Drink_DATA$ALQ130<=2&Drink_DATA$Gender=="Male"))
                                &Drink_DATA$Drink_status=="Drinker")|Drink_DATA$ALQ150==1]<-"Light/moderate_drinker"
    
    
    
    
    Drink_CON<-Drink_DATA[,c("seqn","Drinking_status")]
    colnames(Drink_CON)[1]<-"SEQN"
    Covariates_CON2_4<-merge(Covariates_CON2_3,Drink_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4')])
}
{#** section 10.5 Physical_activity  ####
    nhs_tsv('paq')[10:12]
    Physical_DATA<-nhs_read(nhs_tsv('paq')[10:12],"PAQ706","PAQ605","PAQ610",
                            "PAQ620","PAQ625","PAQ635","PAQ640","PAQ650",
                            "PAQ655","PAQ665","PAQ670",
                            Year = F,codebook = F)
    # PAD020 - Walked or bicycled over past 30 days
    # PAQ050Q - # times walked or bicycled
    # PAQ050U - Unit of measure (day/week/month)
    # PAD080 - How long per day (minutes)
    # PAQ100 - Tasks around home/yard past 30 days
    # PAD120 - # of times past 30 days
    # PAD160 - How long each time (minutes)
    # PAQ180 - Avg level of physical activity each day
    # PAD200 - Vigorous activity over past 30 days
    # PAD320 - Moderate activity over past 30 days
    # PAD440 - Muscle strengthening activities
    # PAD460 - Number of times past 30 days
    # PAQ480 - Daily hours of TV, video or computer use
    # PAQ500 - Activity comparison last mo - last yr
    # PAQ520 - Compare activity w/others same age
    # PAQ540 - Compare activity with 10 years ago
    # PAQ560 - # time/week you play or exercise hard
    # PAD570 - # of hours watch TV or videos yesterday
    # PAQ580 - # hours use computer/games yesterday
    Physical_DATA[Physical_DATA==77]<-NA
    Physical_DATA[Physical_DATA==99]<-NA
    Physical_DATA$PAQ605[Physical_DATA$PAQ605==7]<-NA
    Physical_DATA$PAQ620[Physical_DATA$PAQ620==7]<-NA
    Physical_DATA$PAQ635[Physical_DATA$PAQ635==7]<-NA
    Physical_DATA$PAQ650[Physical_DATA$PAQ650==7]<-NA
    Physical_DATA$PAQ665[Physical_DATA$PAQ665==7]<-NA
    Physical_DATA$PAQ605[Physical_DATA$PAQ605==9]<-NA
    Physical_DATA$PAQ620[Physical_DATA$PAQ620==9]<-NA
    Physical_DATA$PAQ635[Physical_DATA$PAQ635==9]<-NA
    Physical_DATA$PAQ650[Physical_DATA$PAQ650==9]<-NA
    Physical_DATA$PAQ665[Physical_DATA$PAQ665==9]<-NA
    
    
    Physical_DATA$Physical_status[Physical_DATA$PAQ605==1|Physical_DATA$PAQ620==1|
                                    Physical_DATA$PAQ635==1|Physical_DATA$PAQ650==1|Physical_DATA$PAQ665==1]<-"Insufficient"
    Physical_DATA$Physical_status[
      (Physical_DATA$PAQ605==2|is.na(Physical_DATA$PAQ605))&
        (Physical_DATA$PAQ620==2|is.na(Physical_DATA$PAQ620))&
        (Physical_DATA$PAQ635==2|is.na(Physical_DATA$PAQ635))&
        (Physical_DATA$PAQ650==2|is.na(Physical_DATA$PAQ650))&
        (Physical_DATA$PAQ665==2|is.na(Physical_DATA$PAQ665))]<-"Inactive"
    Physical_DATA$Physical_status[is.na(Physical_DATA$PAQ605)&
                                    is.na(Physical_DATA$PAQ620)&is.na(Physical_DATA$PAQ635)&
                                    is.na(Physical_DATA$PAQ650)&is.na(Physical_DATA$PAQ665)]<-NA
    Physical_DATA$PAQ610[is.na(Physical_DATA$PAQ610)]<-0
    Physical_DATA$PAQ625[is.na(Physical_DATA$PAQ625)]<-0 
    Physical_DATA$PAQ640[is.na(Physical_DATA$PAQ640)]<-0
    Physical_DATA$PAQ655[is.na(Physical_DATA$PAQ655)]<-0
    Physical_DATA$PAQ670[is.na(Physical_DATA$PAQ670)]<-0
    Physical_DATA$Physical_status[(Physical_DATA$PAQ610)+(Physical_DATA$PAQ655)>=3|
                                    (Physical_DATA$PAQ625)+(Physical_DATA$PAQ640)+(Physical_DATA$PAQ670)>=5]<-"Recommended"
    table(Physical_DATA$Physical_status,useNA = 'ifany')
    
    Physical_CON<-Physical_DATA[,c("seqn","Physical_status")]
    colnames(Physical_CON)[1]<-"SEQN"
    Covariates_CON2_5<-merge(Covariates_CON2_4,Physical_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5')])
}
{#** section 10.6 HEI  ####
    nhs_tsv("drxtot|dr1tot")[6:8]
    data_HEI<-dex_HEI(years = 2009:2014,version = "2015",method = "ssum",dietary = "tot",day = 1)
    HEI_data<-nhs_read(nhs_tsv("drxtot|dr1tot")[6:8],'WTDRD1:weight2',Year = T,codebook = F)
    CON_HEI<-merge(data_HEI[,c("seqn","hei2015_total_score")],HEI_data[,c("seqn","weight2",'Year')],by="seqn",all.x = T)
    CON_HEI$ weight =CON_HEI$ weight/2
    colnames(CON_HEI)[1]<-"SEQN"
    load("/media/lee/paper_record/paper_2_PD&DM/data_DC/CON2_baseline.Rdata")
    CON_HEI<-merge(CON_HEI,CON2_baseline[,c("SEQN","ID")],by="SEQN",all.y = T)
    Quantile<-weightedQuantile(CON_HEI$hei2015_total_score,weights = CON_HEI$weight, probs=c(0.2,0.4,0.6,0.8),  na.rm=TRUE)
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[1]]<-"Quintile 1"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[2]&CON_HEI$hei2015_total_score>Quantile[1]]<-"Quintile 2"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[3]&CON_HEI$hei2015_total_score>Quantile[2]]<-"Quintile 3"
    CON_HEI$HEI[CON_HEI$hei2015_total_score<=Quantile[4]&CON_HEI$hei2015_total_score>Quantile[3]]<-"Quintile 4"
    CON_HEI$HEI[CON_HEI$hei2015_total_score>Quantile[4]]<-"Quintile 5"
    colnames(CON_HEI)[2]<-"HEI_Score"
    HEI_CON<-CON_HEI[,c("SEQN","HEI_Score","HEI")]
    Covariates_CON2_6<-merge(Covariates_CON2_5,HEI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6')])
}
{#** section 10.7 BMI  ####
    nhs_tsv("bmx")[6:8]
    BMI_CON<-nhs_read(nhs_tsv("bmx")[6:8],'BMXBMI:BMI',Year = F,codebook = F)
    BMI_CON$BMI_Grade[BMI_CON$BMI<18.5]<-"<18.5"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=18.5&BMI_CON$BMI<25]<-"[18.5,25.0)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=25&BMI_CON$BMI<30]<-"[25.0-30)"
    BMI_CON$BMI_Grade[BMI_CON$BMI>=30]<-">=30"
    colnames(BMI_CON)[1]<-"SEQN"
    Covariates_CON2_7<-merge(Covariates_CON2_6,BMI_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7')])
}
{#** section 10.8 Hypertension  ####
    HTN_exam_CON<-diag_Hypertension(told = F,drug = F,bpx = TRUE,method = c("mean"),years = 2009:2014,join = "left")
    colnames(HTN_exam_CON)<-c("SEQN","HTN_exam_status")
    nhs_tsv("bpq")[6:8]
    HTN_self_CON<-nhs_read(nhs_tsv("bpq")[6:8],'BPQ020','BPQ030','BPQ040A',Year = F,codebook = F)
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==2]<-"NO"
    HTN_self_CON$HTN_self_status[HTN_self_CON$BPQ020==1|HTN_self_CON$BPQ030==1|HTN_self_CON$BPQ040A==1]<-"YES"
    colnames(HTN_self_CON)[1]<-"SEQN"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="no"]<-"NO"
    HTN_exam_CON$HTN_exam_status[HTN_exam_CON$HTN_exam_status=="yes"]<-"YES"
    HTN_CON<-merge(HTN_exam_CON,HTN_self_CON,by = "SEQN",all = T)
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="NO"|HTN_CON$HTN_self_status=="NO"]<-"NO"
    HTN_CON$HTN_status[HTN_CON$HTN_exam_status=="YES"|HTN_CON$HTN_self_status=="YES"]<-"YES"
    HTN_CON<-HTN_CON[,c("SEQN","HTN_status")]
    table(HTN_CON$HTN_status)
    Covariates_CON2_8<-merge(Covariates_CON2_7,HTN_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8')])
}
{#** section 10.9 hyperlipoidemia  ####
    nhs_tsv("TCHOL")[3:5]
    HPL_exam_CON<-HTN_self_CON<-nhs_read(nhs_tsv("TCHOL")[3:5],'LBXTC',Year = F,codebook = F)
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC>=200]<-"YES"
    HPL_exam_CON$HPL_exam_status[HPL_exam_CON$LBXTC<200]<-"NO"
    table(HPL_exam_CON$HPL_exam_status)
    colnames(HPL_exam_CON)[1]<-"SEQN"
    nhs_tsv("bpq")[6:8]
    HPL_self_CON<-nhs_read(nhs_tsv("bpq")[6:8],'BPQ080','BPQ090D',Year = F,codebook = F)
    HPL_self_CON$HPL_self_status<-NULL
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==2]<-"NO"
    HPL_self_CON$HPL_self_status[HPL_self_CON$BPQ080==1|HPL_self_CON$BPQ090D==1]<-"YES"
    colnames(HPL_self_CON)[1]<-"SEQN"
    table(HPL_self_CON$HPL_self_status)
    HPL_CON<-merge(HPL_exam_CON,HPL_self_CON,by = "SEQN",all = T)
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="NO"|HPL_CON$HPL_self_status=="NO"]<-"NO"
    HPL_CON$HPL_status[HPL_CON$HPL_exam_status=="YES"|HPL_CON$HPL_self_status=="YES"]<-"YES"
    table(HPL_CON$HPL_status)
    HPL_CON<-HPL_CON[,c("SEQN","HPL_status")]
    Covariates_CON2_9<-merge(Covariates_CON2_8,HPL_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9')])
    
}
{#** section 10.10 CVD ####
    #CVD
    CVD_CON<-diag_CVD(years = 2009:2014,join = "left")
    table(CVD_CON$CVD)
    CVD_CON$CVD[CVD_CON$CVD=="no"]<-"NO"
    CVD_CON$CVD[CVD_CON$CVD=="yes"]<-"YES"
    colnames(CVD_CON)[1]<-'SEQN'
    Covariates_CON2_10<-merge(Covariates_CON2_9,CVD_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10')])
}
{#** section 10.11 CKD ####
    {#*** section 10.11.1 eGFR ####
      years <- 2009:2014
      (demo <- nhs_tsv("demo", items = "demo", years = years, 
                       cat = FALSE))
      (bm <- nhs_tsv("bmx", items = "exam", years = years, cat = FALSE))
      
      (biopro <- nhs_tsv("lab18\\.|l40_b\\.|l40_c\\.|biopro", 
                         items = "lab", years = years, cat = FALSE))
      data0 <- nhs_read(demo, "ridageyr:age", "riagendr:sex", 
                        "ridreth1:eth1", "ridreth2:eth2", "ridreth3:eth3", bm, 
                        "bmxwt:weight", "bmxht:height", biopro, "lbxscr,lbdscr:scr", 
                        "lbxsal:alb", "lbxsbu:bun", lower_cd = TRUE, 
                        cat = FALSE, Year = TRUE)
      data0$eth1 <- as.numeric(Recode(data0$eth1, "non-hispanic black::1", 
                                      "non-hispanic white::0", "other race - including multi-racial::0", 
                                      "mexican american::0", "other hispanic::0"))
      table(data0$eth3)
      data0$eth3 <- as.numeric(Recode(data0$eth3, "non-hispanic black::1", 
                                      "non-hispanic white::0", "other race - including multi-racial::0", 
                                      "mexican american::0", "other hispanic::0","non-hispanic asian::0"))
      data0$black <- ifelse(row.sums(data0[, c("eth1", "eth3")]) > 0, "black", "no")
      a <- rep(NA, length(data0$black))
      a[data0$black == "black" & data0$sex == "female"] <- 166
      a[data0$black == "black" & data0$sex == "male"] <- 163
      a[data0$black != "black" & data0$sex == "female"] <- 144
      a[data0$black != "black" & data0$sex == "male"] <- 141
      b <- ifelse(data0$sex == "female", 0.7, 0.9)
      c <- rep(NA, length(data0$black))
      c[data0$sex == "female" & data0$scr <= 0.7] <- -0.329
      c[data0$sex == "female" & data0$scr > 0.7] <- -1.209
      c[data0$sex == "male" & data0$scr <= 0.9] <- -0.411
      c[data0$sex == "male" & data0$scr > 0.9] <- -1.209
      data0$eGFR <- a * ((data0$scr/b)^c) * (0.993^data0$age)
      var <- c("seqn", "eGFR")
      eGFR_CON <- data0[, var]
    }
    {#*** section 10.11.2 uACR ####
      tsv <- nhs_tsv("alb_cr", years = years, cat = F)
      uACR_CON <-  nhs_read(tsv, "urxucr:cr", "urxums:alb", cat = F)
      uACR_CON$uACR <- uACR_CON$alb/uACR_CON$cr * 100
      uACR_CON <- uACR_CON[, c("seqn","uACR")]
    }
    {#*** section 10.11.3 Conbine ####
      CKD_dia_CON<-merge(eGFR_CON,uACR_CON,by="seqn",all=T)
      CKD_dia_CON$CKD[CKD_dia_CON$eGFR>=60|CKD_dia_CON$uACR<=30]<-"NO"
      CKD_dia_CON$CKD[CKD_dia_CON$eGFR<60|CKD_dia_CON$uACR>30]<-"YES"
      CKD_CON<-CKD_dia_CON[,c("seqn","CKD")]
      colnames(CKD_CON)[1]<-'SEQN'
      Covariates_CON2_11<-merge(Covariates_CON2_10,CKD_CON,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                              record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                              record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                              record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                              record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                              record=='Covariates_CON2_11')])
      
    }
}
{#** section 10.12 Caner ####
    tsv<-nhs_tsv('mcq',years = 2009:2014)
    Cancer_CON<-nhs_read(tsv,'mcq220:Cancer',Year = F)
    Cancer_CON$Cancer[Cancer_CON$Cancer=="Yes"]<-"YES"
    Cancer_CON$Cancer[Cancer_CON$Cancer=="No"]<-"NO"
    colnames(Cancer_CON)[1]<-'SEQN'
    Covariates_CON2_12<-merge(Covariates_CON2_11,Cancer_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                            record=='Covariates_CON2_11'|record=='Covariates_CON2_12')])
}
{#** section 10.13 HbA1c  ####
    nhs_tsv("ghb_")[c(3:5)]
    HbA1c_CON<-nhs_read(nhs_tsv("ghb_")[c(3:5)],'lbxgh',Year = F,codebook = F)
    colnames(HbA1c_CON)<-c("SEQN","HbA1c")
    Covariates_CON2_13<-merge(Covariates_CON2_12,HbA1c_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                            record=='Covariates_CON2_11'|record=='Covariates_CON2_12'|
                            record=='Covariates_CON2_13')])
}
{#** section 10.14 Blood relatives have diabetes  ####
    nhs_tsv("mcq")[6:8]
    Br_diabetes_CON<-nhs_read(nhs_tsv("mcq")[6:8],'mcq300c',Year = F,codebook = F)
    Br_diabetes_CON$mcq300c[Br_diabetes_CON$mcq300c==1]<-"YES"
    Br_diabetes_CON$mcq300c[Br_diabetes_CON$mcq300c==2]<-"NO"
    Br_diabetes_CON$mcq300c[Br_diabetes_CON$mcq300c==7]<-NA
    Br_diabetes_CON$mcq300c[Br_diabetes_CON$mcq300c==9]<-NA
    colnames(Br_diabetes_CON)<-c("SEQN","Br_diabetes_status")
    Covariates_CON2_14<-merge(Covariates_CON2_13,Br_diabetes_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                            record=='Covariates_CON2_11'|record=='Covariates_CON2_12'|
                            record=='Covariates_CON2_13'|record=='Covariates_CON2_14')])
  }
{#** section 10.15 Time since diabetes diagnosis  ####
    {#*** section 10.15.1 Time since diabetes diagnosis-DIQ  ####
      #9900
      nhs_tsv("DEMO")[6:8]
      duration_year_CON<-nhs_read(nhs_tsv("DIQ")[6:8],"DID040:DIQ040",nhs_tsv("DEMO")[6:8],"RIDAGEYR:Age",Year = F,codebook = F)
      duration_year_CON$DIQ040[duration_year_CON$DIQ040==80]<-NA
      duration_year_CON$DIQ040[duration_year_CON$DIQ040==666]<-NA
      duration_year_CON$DIQ040[duration_year_CON$DIQ040==777]<-NA
      duration_year_CON$DIQ040[duration_year_CON$DIQ040==999]<-NA
      duration_year_CON$duration<-(duration_year_CON$Age-duration_year_CON$DIQ040)
    }
    {#*** section 10.15.2 Time since diabetes diagnosis-all  ####
      
      duration_CON<-duration_year_CON[,c("seqn","duration")]
      duration_CON$duration[is.na(duration_CON$duration)]<-0.5
      duration_CON$duration[duration_CON$duration==0]<-0.5
      colnames(duration_CON)[1]<-'SEQN'
      duration_CON<-duration_CON[,c("SEQN","duration")]
      Covariates_CON2_15<-merge(Covariates_CON2_14,duration_CON,by="SEQN",all.x = T)
      record<-ls()
      rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                              record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                              record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                              record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                              record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                              record=='Covariates_CON2_11'|record=='Covariates_CON2_12'|
                              record=='Covariates_CON2_13'|record=='Covariates_CON2_14'|
                              record=='Covariates_CON2_15')])
    }
}
{#** section 10.16 diabetes medicine  ####
    medicine_CON<-nhs_read(nhs_tsv("DIQ")[6:8],"DIQ050:insuli","DIQ070:pills",Year = F,codebook = F)
    medicine_CON$medicine<-"NO"
    medicine_CON$medicine[medicine_CON$insuli==1|medicine_CON$pills==1]<-"YES"
    medicine_CON<-medicine_CON[,c("seqn","medicine")]
    colnames(medicine_CON)[1]<-"SEQN"
    Covariates_CON2_16<-merge(Covariates_CON2_15,medicine_CON,by="SEQN",all.x = T)
    record<-ls()
    rm(list=record[-which(record=='Covariates_CON2_1'|record=='Covariates_CON2_2'|
                            record=='Covariates_CON2_3'|record=='Covariates_CON2_4'|
                            record=='Covariates_CON2_5'|record=='Covariates_CON2_6'|
                            record=='Covariates_CON2_7'|record=='Covariates_CON2_8'|
                            record=='Covariates_CON2_9'|record=='Covariates_CON2_10'|
                            record=='Covariates_CON2_11'|record=='Covariates_CON2_12'|
                            record=='Covariates_CON2_13'|record=='Covariates_CON2_14'|
                            record=='Covariates_CON2_15'|record=='Covariates_CON2_16')])
}
{#** section 10.17 occupation  ####
  occupation_CON<-HTN_self_CON<-nhs_read(nhs_tsv("ocq")[6:8],'OCD241:job','OCQ380:status',Year = F,codebook = F)
  table(occupation_CON$job)
  occupation_CON$status[occupation_CON$status==1]<-"Keeping house"
  occupation_CON$status[occupation_CON$status==2]<-"Going to school"
  occupation_CON$status[occupation_CON$status==3]<-"Retired"
  occupation_CON$status[occupation_CON$status==4]<-"Disabled"
  occupation_CON$status[occupation_CON$status==5]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==6]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==7]<-"Unemployment"
  occupation_CON$status[occupation_CON$status==77]<-NA
  occupation_CON$status[occupation_CON$status==99]<-NA
  occupation_CON$job_status[occupation_CON$status=="Retired"]<-"Retired"
  occupation_CON$job_status[occupation_CON$status=="Keeping house"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Going to school"]<-"Going to school"
  occupation_CON$job_status[occupation_CON$status=="Something else"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Disabled"]<-"Unemployment"
  occupation_CON$job_status[occupation_CON$status=="Unemployment"]<-"Unemployment"
  
  
  occupation_CON$job_status[occupation_CON$job==1]<-"Management Occupations"
  occupation_CON$job_status[occupation_CON$job==2]<-"Business, Financial Operations Occupations"
  occupation_CON$job_status[occupation_CON$job==3]<-"Computer, Mathematical Occupations"
  occupation_CON$job_status[occupation_CON$job==4]<-"Architecture, Engineering Occupations"
  occupation_CON$job_status[occupation_CON$job==5]<-"Life, Physical, Social Science Occupations"
  occupation_CON$job_status[occupation_CON$job==6]<-"Community, Social Services Occupations"
  occupation_CON$job_status[occupation_CON$job==7]<-"Legal Occupations"
  occupation_CON$job_status[occupation_CON$job==8]<-"Education, Training, Library Occupations"
  occupation_CON$job_status[occupation_CON$job==9]<-"Arts, Design, Entertainment, Sports, Media Occupations"
  occupation_CON$job_status[occupation_CON$job==10]<-"Healthcare Practitioner, Technical Occupations"
  occupation_CON$job_status[occupation_CON$job==11]<-"Healthcare Support Occupations"
  occupation_CON$job_status[occupation_CON$job==12]<-"Protective Service Occupations"
  occupation_CON$job_status[occupation_CON$job==13]<-"Food Preparation, Serving Occupations"
  occupation_CON$job_status[occupation_CON$job==14]<-"Building & Grounds Cleaning, Maintenance Occupations"
  occupation_CON$job_status[occupation_CON$job==15]<-"Personal Care, Service Occupations"
  occupation_CON$job_status[occupation_CON$job==16]<-"Sales & Related Occupations"
  occupation_CON$job_status[occupation_CON$job==17]<-"Office, Administrative Support Occupations"
  occupation_CON$job_status[occupation_CON$job==18]<-"Farming, Fishing, Forestry Occupations"
  occupation_CON$job_status[occupation_CON$job==19]<-"Construction, Extraction Occupations"
  occupation_CON$job_status[occupation_CON$job==20]<-"Installation, Maintenance, Repair Occupations"
  occupation_CON$job_status[occupation_CON$job==21]<-"Production Occupations"
  occupation_CON$job_status[occupation_CON$job==22]<-"Transportation, Material Moving Occupations"
  occupation_CON$job_status[occupation_CON$job==23]<-"Armed Forces"
  
  
  occupation_CON$SEI[occupation_CON$job_status=="Management Occupations"|
                       occupation_CON$job_status=="Business, Financial Operations Occupations"|  
                       occupation_CON$job_status=="Computer, Mathematical Occupations"|
                       occupation_CON$job_status=="Architecture, Engineering Occupations"|
                       occupation_CON$job_status=="Life, Physical, Social Science Occupations"|
                       occupation_CON$job_status=="Community, Social Services Occupations"|
                       occupation_CON$job_status=="Legal Occupations"|
                       occupation_CON$job_status=="Education, Training, Library Occupations"|
                       occupation_CON$job_status=="Arts, Design, Entertainment, Sports, Media Occupations"|
                       occupation_CON$job_status=="Healthcare Practitioner, Technical Occupations"|
                       occupation_CON$job_status=="Office, Administrative Support Occupations"|
                       occupation_CON$job_status=="Armed Forces"
  ]<-"Upper"
  occupation_CON$SEI[occupation_CON$job_status=="Healthcare Support Occupations"|
                       occupation_CON$job_status=="Protective Service Occupations"|
                       occupation_CON$job_status=="Food Preparation, Serving Occupations"|
                       occupation_CON$job_status=="Building & Grounds Cleaning, Maintenance Occupations"|
                       occupation_CON$job_status=="Personal Care, Service Occupations"|
                       occupation_CON$job_status=="Farming, Fishing, Forestry Occupations"|
                       occupation_CON$job_status=="Construction, Extraction Occupations"|
                       occupation_CON$job_status=="Installation, Maintenance, Repair Occupations"|
                       occupation_CON$job_status=="Production Occupations"|
                       occupation_CON$job_status=="Transportation, Material Moving Occupations"|
                       occupation_CON$job_status=="Sales & Related Occupations"
  ]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Retired"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Going to school"]<-"Lower"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON$SEI[occupation_CON$job_status=="Unemployment"]<-"Unemployment"
  occupation_CON<-occupation_CON[,c("seqn","SEI")]
  colnames(occupation_CON)[1]<-"SEQN"
  Covariates_CON2_17<-merge(Covariates_CON2_16,occupation_CON,by="SEQN",all.x = T)
  record<-ls()
  rm(list=record[-which(record=='Covariates_CON1_1'|record=='Covariates_CON1_2'|
                          record=='Covariates_CON1_3'|record=='Covariates_CON1_4'|
                          record=='Covariates_CON1_5'|record=='Covariates_CON1_6'|
                          record=='Covariates_CON1_7'|record=='Covariates_CON1_8'|
                          record=='Covariates_CON1_9'|record=='Covariates_CON1_10'|
                          record=='Covariates_CON1_11'|record=='Covariates_CON1_12'|
                          record=='Covariates_CON1_13'|record=='Covariates_CON1_14'|
                          record=='Covariates_CON1_15'|record=='Covariates_CON1_16'|
                          record=='Covariates_CON1_17')])
}



Covariates_CON2<-Covariates_CON2_17
record<-ls()
rm(list=record[-which(record=='Covariates_CON2')])
save(Covariates_CON2,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Covariates_CON2.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 11. New Covariates <<<<< ####
{#* section 11.1 white blood counts  ####
  #NHANES III
  lab=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
  lab$SEQN<-as.numeric(substring(lab$V1,1,5))
  WBC_III<-as.data.frame(lab$SEQN)
  colnames(WBC_III)<-c("SEQN")
  WBC_III$WBC<-as.numeric(substring(lab$V1,1273,1277))
  WBC_III$WBC[WBC_III$WBC==88888]<-NA
  save(WBC_III,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_III.Rdata")
  #NHANES 1999-2004
  WBC_CON1<-nhs_read(nhs_tsv("lab25|l25")[c(1:3)],"LBXWBCSI:WBC",Year = F,codebook = F)
  colnames(WBC_CON1)[1]<-"SEQN"
  save(WBC_CON1,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_CON1.Rdata")
  #NHANES 2009-2014
  WBC_CON2<-nhs_read(nhs_tsv("CBC")[c(3:5)],"LBXWBCSI:WBC",Year = F,codebook = F)
  colnames(WBC_CON2)[1]<-"SEQN"
  save(WBC_CON2,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_CON2.Rdata")
  record<-ls()
  rm(list=record)
}
{#* section 11.2 CRP  ####
  #NHANES III
  lab=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
  lab$SEQN<-as.numeric(substring(lab$V1,1,5))
  CRP_III<-as.data.frame(lab$SEQN)
  colnames(CRP_III)<-c("SEQN")
  CRP_III$CRP<-as.numeric(substring(lab$V1,1667,1671))
  CRP_III$CRP[CRP_III$CRP==0.21]<-0.20
  CRP_III$CRP[CRP_III$CRP==88888]<-NA
  #NHANES 1999-2004
  CRP_CON1<-nhs_read(nhs_tsv("lab11|l11_")[c(1,3,4)],"lbxcrp:CRP",Year = F,codebook = F)
  CRP_CON1$CRP[CRP_CON1$CRP<=0.20]<-0.20
  colnames(CRP_CON1)[1]<-c("SEQN")
  #NHANES 2009-2014
  CRP_CON2<-nhs_read(nhs_tsv("crp_f"),"lbxcrp:CRP",Year = F,codebook = F)
  CRP_CON2$CRP[CRP_CON2$CRP<=0.20]<-0.20
  colnames(CRP_CON2)[1]<-c("SEQN")
  CRP_III$ID<-paste0("NHANES_III_",CRP_III$SEQN)
  CRP_CON1$ID<-paste0("NHANES_CON1_",CRP_CON1$SEQN)
  CRP_CON2$ID<-paste0("NHANES_CON2_",CRP_CON2$SEQN)
  CRP<-rbind(CRP_III,CRP_CON1,CRP_CON2)
  save(CRP,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/CRP.Rdata")
}
{#* section 11.3 fibrinogen  ####
  #NHANES III
  lab=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
  lab$SEQN<-as.numeric(substring(lab$V1,1,5))
  Fb_III<-as.data.frame(lab$SEQN)
  colnames(Fb_III)<-c("SEQN")
  Fb_III$Fb<-as.numeric(substring(lab$V1,1659,1662))
  Fb_III$Fb[Fb_III$CRP==8888]<-NA
  #NHANES 1999-2004
  Fb_CON1<-nhs_read(nhs_tsv("lab11|l11_")[c(1,3)],"lbxfb:Fb",Year = F,codebook = F)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 12. Weight <<<<< ####
{#*** section 12.1 NHANES III  ####
  exam=read.table("/media/lee/paper_record/paper_2_PD&DM/data/NHANESIII/exam.dat",sep=",",fill=T)
  exam$SEQN<-as.numeric(substring(exam$V1,1,5))
  Weight_data<-as.data.frame(exam$SEQN)
  colnames(Weight_data)<-c("SEQN")
  Weight_data$weight<-as.numeric(substring(exam$V1,59,67))
  Weight_data$sdmvpsu<-as.numeric(substring(exam$V1,41,41))
  Weight_data$sdmvstra<-as.numeric(substring(exam$V1,42,43))
  Weight_data$weight=Weight_data$weight/3
  sum(Weight_data$weight)
  Weight_data$sdmvstra<-Weight_data$sdmvstra+300
  Weight_III<-Weight_data[,c("SEQN","sdmvpsu","sdmvstra","weight")]
  save(Weight_III,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_III.Rdata")
}
{#*** section 12.2 NHANES 1999-2004  ####
  Weight_data<-nhs_read(nhs_tsv("demo")[1:3],'WTMEC2YR:weight2','WTMEC4YR:weight4',Year = T,codebook = F)
  Weight_data <- Weight_data %>% mutate(weight = ifelse(Year=="1999-2000"|Year=="2001-2002",weight4*2/3,weight2/3))
  colnames(Weight_data)[2]<-"SEQN"
  Weight_data $weight2<-NULL
  Weight_data $weight4<-NULL
  Weight_CON1<-Weight_data[,c("SEQN","sdmvpsu","sdmvstra","weight")]
  save(Weight_CON1,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_CON1.Rdata")
}
{#*** section 12.3 NHANES 2009-2014  ####
  load(file="/media/lee/paper_record/paper_2_PD&DM/data/covariates/CON_covariates_09.Rdata")
  Weight_data<-nhs_read(nhs_tsv("demo")[6:8],'WTMEC2YR:weight2',Year = T,codebook = F)
  library(dplyr)
  Weight_data $weight <- Weight_data $weight2/3
  Weight_data $weight2<-NULL
  colnames(Weight_data)[2]<-"SEQN"
  Weight_CON2<-Weight_data[,c("SEQN","sdmvpsu","sdmvstra","weight")]
  save(Weight_CON2,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_CON2.Rdata")
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 13. Combine all data <<<<< ####
{#* section 13.1 NHANES III  ####
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/III_baseline.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Covariates_III.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_III.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_III.Rdata")
  multimerge<-function(dat=list(),...){
    if(length(dat)<2)return(as.data.frame(dat))
    mergedat<-dat[[1]]
    dat[[1]]<-NULL
    for(i in dat){
      mergedat<-merge(mergedat,i,...)
    }
    return(mergedat)
  }
  III_alldata<-multimerge(list(III_baseline,Covariates_III,WBC_III,Weight_III),by="SEQN",all.x = T)
}
{#* section 13.2 NHANES 1999-2004  ####
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/CON1_baseline.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Covariates_CON1.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_CON1.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_CON1.Rdata")
  multimerge<-function(dat=list(),...){
    if(length(dat)<2)return(as.data.frame(dat))
    mergedat<-dat[[1]]
    dat[[1]]<-NULL
    for(i in dat){
      mergedat<-merge(mergedat,i,...)
    }
    return(mergedat)
  }
  CON1_alldata<-multimerge(list(CON1_baseline,Covariates_CON1,WBC_CON1,Weight_CON1),by="SEQN",all.x = T)
}
{#* section 13.3 NHANES 2009-2014  ####
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/CON2_baseline.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Covariates_CON2.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/WBC_CON2.Rdata")
  load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Weight_CON2.Rdata")
  multimerge<-function(dat=list(),...){
    if(length(dat)<2)return(as.data.frame(dat))
    mergedat<-dat[[1]]
    dat[[1]]<-NULL
    for(i in dat){
      mergedat<-merge(mergedat,i,...)
    }
    return(mergedat)
  }
  CON2_alldata<-multimerge(list(CON2_baseline,Covariates_CON2,WBC_CON2,Weight_CON2),by="SEQN",all.x = T)
}
{#* section 13.4 merge all data ####
  All_data<-rbind(III_alldata,CON1_alldata,CON2_alldata)
  All_data$sdmvstra<-as.factor(All_data$sdmvstra)
  All_data$sdmvstra<-as.numeric(All_data$sdmvstra)
  record<-ls()
  rm(list=record[-which(record=='All_data')])
  save(All_data,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/All_data.Rdata")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 14. Data clearn <<<<< ####

load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/All_data.Rdata")
table(All_data$T2D)
All_data$T2D<-NULL
table(All_data$PD_diagnosis)
All_data$PD_diagnosis<-factor(All_data$PD_diagnosis,
                              levels = c("No/Mild periodontitis","Moderate/Severe periodontitis"))
table(All_data$PD_diagnosis)
table(All_data$Periodontitis_diagnosis)
All_data$Periodontitis_diagnosis<-factor(All_data$Periodontitis_diagnosis,
                              levels = c("normal","mild","moderate","severe"))
table(All_data$Periodontitis_diagnosis)
table(All_data$Pregnancy)
All_data$Pregnancy<-NULL
table(All_data$MORT_stat)
All_data$MORT_stat<-factor(All_data$MORT_stat,
                                         levels = c("Alive","Deceased"))
table(All_data$ucod_leading)
table(All_data$diabetes)
summary(All_data$permth)
All_data$peryear<-All_data$permth/12
summary(All_data$DMFT)
table(All_data$Year)
summary(All_data$Age)
All_data$Age[All_data$Age>=80]<-80
All_data$Age_status[All_data$Age<45]<-"<45"
All_data$Age_status[All_data$Age>=45&All_data$Age<65]<-"[45,65)"
All_data$Age_status[All_data$Age>=65]<-">=65"
All_data$Age_status<-factor(All_data$Age_status,
                        levels = c("<45","[45,65)",">=65"))
table(All_data$Age_status)
table(All_data$Gender)
All_data$Gender<-factor(All_data$Gender,
                                         levels = c("Male","Female"))
table(All_data$Gender)
table(All_data$Race_ethnicity)
All_data$Race_ethnicity[All_data$Race_ethnicity=='Non-Hispanic black']<-
  'Non-Hispanic Black'
All_data$Race_ethnicity[All_data$Race_ethnicity=='Non-Hispanic white']<-
  'Non-Hispanic White'
All_data$Race_ethnicity<-factor(All_data$Race_ethnicity,
                        levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other_Race"))
table(All_data$Race_ethnicity)
table(All_data$Education_levels)
All_data$Education_levels<-factor(All_data$Education_levels,
                                levels = c("Less_than_high_school","High_school_or_Equivalent","College_or_above"))
table(All_data$Education_levels)
summary(All_data$PIR_raw)
table(All_data$PIR)
All_data$PIR[All_data$PIR_raw<=1]<-
  '(0, 1]'
All_data$PIR[All_data$PIR_raw>1&All_data$PIR_raw<4]<-
  '(1,4)'
All_data$PIR[All_data$PIR_raw>=4]<-
  '[4,inf)'
All_data$PIR<-factor(All_data$PIR,
                                  levels = c("(0, 1]","(1,4)","[4,inf)"))
table(All_data$PIR,useNA = "ifany")
table(All_data$Health_insurance)
All_data$Health_insurance<-factor(All_data$Health_insurance,
                     levels = c("No_insurance","Public_insurance","Private_insurance"))
table(All_data$Health_insurance)
table(All_data$Smoking_status)
All_data$Smoking_status<-factor(All_data$Smoking_status,
                                  levels = c("Never_smoker","Former_smoker","Current_smoker"))
table(All_data$Smoking_status)
table(All_data$Drinking_status)
All_data$Drinking_status<-factor(All_data$Drinking_status,
                                levels = c("Nondrinker","Light/moderate_drinker","Heavier_drinker"))
table(All_data$Drinking_status)
table(All_data$Physical_status)

summary(All_data$HEI_Score)
All_data$HEI_Score<-NULL
table(All_data$HEI)
summary(All_data$BMI)
table(All_data$BMI_Grade)
All_data$BMI_Grade[All_data$BMI_Grade=='<18.5']<-'<25'
All_data$BMI_Grade[All_data$BMI_Grade=='[18.5,25.0)']<-'<25'
All_data$BMI_Grade<-factor(All_data$BMI_Grade,
                                 levels = c("<25","[25.0-30)",">=30"))
table(All_data$BMI_Grade)

table(All_data$HTN_status)
table(All_data$HPL_status)
table(All_data$CVD)
table(All_data$CKD)
table(All_data$Cancer)
summary(All_data$HbA1c)
All_data$HbA1c_status[All_data$HbA1c>=7]<-"HbA1c>=7"
All_data$HbA1c_status[All_data$HbA1c<7]<-"HbA1c<7"
All_data$HbA1c_status<-factor(All_data$HbA1c_status,levels = c("HbA1c<7","HbA1c>=7"))
table(All_data$HbA1c_status)
table(All_data$Br_diabetes_status)
summary(All_data$duration)
All_data$duration_status[All_data$duration<3]<-"duration<3"
All_data$duration_status[All_data$duration>=3]<-"duration>=3"
All_data$duration<-NULL
All_data$duration_status<-factor(All_data$duration_status,levels = c("duration<3","duration>=3"))
table(All_data$duration_status)
table(All_data$medicine)
table(All_data$SEI)
All_data$SEI<-factor(All_data$SEI,levels = c("Unemployment","Lower","Upper"))
table(All_data$SEI)
summary(All_data$WBC)
colnames(All_data)
All_data$CVD_status<-All_data$CVD
All_data$CKD_status<-All_data$CKD
All_data$Cancer_status<-All_data$Cancer
All_data$Medicine_use<-All_data$medicine
Clearn_data<-All_data[,c("ID","PD_diagnosis","Periodontitis_diagnosis","CAL_mean","PPD_mean","DMFT",
                         "MORT_stat","ucod_leading","peryear",
                         "Age","BMI","HbA1c","WBC",
                         "Age_status","Gender","Race_ethnicity","Education_levels","PIR","Health_insurance","SEI",
                         "Smoking_status","Drinking_status","Physical_status","HEI",
                         "BMI_Grade","CVD_status","Cancer_status","HTN_status","HPL_status",
                         "HbA1c_status","Br_diabetes_status","Medicine_use","duration_status",
                         "sdmvpsu","sdmvstra","weight")]
colnames<-c("Physical_status","HEI","CVD_status","Cancer_status","HTN_status","HPL_status","Br_diabetes_status","Medicine_use","sdmvpsu","sdmvstra")




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 15. Data conversion <<<<< ####
colApply <- function(dat, cols = colnames, func = as.factor) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
Clearn_data<-colApply(Clearn_data,colnames, as.factor)
colnames(Clearn_data)
#as.factor
colApply <- function(dat, cols = colnames, func = as.factor) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
colname<-colnames(Clearn_data)[14:33]
Characters<-colApply(Clearn_data[,c(1,10:34)],colname, as.factor)
#as.numeric
rownames(Characters)<-Characters$ID
Characters$ID<-NULL
Characters$Age_status<-NULL
Characters$BMI_Grade<-NULL
Characters$HbA1c_status<-NULL
colApply <- function(dat, cols = colnames, func = as.numeric) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
#as.factor
Characters<-colApply(Characters,colnames(Characters), as.numeric)

colApply <- function(dat, cols = colnames, func = as.factor) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
colnames(Characters)
colname<-colnames(Characters)[-c(1:4,22)]
colname
Characters<-colApply(Characters,colname, as.factor)
Clearn_data_new<-Clearn_data
Clearn_data<-Clearn_data_old<-Clearn_data[,-c(28,29)]
save(Clearn_data,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
save(Clearn_data_new,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Clearn_data_new.Rdata")
save(Characters,file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Characters.Rdata")
load(file="/media/lee/paper_record/paper_2_PD&DM/data_DC/Clearn_data.Rdata")
table(Clearn_data$Gender)
table(Characters$Gender)

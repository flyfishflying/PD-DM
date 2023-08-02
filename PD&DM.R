# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#>>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0.1 Packages ####
  library(foreign)
  library(dplyr)
  library(nhanesR)
  library(tidyverse)
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
  {#** section 0.2.1 Batch conversion to numeric ####
    colApply <- function(dat, cols = colnames(dat), func = as.numeric) {
      dat[cols] <- lapply(dat[cols], func)
      return(dat)
    }
  }
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 1. Periodontitis diagnosis <<<<< ####
{#* section 1.1 NHANES III Periodontitis diagnosis ####
  {#** section 1.1.1 Data Preparation ####
    PD_dia=read.table("G:/paper_2_PD&DM/data/NHANESIII/exam.dat",sep=",",fill=T)
    PD_dia$SEQN<-substring(PD_dia$V1,1,5)
    #PPD
    PD_dia$PPD_01_M<-as.numeric(substring(PD_dia$V1,2445,2446))
    PD_dia$PPD_01_B<-as.numeric(substring(PD_dia$V1,2449,2450))
    PD_dia$PPD_02_M<-as.numeric(substring(PD_dia$V1,2453,2454))
    PD_dia$PPD_02_B<-as.numeric(substring(PD_dia$V1,2457,2458))
    PD_dia$PPD_03_M<-as.numeric(substring(PD_dia$V1,2462,2463))
    PD_dia$PPD_03_B<-as.numeric(substring(PD_dia$V1,2467,2468))
    PD_dia$PPD_04_M<-as.numeric(substring(PD_dia$V1,2471,2472))
    PD_dia$PPD_04_B<-as.numeric(substring(PD_dia$V1,2475,2476))
    PD_dia$PPD_05_M<-as.numeric(substring(PD_dia$V1,2479,2480))
    PD_dia$PPD_05_B<-as.numeric(substring(PD_dia$V1,2483,2484))
    PD_dia$PPD_06_M<-as.numeric(substring(PD_dia$V1,2488,2489))
    PD_dia$PPD_06_B<-as.numeric(substring(PD_dia$V1,2493,2494))
    PD_dia$PPD_07_M<-as.numeric(substring(PD_dia$V1,2498,2499))
    PD_dia$PPD_07_B<-as.numeric(substring(PD_dia$V1,2503,2504))
    
    PD_dia$PPD_11_M<-as.numeric(substring(PD_dia$V1,2507,2508))
    PD_dia$PPD_11_B<-as.numeric(substring(PD_dia$V1,2512,2513))
    PD_dia$PPD_12_M<-as.numeric(substring(PD_dia$V1,2516,2517))
    PD_dia$PPD_12_B<-as.numeric(substring(PD_dia$V1,2521,2522))
    PD_dia$PPD_13_M<-as.numeric(substring(PD_dia$V1,2526,2527))
    PD_dia$PPD_13_B<-as.numeric(substring(PD_dia$V1,2531,2532))
    PD_dia$PPD_14_M<-as.numeric(substring(PD_dia$V1,2536,2537))
    PD_dia$PPD_14_B<-as.numeric(substring(PD_dia$V1,2541,2542))
    PD_dia$PPD_15_M<-as.numeric(substring(PD_dia$V1,2545,2546))
    PD_dia$PPD_15_B<-as.numeric(substring(PD_dia$V1,2549,2550))
    PD_dia$PPD_16_M<-as.numeric(substring(PD_dia$V1,2554,2555))
    PD_dia$PPD_16_B<-as.numeric(substring(PD_dia$V1,2559,2560))
    PD_dia$PPD_17_M<-as.numeric(substring(PD_dia$V1,2564,2565))
    PD_dia$PPD_17_B<-as.numeric(substring(PD_dia$V1,2569,2570))
    
    #CAL
    PD_dia$CAL_01_M<-as.numeric(substring(PD_dia$V1,2571,2572))
    PD_dia$CAL_01_B<-as.numeric(substring(PD_dia$V1,2573,2574))
    PD_dia$CAL_02_M<-as.numeric(substring(PD_dia$V1,2575,2576))
    PD_dia$CAL_02_B<-as.numeric(substring(PD_dia$V1,2577,2578))
    PD_dia$CAL_03_M<-as.numeric(substring(PD_dia$V1,2579,2580))
    PD_dia$CAL_03_B<-as.numeric(substring(PD_dia$V1,2581,2582))
    PD_dia$CAL_04_M<-as.numeric(substring(PD_dia$V1,2583,2584))
    PD_dia$CAL_04_B<-as.numeric(substring(PD_dia$V1,2585,2586))
    PD_dia$CAL_05_M<-as.numeric(substring(PD_dia$V1,2587,2588))
    PD_dia$CAL_05_B<-as.numeric(substring(PD_dia$V1,2589,2590))
    PD_dia$CAL_06_M<-as.numeric(substring(PD_dia$V1,2591,2592))
    PD_dia$CAL_06_B<-as.numeric(substring(PD_dia$V1,2593,2594))
    PD_dia$CAL_07_M<-as.numeric(substring(PD_dia$V1,2595,2596))
    PD_dia$CAL_07_B<-as.numeric(substring(PD_dia$V1,2597,2598))
    
    PD_dia$CAL_11_M<-as.numeric(substring(PD_dia$V1,2599,2600))
    PD_dia$CAL_11_B<-as.numeric(substring(PD_dia$V1,2601,2602))
    PD_dia$CAL_12_M<-as.numeric(substring(PD_dia$V1,2603,2604))
    PD_dia$CAL_12_B<-as.numeric(substring(PD_dia$V1,2605,2606))
    PD_dia$CAL_13_M<-as.numeric(substring(PD_dia$V1,2607,2608))
    PD_dia$CAL_13_B<-as.numeric(substring(PD_dia$V1,2609,2610))
    PD_dia$CAL_14_M<-as.numeric(substring(PD_dia$V1,2611,2612))
    PD_dia$CAL_14_B<-as.numeric(substring(PD_dia$V1,2613,2614))
    PD_dia$CAL_15_M<-as.numeric(substring(PD_dia$V1,2615,2616))
    PD_dia$CAL_15_B<-as.numeric(substring(PD_dia$V1,2617,2618))
    PD_dia$CAL_16_M<-as.numeric(substring(PD_dia$V1,2619,2620))
    PD_dia$CAL_16_B<-as.numeric(substring(PD_dia$V1,2621,2622))
    PD_dia$CAL_17_M<-as.numeric(substring(PD_dia$V1,2623,2624))
    PD_dia$CAL_17_B<-as.numeric(substring(PD_dia$V1,2625,2626))
    PD_dia<-PD_dia[,-1]
    PD_dia[PD_dia=="  "]<-NA
    colnames<-colnames(PD_dia)
    rownames(PD_dia)<-PD_dia$SEQN
    CAL<-grep('CAL.', colnames, value = T)
    PPD<-grep('PPD.', colnames, value = T)
    tooth<-grep('tooth.', colnames, value = T)
    CAL<-PD_dia[,CAL]
    PPD<-PD_dia[,PPD]
    record<-ls()
    rm(list=record[which(record!='PPD'& record!='CAL') ])
  }
  {#** section 1.1.2 CAL diagnosis ####
    #cal=6
    #Set the point with CAL>=6-> 1 and the point CAL<6->0
    CAL[CAL==99]<-NA
    CAL_6<-CAL
    CAL_6[CAL_6<6]<-0
    CAL_6[CAL_6>=6]<-1
    colnames<-colnames(CAL_6)
    tooth<-colnames(CAL)
    toothnumber<-substr(tooth,5,6)
    #Sum the sites of each tooth position CAL>=6. 
    #If it is greater than 1, there are point CAL>=6
    CAL6<-t(CAL_6)
    CAL_TOOTH6<-rowsum(CAL6, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=6 are recorded as 1 
    #and those without CAL>=6 are recorded as 0
    CAL_TOOTH6[CAL_TOOTH6<1]<-0
    CAL_TOOTH6[CAL_TOOTH6>=1]<-1
    CAL_TOOTH61<-as.data.frame(t(CAL_TOOTH6))
    #For teeth with CAL>=6, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=6
    CAL_6number<-rowSums(CAL_TOOTH61,na.rm = T)
    CAL_6number<-data.frame(CAL_6number)
    CAL_6number$SEQN<-rownames(CAL_6number)
    #Cal=4
    #Set the point with CAL>=4-> 1 and the point CAL<4->0
    CAL_4<-CAL
    CAL_4[CAL_4<4]<-0
    CAL_4[CAL_4>=4]<-1
    colnames<-colnames(CAL_4)
    #Sum the sites of each tooth position CAL>=4. 
    #If it is greater than 1, there are point CAL>=4
    CAL4<-t(CAL_4)
    CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=4are recorded as 1 
    #and those without CAL>=4 are recorded as 0
    CAL_TOOTH4[CAL_TOOTH4<1]<-0
    CAL_TOOTH4[CAL_TOOTH4>=1]<-1
    CAL_TOOTH41<-t(CAL_TOOTH4)
    #For teeth with CAL>=4, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=4
    CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
    CAL_4number<-data.frame(CAL_4number)
    CAL_4number$SEQN<-rownames(CAL_4number)
    #Merge the CAL>=4 data into the summary table
    OHXPER.CAL_46<-merge(CAL_6number,CAL_4number,by="SEQN",all = T)
    #Cal=3
    #Set the point with CAL>=3-> 1 and the point CAL<3->0
    CAL_3<-CAL
    CAL_3[CAL_3<3]<-0
    CAL_3[CAL_3>=3]<-1
    colnames<-colnames(CAL_3)
    #Sum the sites of each tooth position CAL>=3. 
    #If it is greater than 1, there are point CAL>=3
    CAL3<-t(CAL_3)
    CAL_TOOTH3<-rowsum(CAL3, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=3are recorded as 1 
    #and those without CAL>=3 are recorded as 0
    CAL_TOOTH3[CAL_TOOTH3<1]<-0
    CAL_TOOTH3[CAL_TOOTH3>=1]<-1
    CAL_TOOTH31<-t(CAL_TOOTH3)
    #For teeth with CAL>=3, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=3
    CAL_3number<-rowSums(CAL_TOOTH31,na.rm = T)
    CAL_3number<-data.frame(CAL_3number)
    CAL_3number$SEQN<-rownames(CAL_3number)
    OHXPER.CAL_346<-merge(OHXPER.CAL_46,CAL_3number,by="SEQN",all = T)
  }
  {#** section 1.1.3 PPD diagnosis ####
    #PPD=5
    PPD[PPD==99]<-NA
    PPD_5<-PPD
    #Set the point with ppd>=5-> 1 and the point ppd<5->0
    PPD_5[PPD_5<5]<-0
    PPD_5[PPD_5>=5]<-1
    colnames<-colnames(PPD_5)
    #Sum the sites of each tooth position ppd>=5. 
    #If it is greater than 1, there are point ppd>=5
    tooth_PPD<-colnames(PPD)
    toothnumber_PPD<-substr(tooth_PPD,5,6)
    PPD51<-t(PPD_5)
    PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=5are recorded as 1 
    #and those without PPD>=5 are recorded as 0
    PPD_TOOTH5[PPD_TOOTH5<1]<-0
    PPD_TOOTH5[PPD_TOOTH5>=1]<-1
    PPD_TOOTH51<-t(PPD_TOOTH5)
    #For teeth with PPD>=5, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=5
    PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
    PPD_5number<-data.frame(PPD_5number)
    #Merge the PPD>=5 data into the summary table
    PPD_5number$SEQN<-rownames(PPD_5number)
    OHXPER.CAL_346.PPD_5<-merge(OHXPER.CAL_346,PPD_5number,by="SEQN",all = T)
    #PPD=4
    #Set the point with ppd>=4-> 1 and the point ppd<4->0
    PPD_4<-PPD
    PPD_4[PPD_4<4]<-0
    PPD_4[PPD_4>=4]<-1
    #Sum the sites of each tooth position ppd>=4. 
    #If it is greater than 1, there are point ppd>=4
    PPD41<-t(PPD_4)
    PPD_TOOTH4<-rowsum(PPD41, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=4are recorded as 1 
    #and those without PPD>=4 are recorded as 0
    PPD_TOOTH4[PPD_TOOTH4<1]<-0
    PPD_TOOTH4[PPD_TOOTH4>=1]<-1
    PPD_TOOTH41<-t(PPD_TOOTH4)
    #For teeth with PPD>=4, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=4
    PPD_4number<-rowSums(PPD_TOOTH41,na.rm = T)
    PPD_4number<-data.frame(PPD_4number)
    #Merge the PPD>=4 data into the summary table
    PPD_4number$SEQN<-rownames(PPD_4number)
    CAL_346.PPD_45<-merge(OHXPER.CAL_346.PPD_5,PPD_4number,by="SEQN",all = T)
    OXPER.ALL<-CAL_346.PPD_45
    
  }
  {#** section 1.1.4 CAL mean ####
    CAL_mean=as.data.frame(rowMeans(CAL,na.rm = T))
    colnames(CAL_mean)<-"CAL_mean"
    colnames(CAL_mean)
    CAL_mean[CAL_mean=="NaN"]<-NA
    CAL_mean$SEQN<-rownames(CAL_mean)
  }
  
  {#** section 1.1.5 PPD mean ####
    PPD_mean=as.data.frame(rowMeans(PPD,na.rm = T))
    colnames(PPD_mean)<-"PPD_mean"
    colnames(PPD_mean)
    PPD_mean[PPD_mean=="NaN"]<-NA
    PPD_mean$SEQN<-rownames(PPD_mean)
  }
  record<-ls()
  rm(list=record[which(record!='OXPER.ALL'&record!='CAL_mean'&record!='PPD_mean'&
                         record!='CAL'&record!='PPD')])
  {#** section 1.1.6 data Collation ####
    CAL_mean$SEQN<-as.numeric(CAL_mean$SEQN)
    PPD_mean$SEQN<-as.numeric(PPD_mean$SEQN)
    OXPER.ALL$SEQN<-as.numeric(OXPER.ALL$SEQN)
    OXPER.CAL<-merge(OXPER.ALL,CAL_mean,by="SEQN",all.x = T)
    OXPER.PPD<-merge(OXPER.CAL,PPD_mean,by="SEQN",all.x = T)
    OXPER.ALL<-na.omit(OXPER.PPD)
    #Graded periodontal disease and normal
    #severe periodontitis 
    OXPER.ALL$severe[OXPER.ALL$CAL_6number>=1&OXPER.ALL$PPD_5number>=1]<-"severe"
    table(OXPER.ALL$severe)
    OXPER.ALL$severe[is.na(OXPER.ALL$severe)] <-"no severe"
    #moderate periodontitis
    OXPER.ALL$moderate[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"moderate"
    OXPER.ALL$moderate[is.na(OXPER.ALL$moderate)] <-"no moderate"
    #mild periodontitis
    OXPER.ALL$mild[(OXPER.ALL$CAL_3number>=1&OXPER.ALL$PPD_4number>=1)]<-"mild"
    OXPER.ALL$mild[is.na(OXPER.ALL$mild)] <-"no Mild"
    #only severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$severe=="severe"]<-"severe"
    #moderat not severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="moderate"& OXPER.ALL$severe=="no severe"]<-"moderate"
    #Mild not moderate
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="no moderate"& OXPER.ALL$mild=="mild"]<-"mild"
    #normal
    OXPER.ALL$Periodontitis_diagnosis[is.na(OXPER.ALL$Periodontitis_diagnosis)] <-"normal"
    #?
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"Moderate/Severe periodontitis"
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number<1&OXPER.ALL$PPD_5number<1]<-"No/Mild periodontitis"
    table(OXPER.ALL$PD_diagnosis,OXPER.ALL$Periodontitis_diagnosis)
    OXPER.diagnosis<-OXPER.ALL[c("SEQN","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='OXPER.diagnosis')])
  }
  {#** section 1.1.7 data selction age>=30 ####
    adult=read.table("G:/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
    adult$SEQN<-as.numeric(substring(adult$V1,1,5))
    age<-as.data.frame(adult$SEQN)
    colnames(age)<-c("SEQN")
    age$ageyr<-as.numeric(substring(adult$V1,18,19))
    age$ageyr[age$ageyr<30]<-NA
    age<-na.omit(age)
    PD_dia_30<-merge(OXPER.diagnosis,age,by="SEQN",all.x = T)
    PD_III<-na.omit(PD_dia_30)
    PD_III$chort<-"NHANES_III"
    PD_III$ID<-paste(PD_III$chort,PD_III$SEQN,sep="_")
    PD_III<-PD_III[,c("SEQN","ID","chort","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='PD_III')])
    #table(PD_III$Periodontitis_diagnosis)
    save(PD_III,file="G:/paper_2_PD&DM/data_DC/PD_III.Rdata")
  }
}
{#* section 1.2 NHANES 1999-2004 Periodontitis diagnosis ####
  {#** section 1.2.1 Data Preparation ####
    #periodontal_data
    OHXPER_9900<-read.xport("G:/paper_2_PD&DM/data/PD/OHXPERIO.XPT")
    OHXPERL_0102<-read.xport("G:/paper_2_PD&DM/data/PD/OHXPRL_B.XPT")
    OHXPERU_0102<-read.xport("G:/paper_2_PD&DM/data/PD/OHXPRU_B.XPT")
    OHXPERL_0304<-read.xport("G:/paper_2_PD&DM/data/PD/OHXPRL_C.XPT")
    OHXPERU_0304<-read.xport("G:/paper_2_PD&DM/data/PD/OHXPRU_C.XPT")
    OHXPER_0102<-merge(OHXPERU_0102,OHXPERL_0102[,
                                                 -which(names(OHXPERU_0102)%in%c("OHAEXSTS","OHASCST4"))],by = "SEQN",all=T)
    OHXPER_0304<-merge(OHXPERU_0304,OHXPERL_0304[,
                                                 -which(names(OHXPERU_0304)%in%c("OHAEXSTS","OHASCST4"))],by = "SEQN",all=T)
    #Periodontal examination integrity and patient number
    myvars<-c("SEQN","OHASCST4")
    PDdata_9900<-OHXPER_9900[myvars]
    OHXPER_9900
    PDdata_0102<-OHXPER_0102[myvars]
    PDdata_0304<-OHXPER_0304[myvars]
    #All patients with complete periodontal examination were combined
    PDdata_9904<-rbind(PDdata_9900,PDdata_0102,PDdata_0304)
    #Only records  with complete periodontal data were selected
    PDdata.Complete<-filter(PDdata_9904,OHASCST4 == 1)
    #Complete periodontal records of the patient's periodontal examination results
    #colnames(OHXPER_9900)<-sub('PCM','PCM',colnames(OHXPER_9900),fixed = F)
    OHXPER_9900
   # colnames(OHXPER_9900)<-sub('LAM','LAM',colnames(OHXPER_9900),fixed = F)
    drop<-c("OHXURGIN","OHXULGIN","OHXLLGIN","OHXLRGIN")
    colnames<-setdiff(colnames(OHXPER_9900),c("OHXURGIN","OHXULGIN","OHXLLGIN","OHXLRGIN"))
    colnames
    colnames<-na.omit(sub('OHD..CJ.',NA,colnames,fixed = F))
    colnames
    OHXPER_9904<-rbind(OHXPER_9900[,colnames],OHXPER_0102[,colnames],OHXPER_0304[,colnames])
    OHXPER.Complete<-filter(OHXPER_9904,OHASCST4 == 1)
    OHXPER<- OHXPER.Complete[,-which(names(OHXPERU_0304)%in%c("OHAEXSTS","OHASCST4"))]
    rownames(OHXPER)<-OHXPER$SEQN
    colnames<-colnames(OHXPER)
    colnames
    #Filter pocket depth data
    pocket_depth <- grep('OHD..PC.', colnames, value = T)
    PPD<-OHXPER[,pocket_depth]
    #Filter CAL data
    CAL<-grep('OHD..LA.', colnames, value = T)
    CAL<-OHXPER[,CAL]
    record<-ls()
    rm(list=record[which(record!='PPD'& record!='CAL') ])
  }

  {#** section 1.2.2 CAL diagnosis ####
    #cal=6
    #Set the point with CAL>=6-> 1 and the point CAL<6->0
    CAL[CAL==99]<-NA
    CAL_6<-CAL
    CAL_6[CAL_6<6]<-0
    CAL_6[CAL_6>=6]<-1
    colnames<-colnames(CAL_6)
    tooth<-colnames(CAL)
    toothnumber<-substr(tooth,4,5)
    #Sum the sites of each tooth position CAL>=6. 
    #If it is greater than 1, there are point CAL>=6
    CAL6<-t(CAL_6)
    CAL_TOOTH6<-rowsum(CAL6, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=6 are recorded as 1 
    #and those without CAL>=6 are recorded as 0
    CAL_TOOTH6[CAL_TOOTH6<1]<-0
    CAL_TOOTH6[CAL_TOOTH6>=1]<-1
    CAL_TOOTH61<-as.data.frame(t(CAL_TOOTH6))
    #For teeth with CAL>=6, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=6
    CAL_6number<-rowSums(CAL_TOOTH61,na.rm = T)
    CAL_6number<-data.frame(CAL_6number)
    CAL_6number$SEQN<-rownames(CAL_6number)
    #Cal=4
    #Set the point with CAL>=4-> 1 and the point CAL<4->0
    CAL_4<-CAL
    CAL_4[CAL_4<4]<-0
    CAL_4[CAL_4>=4]<-1
    colnames<-colnames(CAL_4)
    #Sum the sites of each tooth position CAL>=4. 
    #If it is greater than 1, there are point CAL>=4
    CAL4<-t(CAL_4)
    CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=4are recorded as 1 
    #and those without CAL>=4 are recorded as 0
    CAL_TOOTH4[CAL_TOOTH4<1]<-0
    CAL_TOOTH4[CAL_TOOTH4>=1]<-1
    CAL_TOOTH41<-t(CAL_TOOTH4)
    #For teeth with CAL>=4, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=4
    CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
    CAL_4number<-data.frame(CAL_4number)
    CAL_4number$SEQN<-rownames(CAL_4number)
    #Merge the CAL>=4 data into the summary table
    OHXPER.CAL_46<-merge(CAL_6number,CAL_4number,by="SEQN",all = T)
    #Cal=3
    #Set the point with CAL>=3-> 1 and the point CAL<3->0
    CAL_3<-CAL
    CAL_3[CAL_3<3]<-0
    CAL_3[CAL_3>=3]<-1
    colnames<-colnames(CAL_3)
    #Sum the sites of each tooth position CAL>=3. 
    #If it is greater than 1, there are point CAL>=3
    CAL3<-t(CAL_3)
    CAL_TOOTH3<-rowsum(CAL3, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=3are recorded as 1 
    #and those without CAL>=3 are recorded as 0
    CAL_TOOTH3[CAL_TOOTH3<1]<-0
    CAL_TOOTH3[CAL_TOOTH3>=1]<-1
    CAL_TOOTH31<-t(CAL_TOOTH3)
    #For teeth with CAL>=3, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=3
    CAL_3number<-rowSums(CAL_TOOTH31,na.rm = T)
    CAL_3number<-data.frame(CAL_3number)
    CAL_3number$SEQN<-rownames(CAL_3number)
    OHXPER.CAL_346<-merge(OHXPER.CAL_46,CAL_3number,by="SEQN",all = T)
  }
  {#** section 1.2.3 PPD diagnosis ####
    #PPD=5
    PPD[PPD==99]<-NA
    PPD_5<-PPD
    #Set the point with ppd>=5-> 1 and the point ppd<5->0
    PPD_5[PPD_5<5]<-0
    PPD_5[PPD_5>=5]<-1
    colnames<-colnames(PPD_5)
    #Sum the sites of each tooth position ppd>=5. 
    #If it is greater than 1, there are point ppd>=5
    tooth_PPD<-colnames(PPD)
    toothnumber_PPD<-substr(tooth_PPD,4,5)
    PPD51<-t(PPD_5)
    PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=5are recorded as 1 
    #and those without PPD>=5 are recorded as 0
    PPD_TOOTH5[PPD_TOOTH5<1]<-0
    PPD_TOOTH5[PPD_TOOTH5>=1]<-1
    PPD_TOOTH51<-t(PPD_TOOTH5)
    #For teeth with PPD>=5, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=5
    PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
    PPD_5number<-data.frame(PPD_5number)
    #Merge the PPD>=5 data into the summary table
    PPD_5number$SEQN<-rownames(PPD_5number)
    OHXPER.CAL_346.PPD_5<-merge(OHXPER.CAL_346,PPD_5number,by="SEQN",all = T)
    #PPD=4
    #Set the point with ppd>=4-> 1 and the point ppd<4->0
    PPD_4<-PPD
    PPD_4[PPD_4<4]<-0
    PPD_4[PPD_4>=4]<-1
    #Sum the sites of each tooth position ppd>=4. 
    #If it is greater than 1, there are point ppd>=4
    PPD41<-t(PPD_4)
    PPD_TOOTH4<-rowsum(PPD41, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=4are recorded as 1 
    #and those without PPD>=4 are recorded as 0
    PPD_TOOTH4[PPD_TOOTH4<1]<-0
    PPD_TOOTH4[PPD_TOOTH4>=1]<-1
    PPD_TOOTH41<-t(PPD_TOOTH4)
    #For teeth with PPD>=4, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=4
    PPD_4number<-rowSums(PPD_TOOTH41,na.rm = T)
    PPD_4number<-data.frame(PPD_4number)
    #Merge the PPD>=4 data into the summary table
    PPD_4number$SEQN<-rownames(PPD_4number)
    CAL_346.PPD_45<-merge(OHXPER.CAL_346.PPD_5,PPD_4number,by="SEQN",all = T)
    OXPER.ALL<-CAL_346.PPD_45
    
  }
  {#** section 1.2.4 CAL mean ####
    CAL_mean=as.data.frame(rowMeans(CAL,na.rm = T))
    colnames(CAL_mean)<-"CAL_mean"
    colnames(CAL_mean)
    CAL_mean[CAL_mean=="NaN"]<-NA
    CAL_mean$SEQN<-rownames(CAL_mean)
  }
  
  {#** section 1.2.5 PPD mean ####
    PPD_mean=as.data.frame(rowMeans(PPD,na.rm = T))
    colnames(PPD_mean)<-"PPD_mean"
    colnames(PPD_mean)
    PPD_mean[PPD_mean=="NaN"]<-NA
    PPD_mean$SEQN<-rownames(PPD_mean)
  }
  record<-ls()
  rm(list=record[which(record!='OXPER.ALL'&record!='CAL_mean'&record!='PPD_mean'&
                         record!='CAL'&record!='PPD')])
  {#** section 1.2.6 data Collation ####
    CAL_mean$SEQN<-as.numeric(CAL_mean$SEQN)
    PPD_mean$SEQN<-as.numeric(PPD_mean$SEQN)
    OXPER.ALL$SEQN<-as.numeric(OXPER.ALL$SEQN)
    OXPER.CAL<-merge(OXPER.ALL,CAL_mean,by="SEQN",all.x = T)
    OXPER.PPD<-merge(OXPER.CAL,PPD_mean,by="SEQN",all.x = T)
    OXPER.ALL<-na.omit(OXPER.PPD)
    #Graded periodontal disease and normal
    #severe periodontitis 
    OXPER.ALL$severe[OXPER.ALL$CAL_6number>=1&OXPER.ALL$PPD_5number>=1]<-"severe"
    table(OXPER.ALL$severe)
    OXPER.ALL$severe[is.na(OXPER.ALL$severe)] <-"no severe"
    #moderate periodontitis
    OXPER.ALL$moderate[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"moderate"
    OXPER.ALL$moderate[is.na(OXPER.ALL$moderate)] <-"no moderate"
    #mild periodontitis
    OXPER.ALL$mild[(OXPER.ALL$CAL_3number>=1&OXPER.ALL$PPD_4number>=1)]<-"mild"
    OXPER.ALL$mild[is.na(OXPER.ALL$mild)] <-"no Mild"
    #only severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$severe=="severe"]<-"severe"
    #moderat not severe
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="moderate"& OXPER.ALL$severe=="no severe"]<-"moderate"
    #Mild not moderate
    OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="no moderate"& OXPER.ALL$mild=="mild"]<-"mild"
    #normal
    OXPER.ALL$Periodontitis_diagnosis[is.na(OXPER.ALL$Periodontitis_diagnosis)] <-"normal"
    #?
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"Moderate/Severe periodontitis"
    OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number<1&OXPER.ALL$PPD_5number<1]<-"No/Mild periodontitis"
    table(OXPER.ALL$PD_diagnosis,OXPER.ALL$Periodontitis_diagnosis)
    OXPER.diagnosis<-OXPER.ALL[c("SEQN","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='OXPER.diagnosis')])
  }
  {#** section 1.2.7 data selction age>=30 ####
    age<-db_demo(years = 1999:2004,ageyr=T)
    age$ageyr[age$ageyr<30]<-NA
    age<-na.omit(age)
    colnames(age)[1]<-"SEQN"
    PD_dia_30<-merge(OXPER.diagnosis,age,by="SEQN",all.x = T)
    PD_CON1<-na.omit(PD_dia_30)
    PD_CON1$chort<-"NHANES_CON1"
    PD_CON1$ID<-paste(PD_CON1$chort,PD_CON1$SEQN,sep="_")
    PD_CON1<-PD_CON1[,c("SEQN","ID","chort","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='PD_CON1')])
    #table(PD_CON1$Periodontitis_diagnosis)
    save(PD_CON1,file="G:/paper_2_PD&DM/data_DC/PD_CON1.Rdata")
  }
}
{#* section 1.3 NHANES 2009-2014 Periodontitis diagnosis ####
  {#** section 1.3.1 Data Preparation ####
    #periodontal_data
    
    setwd("G:\\paper_2_PD&DM\\data\\PD")
    OHXPER_0910<-read.xport("OHXPER_F.XPT")
    OHXPER_1112<-read.xport("OHXPER_G.XPT")
    OHXPER_1314<-read.xport("OHXPER_H.XPT")
    
    #Periodontal examination integrity and patient number
    myvars<-c("SEQN","OHDPDSTS")
    PDdata_0910<-OHXPER_0910[myvars]
    PDdata_1112<-OHXPER_1112[myvars]
    PDdata_1314<-OHXPER_1314[myvars]
    #All patients with complete periodontal examination were combined
    PDdata_0914<-rbind(PDdata_0910,PDdata_1112,PDdata_1314)
    #Only records  with complete periodontal data were selected
    PDdata.Complete<-filter(PDdata_0914,OHDPDSTS == 1)
    #Complete periodontal records of the patient's periodontal examination results
    OHXPER_0914<-rbind(OHXPER_0910,OHXPER_1112,OHXPER_1314)
    OHXPER.Complete<-filter(OHXPER_0914,OHDPDSTS == 1)
    
    colnames<-colnames(OHXPER.Complete)
    colnames<-sub('OHX..CJ.',NA,colnames,fixed = F)
    #ɸѡPPD????
    colnames<-sub('OHX..PCL',NA,colnames,fixed = F)#舌中
    colnames<-sub('OHX..PCP',NA,colnames,fixed = F)#远舌 
    colnames<-sub('OHX..PCA',NA,colnames,fixed = F)#近舌 
    colnames<-sub('OHX..PCD',NA,colnames,fixed = F)#颊中 
    #ɸѡcal????
    colnames<-sub('OHX..LAL',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAP',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAA',NA,colnames,fixed = F)
    colnames<-sub('OHX..LAD',NA,colnames,fixed = F)
    colnames
    colnames<-na.omit(colnames)
    OHXPER_9904<-rbind(OHXPER_0910[,colnames],OHXPER_1112[,colnames],OHXPER_1314[,colnames])
    OHXPER.Complete<-filter(OHXPER_9904,OHDPDSTS == 1)
    OHXPER<- OHXPER.Complete[,-c(2,3,4)]
    rownames(OHXPER)<-OHXPER$SEQN
    colnames<-colnames(OHXPER)
    colnames
    #Filter pocket depth data
    pocket_depth <- grep('OHX..PC.', colnames, value = T)
    PPD<-OHXPER[,pocket_depth]
    #Filter CAL data
    CAL<-grep('OHX..LA.', colnames, value = T)
    CAL<-OHXPER[,CAL]
    record<-ls()
    rm(list=record[which(record!='PPD'& record!='CAL') ])
  }
  {#** section 1.3.2 Random screening####
    #Converting 99 to NA values
    CAL[CAL==99]<-NA
    #Set random number
    set.seed(1234)
    Random<-sample(1:4, length(rownames(CAL)),replace = T)
    
    CAL_full=cbind(Random,CAL)   
    CAL_random <- matrix(NA,ncol=length(colnames(CAL)),nrow=length(rownames(CAL)))
    CAL_random <-as.data.frame(CAL_random)
    colnames(CAL_full)
    colnames(CAL_random)=colnames(CAL_full)[2:length(colnames(CAL_full))]
    rownames(CAL_random)=rownames(CAL_full)
    length(colnames(CAL))
    colnames(CAL_full)
    colnames(CAL_full)[2:6]
    for(i in 1:length(rownames(CAL)))
    {
      if(CAL_full[i,"Random"]=="1"){
        CAL_random[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]= 
          CAL_full[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]
      }else if (CAL_full[i,"Random"]=="2"){
        CAL_random[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]= 
          CAL_full[i,c("OHX02LAM","OHX02LAS","OHX03LAM","OHX03LAS",
                       "OHX04LAM","OHX04LAS","OHX05LAM","OHX05LAS",
                       "OHX06LAM","OHX06LAS","OHX07LAM","OHX07LAS",
                       "OHX08LAM","OHX08LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]
      } else if (CAL_full[i,"Random"]=="3"){
        CAL_random[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]= 
          CAL_full[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX18LAM","OHX18LAS","OHX19LAM","OHX19LAS",
                       "OHX20LAM","OHX20LAS","OHX21LAM","OHX21LAS",
                       "OHX22LAM","OHX22LAS","OHX23LAM","OHX23LAS",
                       "OHX24LAM","OHX24LAS")]
      } else if (CAL_full[i,"Random"]=="4"){
        CAL_random[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]= 
          CAL_full[i,c("OHX09LAM","OHX09LAS","OHX10LAM","OHX10LAS",
                       "OHX11LAM","OHX11LAS","OHX12LAM","OHX12LAS",
                       "OHX13LAM","OHX13LAS","OHX14LAM","OHX14LAS",
                       "OHX15LAM","OHX15LAS",
                       "OHX25LAM","OHX25LAS","OHX26LAM","OHX26LAS",
                       "OHX27LAM","OHX27LAS","OHX28LAM","OHX28LAS",
                       "OHX29LAM","OHX29LAS","OHX30LAM","OHX30LAS",
                       "OHX31LAM","OHX31LAS")]
      }
    }
    CAL=CAL_random
    PPD[PPD==99]<-NA
    PPD_full=cbind(Random,PPD)   
    PPD_random <- matrix(NA,ncol=length(colnames(PPD)),nrow=length(rownames(PPD)))
    PPD_random <-as.data.frame(PPD_random)
    colnames(PPD_full)
    colnames(PPD_random)=colnames(PPD_full)[2:length(colnames(PPD_full))]
    rownames(PPD_random)=rownames(PPD_full)
    length(colnames(PPD))
    colnames(PPD_full)
    colnames(PPD_full)[2:6]
    for(i in 1:length(rownames(PPD)))
    {
      if(PPD_full[i,"Random"]=="1"){
        PPD_random[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]= 
          PPD_full[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]
      }else if (PPD_full[i,"Random"]=="2"){
        PPD_random[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]= 
          PPD_full[i,c("OHX02PCM","OHX02PCS","OHX03PCM","OHX03PCS",
                       "OHX04PCM","OHX04PCS","OHX05PCM","OHX05PCS",
                       "OHX06PCM","OHX06PCS","OHX07PCM","OHX07PCS",
                       "OHX08PCM","OHX08PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]
      } else if (PPD_full[i,"Random"]=="3"){
        PPD_random[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]= 
          PPD_full[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX18PCM","OHX18PCS","OHX19PCM","OHX19PCS",
                       "OHX20PCM","OHX20PCS","OHX21PCM","OHX21PCS",
                       "OHX22PCM","OHX22PCS","OHX23PCM","OHX23PCS",
                       "OHX24PCM","OHX24PCS")]
      } else if (PPD_full[i,"Random"]=="4"){
        PPD_random[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]= 
          PPD_full[i,c("OHX09PCM","OHX09PCS","OHX10PCM","OHX10PCS",
                       "OHX11PCM","OHX11PCS","OHX12PCM","OHX12PCS",
                       "OHX13PCM","OHX13PCS","OHX14PCM","OHX14PCS",
                       "OHX15PCM","OHX15PCS",
                       "OHX25PCM","OHX25PCS","OHX26PCM","OHX26PCS",
                       "OHX27PCM","OHX27PCS","OHX28PCM","OHX28PCS",
                       "OHX29PCM","OHX29PCS","OHX30PCM","OHX30PCS",
                       "OHX31PCM","OHX31PCS")]
      }
    }
    PPD<-PPD_random
  }
  {#** section 1.3.3 CAL diagnosis ####
    #cal=6
    #Set the point with CAL>=6-> 1 and the point CAL<6->0
    CAL[CAL==99]<-NA
    CAL_6<-CAL
    CAL_6[CAL_6<6]<-0
    CAL_6[CAL_6>=6]<-1
    colnames<-colnames(CAL_6)
    tooth<-colnames(CAL)
    toothnumber<-substr(tooth,4,5)
    #Sum the sites of each tooth position CAL>=6. 
    #If it is greater than 1, there are point CAL>=6
    CAL6<-t(CAL_6)
    CAL_TOOTH6<-rowsum(CAL6, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=6 are recorded as 1 
    #and those without CAL>=6 are recorded as 0
    CAL_TOOTH6[CAL_TOOTH6<1]<-0
    CAL_TOOTH6[CAL_TOOTH6>=1]<-1
    CAL_TOOTH61<-as.data.frame(t(CAL_TOOTH6))
    #For teeth with CAL>=6, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=6
    CAL_6number<-rowSums(CAL_TOOTH61,na.rm = T)
    CAL_6number<-data.frame(CAL_6number)
    CAL_6number$SEQN<-rownames(CAL_6number)
    #Cal=4
    #Set the point with CAL>=4-> 1 and the point CAL<4->0
    CAL_4<-CAL
    CAL_4[CAL_4<4]<-0
    CAL_4[CAL_4>=4]<-1
    colnames<-colnames(CAL_4)
    #Sum the sites of each tooth position CAL>=4. 
    #If it is greater than 1, there are point CAL>=4
    CAL4<-t(CAL_4)
    CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=4are recorded as 1 
    #and those without CAL>=4 are recorded as 0
    CAL_TOOTH4[CAL_TOOTH4<1]<-0
    CAL_TOOTH4[CAL_TOOTH4>=1]<-1
    CAL_TOOTH41<-t(CAL_TOOTH4)
    #For teeth with CAL>=4, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=4
    CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
    CAL_4number<-data.frame(CAL_4number)
    CAL_4number$SEQN<-rownames(CAL_4number)
    #Merge the CAL>=4 data into the summary table
    OHXPER.CAL_46<-merge(CAL_6number,CAL_4number,by="SEQN",all = T)
    #Cal=3
    #Set the point with CAL>=3-> 1 and the point CAL<3->0
    CAL_3<-CAL
    CAL_3[CAL_3<3]<-0
    CAL_3[CAL_3>=3]<-1
    colnames<-colnames(CAL_3)
    #Sum the sites of each tooth position CAL>=3. 
    #If it is greater than 1, there are point CAL>=3
    CAL3<-t(CAL_3)
    CAL_TOOTH3<-rowsum(CAL3, toothnumber,na.rm = F)
    #According to the tooth position, the teeth with CAL>=3are recorded as 1 
    #and those without CAL>=3 are recorded as 0
    CAL_TOOTH3[CAL_TOOTH3<1]<-0
    CAL_TOOTH3[CAL_TOOTH3>=1]<-1
    CAL_TOOTH31<-t(CAL_TOOTH3)
    #For teeth with CAL>=3, if it is greater than 2, 
    #there are more than 2 teeth with CAL>=3
    CAL_3number<-rowSums(CAL_TOOTH31,na.rm = T)
    CAL_3number<-data.frame(CAL_3number)
    CAL_3number$SEQN<-rownames(CAL_3number)
    OHXPER.CAL_346<-merge(OHXPER.CAL_46,CAL_3number,by="SEQN",all = T)
  }
  {#** section 1.3.4 PPD diagnosis ####
    #PPD=5
    PPD[PPD==99]<-NA
    PPD_5<-PPD
    #Set the point with ppd>=5-> 1 and the point ppd<5->0
    PPD_5[PPD_5<5]<-0
    PPD_5[PPD_5>=5]<-1
    colnames<-colnames(PPD_5)
    #Sum the sites of each tooth position ppd>=5. 
    #If it is greater than 1, there are point ppd>=5
    tooth_PPD<-colnames(PPD)
    toothnumber_PPD<-substr(tooth_PPD,4,5)
    PPD51<-t(PPD_5)
    PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=5are recorded as 1 
    #and those without PPD>=5 are recorded as 0
    PPD_TOOTH5[PPD_TOOTH5<1]<-0
    PPD_TOOTH5[PPD_TOOTH5>=1]<-1
    PPD_TOOTH51<-t(PPD_TOOTH5)
    #For teeth with PPD>=5, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=5
    PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
    PPD_5number<-data.frame(PPD_5number)
    #Merge the PPD>=5 data into the summary table
    PPD_5number$SEQN<-rownames(PPD_5number)
    OHXPER.CAL_346.PPD_5<-merge(OHXPER.CAL_346,PPD_5number,by="SEQN",all = T)
    #PPD=4
    #Set the point with ppd>=4-> 1 and the point ppd<4->0
    PPD_4<-PPD
    PPD_4[PPD_4<4]<-0
    PPD_4[PPD_4>=4]<-1
    #Sum the sites of each tooth position ppd>=4. 
    #If it is greater than 1, there are point ppd>=4
    PPD41<-t(PPD_4)
    PPD_TOOTH4<-rowsum(PPD41, toothnumber_PPD,na.rm = F)
    #According to the tooth position, the teeth with PPD>=4are recorded as 1 
    #and those without PPD>=4 are recorded as 0
    PPD_TOOTH4[PPD_TOOTH4<1]<-0
    PPD_TOOTH4[PPD_TOOTH4>=1]<-1
    PPD_TOOTH41<-t(PPD_TOOTH4)
    #For teeth with PPD>=4, if it is greater than 2, 
    #there are more than 2 teeth with PPD>=4
    PPD_4number<-rowSums(PPD_TOOTH41,na.rm = T)
    PPD_4number<-data.frame(PPD_4number)
    #Merge the PPD>=4 data into the summary table
    PPD_4number$SEQN<-rownames(PPD_4number)
    CAL_346.PPD_45<-merge(OHXPER.CAL_346.PPD_5,PPD_4number,by="SEQN",all = T)
    OXPER.ALL<-CAL_346.PPD_45
    
  }
  {#** section 1.3.5 CAL mean ####
    CAL_mean=as.data.frame(rowMeans(CAL,na.rm = T))
    colnames(CAL_mean)<-"CAL_mean"
    colnames(CAL_mean)
    CAL_mean[CAL_mean=="NaN"]<-NA
    CAL_mean$SEQN<-rownames(CAL_mean)
  }
  
  {#** section 1.3.6 PPD mean ####
    PPD_mean=as.data.frame(rowMeans(PPD,na.rm = T))
    colnames(PPD_mean)<-"PPD_mean"
    colnames(PPD_mean)
    PPD_mean[PPD_mean=="NaN"]<-NA
    PPD_mean$SEQN<-rownames(PPD_mean)
    record<-ls()
    rm(list=record[which(record!='OXPER.ALL'&record!='CAL_mean'&record!='PPD_mean'&
                           record!='CAL'&record!='PPD')])
  }
  
  {#** section 1.3.7 data Collation ####
  CAL_mean$SEQN<-as.numeric(CAL_mean$SEQN)
  PPD_mean$SEQN<-as.numeric(PPD_mean$SEQN)
  OXPER.ALL$SEQN<-as.numeric(OXPER.ALL$SEQN)
  OXPER.CAL<-merge(OXPER.ALL,CAL_mean,by="SEQN",all.x = T)
  OXPER.PPD<-merge(OXPER.CAL,PPD_mean,by="SEQN",all.x = T)
  OXPER.ALL<-na.omit(OXPER.PPD)
  #Graded periodontal disease and normal
  #severe periodontitis 
  OXPER.ALL$severe[OXPER.ALL$CAL_6number>=1&OXPER.ALL$PPD_5number>=1]<-"severe"
  table(OXPER.ALL$severe)
  OXPER.ALL$severe[is.na(OXPER.ALL$severe)] <-"no severe"
  #moderate periodontitis
  OXPER.ALL$moderate[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"moderate"
  OXPER.ALL$moderate[is.na(OXPER.ALL$moderate)] <-"no moderate"
  #mild periodontitis
  OXPER.ALL$mild[(OXPER.ALL$CAL_3number>=1&OXPER.ALL$PPD_4number>=1)]<-"mild"
  OXPER.ALL$mild[is.na(OXPER.ALL$mild)] <-"no Mild"
  #only severe
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$severe=="severe"]<-"severe"
  #moderat not severe
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="moderate"& OXPER.ALL$severe=="no severe"]<-"moderate"
  #Mild not moderate
  OXPER.ALL$Periodontitis_diagnosis[OXPER.ALL$moderate=="no moderate"& OXPER.ALL$mild=="mild"]<-"mild"
  #normal
  OXPER.ALL$Periodontitis_diagnosis[is.na(OXPER.ALL$Periodontitis_diagnosis)] <-"normal"
  #?
  OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number>=1|OXPER.ALL$PPD_5number>=1]<-"Moderate/Severe periodontitis"
  OXPER.ALL$PD_diagnosis[OXPER.ALL$CAL_4number<1&OXPER.ALL$PPD_5number<1]<-"No/Mild periodontitis"
  table(OXPER.ALL$PD_diagnosis,OXPER.ALL$Periodontitis_diagnosis)
  OXPER.diagnosis<-OXPER.ALL[c("SEQN","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
  record<-ls()
  rm(list=record[which(record!='OXPER.diagnosis')])
  }
  {#** section 1.3.8 data selction age>=20 ####
    age<-db_demo(years = 2009:2014,ageyr=T)
    age$ageyr[age$ageyr<20]<-NA
    age<-na.omit(age[,c(1,4)])
    colnames(age)[1]<-"SEQN"
    PD_dia_30<-merge(OXPER.diagnosis,age,by="SEQN",all.x = T)
    PD_CON2<-na.omit(PD_dia_30)
    PD_CON2$chort<-"NHANES_CON2"
    PD_CON2$ID<-paste(PD_CON2$chort,PD_CON2$SEQN,sep="_")
    PD_CON2<-PD_CON2[,c("SEQN","ID","chort","CAL_mean","PPD_mean","PD_diagnosis","Periodontitis_diagnosis")]
    record<-ls()
    rm(list=record[which(record!='PD_CON2')])
    #table(PD_CON1$Periodontitis_diagnosis)
    
    save(PD_CON2,file="G:/paper_2_PD&DM/data_DC/PD_CON2.Rdata")
  }
  
  
}
table(PD_CON2$Periodontitis_diagnosis)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 2. Diabetes diagnosis <<<<< ####
{#* section 2.1 NHANES III Type 2 Diabetes diagnosis ####
  {#** section 2.1.1 Data Collation ####
    lab=read.table("G:/paper_2_PD&DM/data/NHANESIII/lab.dat",sep=",",fill=T)
    #HbA1c(%) > 6.5
    lab$SEQN<-as.numeric(substring(lab$V1,1,5))
    DM_data<-as.data.frame(lab$SEQN)
    colnames(DM_data)<-c("SEQN")
    DM_data$Diabete<-as.numeric(substring(lab$V1,1861,1864))
    DM_data$Diabete[DM_data$Diabete==8888]<-NA
    #Ate_time
    DM_data$Ate_time<-as.numeric(substring(lab$V1,1255,1255))
    #1263-1267
    DM_data$Ate_duration<-as.numeric(substring(lab$V1,1263,1267))
    #TIME
    DM_data$time[DM_data$Ate_time==1|DM_data$Ate_time==3|DM_data$Ate_duration>=8]<-"Satisfied"
    DM_data$time[is.na(DM_data$time)] <- "Unsatisfied"
    #morning weight
    DM_data$Plasma_glu_morning<-as.numeric(substring(lab$V1,113,121))
    #Plasma_glu_1(mmol/L)
    DM_data$Plasma_glu_1<-as.numeric(substring(lab$V1,1871,1876))
    DM_data$Plasma_glu_1[DM_data$Plasma_glu_1==888888]<-NA
    #Plasma_glu_2(mmol/L)
    DM_data$Plasma_glu_2<-as.numeric(substring(lab$V1, 1890,1895))
    DM_data$Plasma_glu_2[DM_data$Plasma_glu_2==888888]<-NA
    #time duration
    # DM_data$Plasma_glu_duration<-as.numeric(substring(lab$V1, 1882,1884))
    #  DM_data$Plasma_glu_duration[DM_data$Plasma_glu_duration==888]<-NA
    adult=read.table("G:/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
    adult$SEQN<-as.numeric(substring(adult$V1,1,5))
    DM_adult<-as.data.frame(adult$SEQN)
    colnames(DM_adult)<-c("SEQN")
    DM_adult$told<-as.numeric(substring(adult$V1,1561,1561))
    DM_adult$told[DM_adult$told==1]<-"T2D"
    DM_adult$told[DM_adult$told==2]<-"No T2D"
    DM_adult$told[DM_adult$told==8]<-NA
    DM_adult$told[DM_adult$told==9]<-NA
    #22->Diabetic on insulin
    DM_data$Plasma_glu_reason<-as.numeric(substring(lab$V1,1249,1249))
    DM_data$Plasma_glu_reason[DM_data$Plasma_glu_reason!=1]<-NA
    DM_data$Plasma_glu_reason[DM_data$Plasma_glu_reason==1]<-"T2D"
    #insulin
    DM_adult$insulin<-as.numeric(substring(adult$V1,1568,1568))
    DM_adult$insulin[DM_adult$insulin==1]<-"T2D"
    DM_adult$insulin[DM_adult$insulin==2]<-NA
    DM_adult$insulin[DM_adult$insulin==8]<-NA
    #medication
    DM_adult$medication<-as.numeric(substring(adult$V1,1578,1578))
    DM_adult$medication[DM_adult$medication==1]<-"T2D"
    DM_adult$medication[DM_adult$medication==2]<-NA
    DM_adult$medication[DM_adult$medication==8]<-NA
    DM_adult$medication[DM_adult$medication==9]<-NA
    
  }
  {#** section 2.1.2 HbA1c diagnosis ####
    #HbA1c(%) > 6.5
    DM_data$HbA1c[DM_data$Diabete>=6.5]<-"T2D"
    DM_data$HbA1c[DM_data$Diabete<6.5]<-"No T2D"
  }
  {#** section 2.1.3 fasting glucose diagnosis ####
    #fasting glucose (mmol/l) >= 7.0
    DM_data$fast_glu[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_1>=7.0)]<-"T2D"
    DM_data$fast_glu[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_1<7.0)]<-"No T2D"
  } 
  {#** section 2.1.4 random blood glucose diagnosis ####
    #random blood glucose (mmol/l) >= 11.1
    DM_data$rand_glu[(DM_data$time=="Unsatisfied")&(DM_data$Plasma_glu_1>=11.1)]<-"T2D"
    DM_data$rand_glu[(DM_data$time=="Unsatisfied")&(DM_data$Plasma_glu_1<11.1)]<-"No T2D"
  }
  {#** section 2.1.5 two-hour OGTT blood glucose diagnosis ####
    #two-hour OGTT blood glucose (mmol/l) >= 11.1
    DM_data$OGTT2[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_morning>0)
                  &(DM_data$Plasma_glu_2>=11.1)]<-"T2D"
    DM_data$OGTT2[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_morning>0)
                  &(DM_data$Plasma_glu_2<11.1)]<-"No T2D"
  }
  {#** section 2.1.6 Use of diabetes medication or insulin diagnosis ####
    #Use of diabetes medication or insulin
    DM_dat<-merge(DM_data,DM_adult,by = "SEQN",all=T)
    DM_dat$drug[DM_dat$Plasma_glu_reason=="T2D"|DM_dat$insulin=="T2D"|
                  DM_dat$medication=="T2D"]<-"T2D"
  }
  {#** section 2.1.7 T2D diagnosis ####
    #Use of diabetes medication or insulin
    DM_dat$T2D[DM_dat$told=="No T2D"|DM_dat$HbA1c=="No T2D"|DM_dat$fast_glu=="No T2D"|
                 DM_dat$OGTT2=="No T2D"|DM_dat$rand_glu=="No T2D"]<-"No T2D"
    DM_dat$T2D[DM_dat$told=="T2D"|DM_dat$HbA1c=="T2D"|DM_dat$fast_glu=="T2D"|
                 DM_dat$OGTT2=="T2D"|DM_dat$rand_glu=="T2D"|DM_dat$drug=="T2D"]<-"T2D"
    DM_III<-DM_dat[,c("SEQN","T2D")]
    record<-ls()
    rm(list=record[which(record!='DM_III')])
  }
  table(DM_III$T2D)
  save(DM_III,file="G:/paper_2_PD&DM/data_DC/DM_III.Rdata")
}

{#* section 2.2 NHANES1999-2004 Type 2 Diabetes diagnosis ####
  
  DM_CON1<-diag_DM(told = T,HbA1c = T,fast_glu = T,OGTT2 = T,
                  rand_glu = T,drug = T,DM1 = F,cat = T,
                  years = 1999:2004,join = "left")
  DM_CON1$DM[DM_CON1$DM=="IFG"]<-'No T2D'
  DM_CON1$DM[DM_CON1$DM=="no"]<-'No T2D'
  DM_CON1$DM[DM_CON1$DM=="DM"]<-'T2D'
  colnames(DM_CON1)<-c("SEQN","T2D")
table(DM_CON1$T2D)
save(DM_CON1,file="G:/paper_2_PD&DM/data_DC/DM_CON1.Rdata")
}  


{#* section 2.3 NHANES2009-2014 Type 2 Diabetes diagnosis ####
  DM_CON2<-diag_DM(told = T,HbA1c = T,fast_glu = T,OGTT2 = T,
                  rand_glu = T,drug = T,DM1 = F,cat = T,
                  years = 2009:2014,join = "left")
  DM_CON2$DM[DM_CON2$DM=="IGT"]<-'No T2D'
  DM_CON2$DM[DM_CON2$DM=="IFG"]<-'No T2D'
  DM_CON2$DM[DM_CON2$DM=="no"]<-'No T2D'
  DM_CON2$DM[DM_CON2$DM=="DM"]<-'T2D'
  colnames(DM_CON2)<-c("SEQN","T2D")
table(DM_CON2$T2D)
save(DM_CON2,file="G:/paper_2_PD&DM/data_DC/DM_CON2.Rdata")
}  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 3. Exclusion Data <<<<< ####
{#* section 3.1 NHANES III Exclusion Data ####
  {#** section 3.1.1 Data Preparation ####
    exam=read.table("G:/paper_2_PD&DM/data/NHANESIII/exam.dat",sep=",",fill=T)
    exam$SEQN<-as.numeric(substring(exam$V1,1,5))
    Ex_exam<-as.data.frame(exam$SEQN)
  }
  {#** section 3.1.2 Pregnancy ####
    Ex_exam$Pregnancy<-as.numeric(substring(exam$V1,5151,5151))
    Ex_exam$Pregnancy[Ex_exam$Pregnancy==1]<-"YES"
    Ex_exam$Pregnancy[Ex_exam$Pregnancy==2]<-"NO"
    Ex_exam$Pregnancy[Ex_exam$Pregnancy==8]<-NA
    Ex_exam$Pregnancy[Ex_exam$Pregnancy==9]<-NA
    Ex_exam$Pregnancy[is.na(Ex_exam$Pregnancy)]<-"NO"
    colnames(Ex_exam)[1]<-"SEQN"
  }
  {#** section 3.1.3 Conbine ####
    EX_III<-Ex_exam[,c("SEQN","Pregnancy")]
    record<-ls()
    rm(list=record[-which(record=='EX_III'|record=='EX_CON')])
  }
}
save(EX_III,file="G:/paper_2_PD&DM/data_DC/EX_III.Rdata")
{#* section 3.2 NHANES1999-2004 Exclusion Data ####
  {#** section 3.2.1 Pregnancy ####
    tsv<-nhs_tsv('demo',years = 1999:2004)
    Pregnancy_CON<-nhs_read(tsv,'ridexprg:Pregnancy')
    table(Pregnancy_CON$Pregnancy)
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Cannot ascertain if SP is pregnant at exam"]<-NA
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Yes, positive lab pregnancy test or self-reported pregnant at exam"]<-"YES"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "SP not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[is.na(Pregnancy_CON$Pregnancy)]<-"NO"
    EX_CON1<-Pregnancy_CON[,c("seqn","Pregnancy")]
    colnames(EX_CON1)[1]<-"SEQN"
    record<-ls()
    rm(list=record[-which(record==c('EX_CON1'))])
  }
}
save(EX_CON1,file="G:/paper_2_PD&DM/data_DC/EX_CON1.Rdata")
{#* section 3.3 NHANES 2009-2014 Exclusion Data ####
  {#** section 3.3.1 Pregnancy ####
    tsv<-nhs_tsv('demo',years = 2009:2014)
    Pregnancy_CON<-nhs_read(tsv,'ridexprg:Pregnancy')
    table(Pregnancy_CON$Pregnancy,useNA = "ifany")
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Cannot ascertain if SP is pregnant at exam"|
                              Pregnancy_CON$Pregnancy=="Cannot ascertain if the participant is pregnant at exam"]<-NA
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "Yes, positive lab pregnancy test or self-reported pregnant at exam"]<-"YES"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "SP not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy==
                              "The participant was not pregnant at exam"]<-"NO"
    Pregnancy_CON$Pregnancy[is.na(Pregnancy_CON$Pregnancy)]<-"NO"
    EX_CON2<-Pregnancy_CON[,c("seqn","Pregnancy")]
    colnames(EX_CON2)[1]<-"SEQN"
    record<-ls()
    rm(list=record[-which(record==c('EX_CON2'))])
  }
}
save(EX_CON2,file="G:/paper_2_PD&DM/data_DC/EX_CON2.Rdata")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 4. Periodontal status of DM patients <<<<< ####
load(file="G:/paper_2_PD&DM/data_DC/DM_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/DM_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/DM_CON2.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/PD_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/PD_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/PD_CON2.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/EX_III.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/EX_CON1.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/EX_CON2.Rdata")
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...)
  }
  return(mergedat)
}
III<-multimerge(list(DM_III,PD_III,EX_III),by="SEQN",all=T)
DM_patient_III<-subset(III,T2D=="T2D"&Pregnancy=="NO")
DM_PD_III<-na.omit(DM_patient_III)

multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...)
  }
  return(mergedat)
}
CON1<-multimerge(list(DM_CON1,PD_CON1,EX_CON1),by="SEQN",all=T)
DM_patient_CON1<-subset(CON1,T2D=="T2D"&Pregnancy=="NO")
DM_PD_CON1<-na.omit(DM_patient_CON1)
multimerge<-function(dat=list(),...){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,...)
  }
  return(mergedat)
}
CON2<-multimerge(list(DM_CON2,PD_CON2,EX_CON2),by="SEQN",all=T)
DM_patient_CON2<-subset(CON2,T2D=="T2D")
DM_PD_CON2<-na.omit(DM_patient_CON2)

DM_PD<-rbind(DM_PD_III,DM_PD_CON1,DM_PD_CON2)
DM_PD_all<-subset(DM_PD,Pregnancy=="NO")
DM_PD_all$Pregnancy<-NULL
save(DM_PD_all,file="G:/paper_2_PD&DM/data_DC/DM_PD_all.Rdata")
save(DM_PD_III,file="G:/paper_2_PD&DM/data_DC/DM_PD_III.Rdata")
save(DM_PD_CON1,file="G:/paper_2_PD&DM/data_DC/DM_PD_CON1.Rdata")
save(DM_PD_CON2,file="G:/paper_2_PD&DM/data_DC/DM_PD_CON2.Rdata")
record<-ls()
rm(list=record[which(record!='DM_PD_all')])

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 5. Ascertainment of Mortality <<<<< ####
{#* section 5.1 NHANES III Ascertainment of Mortality ####
  MORT_III=read.table("G:/NHANES/mort/nhanes_iii_mort_2019_public.tsv",header = T,sep = "\t",fill = F)
  MORT_III <- MORT_III %>%
    mutate(permth = ifelse(is.na(permth_exm),permth_int, permth_exm))
  MORT_III<-MORT_III[,c("seqn","mortstat","ucod_leading","diabetes","permth")]
  table(MORT_III$mortstat, useNA="ifany")
  MORT_III$mortstat[MORT_III$mortstat==0]<-"Alive"
  MORT_III$mortstat[MORT_III$mortstat==1]<-"Deceased"
  MORT_III$ucod_leading[MORT_III$ucod_leading==1|MORT_III$ucod_leading==5]<-
    "CVD"
  MORT_III$ucod_leading[MORT_III$ucod_leading==2]<-
    "Cancer"
  MORT_III$ucod_leading[MORT_III$ucod_leading==3]<-
    "CLRD"
  MORT_III$ucod_leading[MORT_III$ucod_leading==4]<-
    "Accidents"
  MORT_III$ucod_leading[MORT_III$ucod_leading==6]<-
    "Alzheimer"
  MORT_III$ucod_leading[MORT_III$ucod_leading==7]<-
    "DM"
  MORT_III$ucod_leading[MORT_III$ucod_leading==8]<-
    "Flu&pneumonia"
  MORT_III$ucod_leading[MORT_III$ucod_leading==9]<-
    "KD"
  MORT_III$ucod_leading[MORT_III$ucod_leading==10]<-
    "Others"
  table(MORT_III$ucod_leading, useNA="ifany")
  table(MORT_III$diabetes, useNA="ifany")
  MORT_III$diabetes[MORT_III$diabetes==0]<-
    "NO"
  MORT_III$diabetes[MORT_III$diabetes==1]<-
    "YES"
  table(MORT_III$diabetes, useNA="ifany")
  colnames(MORT_III)<-c("SEQN","MORT_stat","ucod_leading","diabetes","permth")
  rownames(MORT_III)<-MORT_III$SEQN 
  record<-ls()
  rm(list=record[which(record!='MORT_III')])
}
{#* section 5.2 NHANES 1999-2004 Ascertainment of Mortality ####
  MORT_CON<-db_mort(years = 1999:2004,codebook = F,varLabel = F,join = "left")
  #If there is a follow-up period for the examination,
  #use the permth_exm, else use the permth_int
  # 0 = Assumed alive
  # 1 = Assumed deceased
  MORT_CON$mortstat[MORT_CON$mortstat==0]<-"Alive"
  MORT_CON$mortstat[MORT_CON$mortstat==1]<-"Deceased"
  # 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
  # 2 = Malignant neoplasms (C00-C97)
  # 3 = Chronic lower respiratory diseases (J40-J47)
  # 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
  # 5 = Cerebrovascular diseases (I60-I69)
  # 6 = Alzheimer's disease (G30)
  # 7 = Diabetes mellitus (E10-E14)
  # 8 = Influenza and pneumonia (J09-J18)
  # 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
  # 10 = All other causes (residual)  
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==1|MORT_CON$ucod_leading==5]<-
    "CVD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==2]<-
    "Cancer"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==3]<-
    "CLRD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==4]<-
    "Accidents"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==6]<-
    "Alzheimer"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==7]<-
    "DM"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==8]<-
    "Flu&pneumonia"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==9]<-
    "KD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==10]<-
    "Others"
  table(MORT_CON$ucod_leading, useNA="ifany")
  table(MORT_CON$diabetes, useNA="ifany")
  # 0 = No - Condition not listed as a multiple cause of death
  # 1 = Yes - Condition listed as a multiple cause of death
  MORT_CON$diabetes[MORT_CON$diabetes==0]<-
    "NO"
  MORT_CON$diabetes[MORT_CON$diabetes==1]<-
    "YES"
  MORT_CON <- MORT_CON %>%
    mutate(permth = ifelse(is.na(permth_exm),permth_int, permth_exm))
  MORT_CON<-MORT_CON[,c("seqn","mortstat","ucod_leading","diabetes","permth")]
  colnames(MORT_CON)<-c("SEQN","MORT_stat","ucod_leading","diabetes","permth")
  rownames(MORT_CON)<-MORT_CON$SEQN
  MORT_CON1<-MORT_CON
  #MORT_CON_stat <- MORT_CON %>% drop_na(MORT_stat)
  #MORT_CON_permth <- MORT_CON_stat %>% drop_na(permth)
  record<-ls()
  rm(list=record[which(record!='MORT_CON1'&record!='MORT_III')])
} 
{#* section 5.3 NHANES 2009-2014 Ascertainment of Mortality ####
  MORT_CON<-db_mort(years = 2009:2014,codebook = F,varLabel = F,join = "left")
  #If there is a follow-up period for the examination,
  #use the permth_exm, else use the permth_int
  # 0 = Assumed alive
  # 1 = Assumed deceased
  MORT_CON$mortstat[MORT_CON$mortstat==0]<-"Alive"
  MORT_CON$mortstat[MORT_CON$mortstat==1]<-"Deceased"
  # 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
  # 2 = Malignant neoplasms (C00-C97)
  # 3 = Chronic lower respiratory diseases (J40-J47)
  # 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
  # 5 = Cerebrovascular diseases (I60-I69)
  # 6 = Alzheimer's disease (G30)
  # 7 = Diabetes mellitus (E10-E14)
  # 8 = Influenza and pneumonia (J09-J18)
  # 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
  # 10 = All other causes (residual)  
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==1|MORT_CON$ucod_leading==5]<-
    "CVD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==2]<-
    "Cancer"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==3]<-
    "CLRD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==4]<-
    "Accidents"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==6]<-
    "Alzheimer"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==7]<-
    "DM"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==8]<-
    "Flu&pneumonia"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==9]<-
    "KD"
  MORT_CON$ucod_leading[MORT_CON$ucod_leading==10]<-
    "Others"
  table(MORT_CON$ucod_leading, useNA="ifany")
  table(MORT_CON$diabetes, useNA="ifany")
  # 0 = No - Condition not listed as a multiple cause of death
  # 1 = Yes - Condition listed as a multiple cause of death
  MORT_CON$diabetes[MORT_CON$diabetes==0]<-
    "NO"
  MORT_CON$diabetes[MORT_CON$diabetes==1]<-
    "YES"
  MORT_CON <- MORT_CON %>%
    mutate(permth = ifelse(is.na(permth_exm),permth_int, permth_exm))
  MORT_CON<-MORT_CON[,c("seqn","mortstat","ucod_leading","diabetes","permth")]
  colnames(MORT_CON)<-c("SEQN","MORT_stat","ucod_leading","diabetes","permth")
  rownames(MORT_CON)<-MORT_CON$SEQN
  MORT_CON2<-MORT_CON
  #MORT_CON_stat <- MORT_CON %>% drop_na(MORT_stat)
  #MORT_CON_permth <- MORT_CON_stat %>% drop_na(permth)
  record<-ls()
  rm(list=record[which(record!='MORT_III'&record!='MORT_CON1'&record!='MORT_CON2')])
} 
save(MORT_III,file="G:/paper_2_PD&DM/data_DC/MORT_III.Rdata")
save(MORT_CON1,file="G:/paper_2_PD&DM/data_DC/MORT_CON1.Rdata")
save(MORT_CON2,file="G:/paper_2_PD&DM/data_DC/MORT_CON2.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 6. Combine of PD & DM & Mortality <<<<< ####
{#* section 6.1 NHANES III Combine of data ####
  load(file="G:/paper_2_PD&DM/data_DC/DM_PD_III.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC//MORT_III.Rdata")
  III_base_mo_stat<-merge(DM_PD_III,MORT_III,by = "SEQN",all= F)
  III_baseline<-III_base_mo_stat %>% drop_na(permth)
  record<-ls()
  rm(list=record[-which(record=='III_baseline')])
} 
{#* section 6.2 NHANES 1999-2004 Combine of data ####
  load(file="G:/paper_2_PD&DM/data_DC/DM_PD_CON1.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/MORT_CON1.Rdata")
  CON1_base_mo_stat<-merge(DM_PD_CON1,MORT_CON1,by = "SEQN",all.x = T)
  CON1_baseline<-CON1_base_mo_stat %>% drop_na(permth)
  record<-ls()
  rm(list=record[which(record!='CON1_baseline'&record!='III_baseline')])
} 

{#* section 6.3 NHANES 2009-2014 Combine of data ####
  load(file="G:/paper_2_PD&DM/data_DC/DM_PD_CON2.Rdata")
  load(file="G:/paper_2_PD&DM/data_DC/MORT_CON2.Rdata")
  CON2_base_mo_stat<-merge(DM_PD_CON2,MORT_CON2,by = "SEQN",all.x = T)
  CON2_baseline<-CON2_base_mo_stat %>% drop_na(permth)
  record<-ls()
  rm(list=record[which(record!='CON1_baseline'&record!='III_baseline'&record!='CON2_baseline')])
} 

save(III_baseline,file="G:/paper_2_PD&DM/data_DC/III_baseline.Rdata")
save(CON1_baseline,file="G:/paper_2_PD&DM/data_DC/CON1_baseline.Rdata")
save(CON2_baseline,file="G:/paper_2_PD&DM/data_DC/CON2_baseline.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 7. Caries Data <<<<< ####
{#* section 7.1 NHANES III Caries Data ####
  Caries=read.table("G:/paper_2_PD&DM/data/NHANESIII/exam.dat",sep=",",fill=T)
  Caries$SEQN<-as.numeric(substring(Caries$V1,1,5))
  Caries$DMFT<-as.numeric(substring(Caries$V1,1915,1916))
  adult=read.table("G:/paper_2_PD&DM/data/NHANESIII/adult.dat",sep=",",fill=T)
  adult$SEQN<-as.numeric(substring(adult$V1,1,5))
  age<-as.data.frame(adult$SEQN)
  colnames(age)<-c("SEQN")
  age$ageyr<-as.numeric(substring(adult$V1,18,19))
  age$ageyr[age$ageyr<30]<-NA
  age<-na.omit(age)
  Caries_30<-merge(Caries[,c("SEQN","DMFT")],age,by="SEQN",all.x = T)
  Caries_III<-na.omit(Caries_30)
  Caries_III$ageyr<-NULL
  record<-ls()
  rm(list=record[which(record!='Caries_III')])
} 
{#* section 7.2 NHANES 1999-2004 Caries Data ####
  ohxden <- nhs_read(nhs_tsv("ohxden")[1:3],codebook = F)
  colnames<-colnames(ohxden)
  Tooth_Count<- grep('ohx..ctc', colnames, value = T)
  ohxdent<-ohxden[,c('seqn',Tooth_Count)]
  ohxdent[ohxdent=="J"|ohxdent=="T"|ohxdent=="U"|ohxdent=="E"|ohxdent=="P"|ohxdent=="R"|ohxdent=="Z"|ohxdent=="K"]<-1
  ohxdent[ohxdent=="S"|ohxdent=="D"]<-0
  ohxdent[ohxdent=="M"|ohxdent=="Q"|ohxdent=="X"|ohxdent=="Y"]<-NA
  age<-db_demo(years = 1999:2004,ageyr=T)
  age$ageyr[age$ageyr<30]<-NA
  age<-na.omit(age)
  colnames(age)[2:4]
  ohxden<-merge(ohxdent,age,by="seqn",all = F)
  rownames(ohxden)<-ohxden$seqn
  ohxden=select(ohxden,-colnames(age))
  colApply <- function(dat, cols = colnames(dat), func = as.numeric) {
    dat[cols] <- lapply(dat[cols], func)
    return(dat)
  }
  fill_dat<-colApply(ohxden,colnames(ohxden), as.numeric)
  caries<-as.data.frame(cbind(rownames(ohxden),rowSums(fill_dat, na.rm = T)))
  colnames(caries)<-c("SEQN","DMFT")
  Caries_CON1<-caries
  Caries_CON1$DMFT<-as.numeric(Caries_CON1$DMFT)
  record<-ls()
  rm(list=record[which(record!='Caries_CON1'&record!='Caries_III')])
} 
{#* section 7.3 NHANES 2009-2014 Caries Data ####
  ohxden <- nhs_read(nhs_tsv("ohxden")[4:6],codebook = F)
  colnames<-colnames(ohxden)
  Tooth_Count<- grep('ohx..ctc', colnames, value = T)
  ohxdent<-ohxden[,c('seqn',Tooth_Count)]
  ohxdent[ohxdent=="J"|ohxdent=="T"|ohxdent=="U"|ohxdent=="E"|ohxdent=="P"|ohxdent=="R"|ohxdent=="Z"|ohxdent=="K"]<-1
  ohxdent[ohxdent=="S"|ohxdent=="D"]<-0
  ohxdent[ohxdent=="M"|ohxdent=="Q"|ohxdent=="X"|ohxdent=="Y"]<-NA
  age<-db_demo(years = 2009:2014,ageyr=T)
  age$ageyr[age$ageyr<30]<-NA
  age<-na.omit(age)
  colnames(age)[2:4]
  ohxden<-merge(ohxdent,age,by="seqn",all = F)
  rownames(ohxden)<-ohxden$seqn
  ohxden=select(ohxden,-colnames(age))
  colApply <- function(dat, cols = colnames(dat), func = as.numeric) {
    dat[cols] <- lapply(dat[cols], func)
    return(dat)
  }
  fill_dat<-colApply(ohxden,colnames(ohxden), as.numeric)
  caries<-as.data.frame(cbind(rownames(ohxden),rowSums(fill_dat, na.rm = T)))
  colnames(caries)<-c("SEQN","DMFT")
  Caries_CON2<-caries
  Caries_CON2$DMFT<-as.numeric(Caries_CON2$DMFT)
  record<-ls()
  rm(list=record[which(record!='Caries_CON1'&record!='Caries_CON2'&record!='Caries_III')])
} 
# summary(Caries_III)
# summary(Caries_CON1)
# summary(Caries_CON2)
load(file="G:/paper_2_PD&DM/data_DC/III_baseline.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/CON1_baseline.Rdata")
load(file="G:/paper_2_PD&DM/data_DC/CON2_baseline.Rdata")
III_baseline<-merge(III_baseline,Caries_III,by = "SEQN",all.x = T)
CON1_baseline<-merge(CON1_baseline,Caries_CON1,by = "SEQN",all.x = T)
CON2_baseline<-merge(CON2_baseline,Caries_CON2,by = "SEQN",all.x = T)
save(III_baseline,file="G:/paper_2_PD&DM/data_DC/III_baseline.Rdata")
save(CON1_baseline,file="G:/paper_2_PD&DM/data_DC/CON1_baseline.Rdata")
save(CON2_baseline,file="G:/paper_2_PD&DM/data_DC/CON2_baseline.Rdata")

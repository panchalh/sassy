library(DescTools)
library(dplyr)
library(haven)
library(scales)
library(sqldf)
library(sjlabelled)

#Create a dummy dataset, masking, randomizing subject ids, treatment arms and randomly keeping a smaller subset.
adslo<-read_sas("L:/LocalPath/adsl.sas7bdat")[, c(3, 11, 13, 14, 18, 19, 31, 32, 7)]
adslo$AGE<-as.integer(runif(length(adslo$SUBJID), min=29, max=93))
adslo$AGEGR<-ifelse(adslo$AGE>=65, '>=65 yrs', '<65 yrs')
adslo$ETHNICN<-as.integer(runif(length(adslo$SUBJID), min=1, max=3))
adslo$ETHNIC<-ifelse(adslo$ETHNICN==1, 'LATINO', 'NOT LATINO')
adslo$RACEN<-as.integer(runif(length(adslo$SUBJID), min=1, max=6))
adslo<-adslo %>% mutate(RACE=case_when(RACEN==1 ~ 'WHITE', 
                                       RACEN==2 ~ 'BLACK',
                                       RACEN==3 ~ 'NATIVE AMERICAN',
                                       RACEN==4 ~ 'ASIAN',
                                       RACEN==5 ~ 'OTHER'))
adslo$COUNTRYN<-as.integer(runif(length(adslo$SUBJID), min=1, max=7))
adslo<-adslo %>% mutate(COUNTRY=case_when(COUNTRYN==1 ~ 'USA', 
                                          COUNTRYN==2 ~ 'CANADA',
                                          COUNTRYN==3 ~ 'AUSTRALIA',
                                          COUNTRYN==4 ~ 'RUSSIA',
                                          COUNTRYN==5 ~ 'UKRAINE',
                                          COUNTRYN==6 ~ 'INDIA'))
adslo$COUNTRYN<-NULL
adslo$USAFL<-ifelse(adslo$COUNTRY=='USA', 'Y', 'N')
adslo$SUBJID<-as.integer(runif(length(adslo$SUBJID), min=1, max=length(adslo$SUBJID)))
adslo<-adslo[order(adslo$SUBJID),]
adslo$SIDREP<-duplicated(adslo$SUBJID)
length(adslo[adslo$SIDREP==TRUE,]$SUBJID)
adslo<-adslo[adslo$SIDREP==FALSE,]
adslo$SIDREP<-NULL
length(adslo$SUBJID)
adslo$TRT01P<-ifelse(adslo$SUBJID%%2==0, 'Active', 'Comparator')
adslo$TRT01PN<-ifelse(adslo$TRT01P=='Active', 1, 2)
adslo<-adslo %>% mutate(DIABTS=case_when(runif(length(adslo$SUBJID), min=0, max=1)<.5  ~ 'Y', 
                                         runif(length(adslo$SUBJID), min=0, max=1)>=.5 ~ 'N'))
adslo<-adslo %>% mutate(DIABTYP=case_when(DIABTS=='Y' & runif(length(adslo$SUBJID), min=0, max=1)<.5 ~ 'Type I Diabetes',
                                          DIABTS=='Y' & runif(length(adslo$SUBJID), min=0, max=1)>=.5 ~ 'Type II Diabetes',
                                          DIABTS=='N' ~ 'No Diabetes'))
adslo$BSWEIGHT<-runif(length(adslo$SUBJID), min=40, max=140)
adslo$BSSBP<-runif(length(adslo$SUBJID), min=83, max=215)
adslo$ITTFL='Y'
adslo$ETHNICN<-NULL
adslo$RACEN<-NULL
adslo$SEX<- adslo$SEX %>% set_label('Gender Male/Female')
adslo<-set_label(adslo, c('Subject ID', 'Gender Male, Female', 'Ethnicity', 'Race', 'Country', 'Country USA Yes/No', 
                          'Planned Treatment', 'Planned Treatment (n)', 'Age (years)', 'Age Group', 'Diabetes Yes/No',
                          'Type of Diabetes', 'Weight (kg)', 'Systolic BP (mmHg)', 'Intent-to-Treat Yes/No'))
adslp<-adslo[rownames(adslo) %in% as.integer(runif(1100, min=1, max=length(adslo$SUBJID))),]




adsl<-set_label(adslp, get_label(adslo))

str(adsl)
attributes(adsl)
get_label(adsl)

#initiate a blank data frame.
alsumry_<-c('seclb', 'rwlb', 'Arm_A', 'Arm_B', 'Overall', 'pvl_1', 'pvl_2', 'hodl', 'pgn')
alsumry<-as.data.frame(t(alsumry_))
names(alsumry)<-alsumry_
alsumry[, c(1:length(alsumry))]<-''

blnkRow<-alsumry

slcontv_<-adsl[, c(2:6, 10:12)]
section_count<-0
for (p in names(slcontv_)){
  inds_<-data.frame(adsl$SUBJID, adsl$TRT01PN, adsl$TRT01P, slcontv_[p], adsl$ITTFL)
  inds_<-inds_ %>% select(c(1:length(inds_)), ju=names(slcontv_[p]))
  inds_$ju<-ifelse(inds_$ju=='', 'Missing', inds_$ju)
  inds_$ju<-ifelse(inds_$ju=='Both Type I and Type II Diabetes', 'Type II Diabetes', inds_$ju)
  idto_<-inds_
  idto_$adsl.TRT01PN<-9
  idto_$adsl.TRT01P<-'All'
  idal_<-rbind(inds_, idto_)

  #Categorical summarization of Age
  ctvfrq1<-as.data.frame(table(idal_$ju[idal_$adsl.ITTFL=='Y'], 
                               idal_$adsl.TRT01PN[idal_$adsl.ITTFL=='Y']))

  ctvfrq1<-ctvfrq1 %>% select(c(1:length(ctvfrq1)), TRT01PN=Var2)

  popt<-as.data.frame(table(idal_$adsl.TRT01PN[idal_$adsl.ITTFL=='Y']))
  popt<-popt %>% select(c(1:length(popt)), TRT01PN=Var1, deno=Freq)

  ctvfrq2<-merge(ctvfrq1, popt, by='TRT01PN')

  ctvfrq2$PCT_<-100*(ctvfrq2$Freq/ctvfrq2$deno)
  ctvfrq2$Vtx<-paste(ctvfrq2$Freq, ' (', sprintf("%.1f", ctvfrq2$PCT_), '%)', sep='')

  # prepare for transpose by dropping variables Freq, deno, PCT_
  ctvfrq3<-ctvfrq2[, c(1, 2, 3, 6)]
  #View(ctvfrq3)
  ctvfrq4<-merge(ctvfrq3[ctvfrq3$TRT01PN==1,], ctvfrq3[ctvfrq3$TRT01PN==2,], by='Var1')

  ctvfrq4<-ctvfrq4 %>% select(c(1:length(ctvfrq4)), Arm_A=Vtx.x, Arm_B=Vtx.y)

  ctvfrq4$frqxy<-ctvfrq4$Freq.x+ctvfrq4$Freq.y

  ctvfrq4$ord_<-ifelse (toupper(ctvfrq4$Var1)=='MISSING', -10, ctvfrq4$frqxy)
  ctvfrq4$ord_<-ifelse (toupper(ctvfrq4$Var1)=='OTHER', -5, ctvfrq4$ord_)

  ctvfrq5<-ctvfrq4[, c(1, 4, 7, 9)]
  ctvfrq6<-merge(ctvfrq5, ctvfrq3[ctvfrq2$TRT01PN==9,], by='Var1')
  ctvfrq6<-ctvfrq6 %>% select(c(1:length(ctvfrq6)), Overall=Vtx)

  ctvfrq7<-ctvfrq6[, c(1:4, 7)]
  ctvfrq7<-ctvfrq7[order(-ctvfrq7$ord_),]
  ctvfrq7<-ctvfrq7 %>% select(c(1:length(ctvfrq7)), rwlb=Var1)

  ctvfrq7$seclb<-''
  ctvfrq7$pvl_1<-''
  ctvfrq7$pvl_2<-''
  ctvfrq7$hodl<-''
  ctvfrq7<-ctvfrq7[, c(6, 1:3, 5, 7:9)]

  #section label row
  ctvfrseclb<-ctvfrq7[row.names(ctvfrq7)=='1',]
  
  indx_<-inds_[inds_$ju != 'Missing',]
  ctvfrseclb$pvl_1<-scales::pvalue(chisq.test(x=indx_$ju, y=indx_$adsl.TRT01PN, correct = FALSE)$p.value)
  ctvfrseclb$pvl_2<-scales::pvalue(fisher.test(x=indx_$ju, y=indx_$adsl.TRT01PN, workspace = 200000000, simulate.p.value=TRUE)$p.value)
 
  ctvfrseclb$seclb<-get_label(slcontv_[p])
  ctvfrseclb[, c(2, 3, 4, 5)]<-''
  ctvfrq8<-rbind(ctvfrseclb, ctvfrq7)
 
  section_count<-section_count+1
  ctvfrq8$pgn<-ifelse(section_count<=5, 1, 2)
  blnkRow$pgn<-ifelse(section_count<=5, 1, 2)
  
  alsumry<-rbind(alsumry, ctvfrq8, blnkRow)
  alsumry$rwlb<-StrAlign(alsumry$rwlb, "\\l")
  print(alsumry)
}







#alsummry<-blnkRow

slcontv_<-adsl[, c(9, 13, 14)]
for (p in names(slcontv_)) {

  inds_<-filter(data.frame(adsl$SUBJID, adsl$TRT01PN, adsl$TRT01P, slcontv_[p], adsl$ITTFL), 
                !is.na(slcontv_[p]), 
                adsl$ITTFL=='Y', 
                !is.na(adsl$TRT01PN))
 
  names(inds_)<-c('USUBJID', 'TRT01PN', 'TRT01P', 'ju', 'ITTFL')
  idto_<-inds_
  idto_$TRT01PN<-9
  idto_$TRT01P<-'All'
  idal_<-rbind(inds_, idto_)
   
  covsuq<-sqldf('select TRT01PN, 
                        count(ju) as n, 
                          avg(ju) as mean, 
                        stdev(ju) as stdev, 
                       median(ju) as median, 
                          min(ju) as min, 
                          max(ju) as max 
                 from idal_ 
                 group by TRT01PN 
                 order by TRT01PN')

  covbyqnt<-as.data.frame(unlist(tapply(idal_$ju, idal_$TRT01PN, quantile, na.rm = TRUE)))
 
  covbyqnt$rowname<-row.names(covbyqnt)
 
  #rename variable
  covbyqnt<-rename(covbyqnt, qcov='unlist(tapply(idal_$ju, idal_$TRT01PN, quantile, na.rm = TRUE))')

  covbyqnt$TRT01PN<-as.integer(sapply(strsplit(covbyqnt$rowname, '%'), function(x)x[1]))
  covbyqnt$qlevel<-as.numeric(sapply(strsplit(covbyqnt$rowname, '%'), function(x)x[1]))-covbyqnt$TRT01PN

  covq1by<-covbyqnt[covbyqnt$qlevel == .25, c(1, 3)]
  covq1by<-covq1by %>% select(c(1:length(covq1by)), q1=qcov)

  covq3by<-covbyqnt[covbyqnt$qlevel == .75, c(1, 3)]
  covq3by<-covq3by %>% select(c(1:length(colnames(covq3by))), q3=qcov)

  covsuq1<-merge(covsuq, covq1by, by="TRT01PN")
  covsuq2<-merge(covsuq1, covq3by, by="TRT01PN")
  
  covsuq2$meanstd<-paste(sprintf("%.1f", covsuq2$mean), ' (', sprintf("%.2f", covsuq2$stdev), ')', sep='')
  covsuq2$medq<-paste(sprintf("%.1f", covsuq2$median), ' (', sprintf("%.1f", covsuq2$q1), ', ', sprintf("%.1f", covsuq2$q3), ')', sep='')
  covsuq2$mimx<-paste(sprintf("%.1f", covsuq2$min), ', ', sprintf("%.1f", covsuq2$max), sep='')
  
  #transpose data frame
  covsuq3<-as.data.frame(t(covsuq2))
  covsuq4<-covsuq3 %>% mutate(rwlb=case_when(row.names(covsuq3)=='n'       ~ 'n',
                                             row.names(covsuq3)=='meanstd' ~ 'Mean (Std)',
                                             row.names(covsuq3)=='medq'    ~ 'Median (Q1, Q3)',
                                             row.names(covsuq3)=='mimx'    ~ 'Min, Max')) %>% filter(!is.na(rwlb))

  covsuq6 <- covsuq4[,c(4, 1, 2, 3)]
  
  covsuq6$pvl_1[row.names(covsuq6)=='n']<-scales::pvalue(wilcox.test(x = inds_$ju[inds_$TRT01PN == 1], y = inds_$ju[inds_$TRT01PN == 2], alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, exact = NULL, correct = TRUE, conf.int = FALSE, conf.level = 0.95)$p.value)
  covsuq6$pvl_2[row.names(covsuq6)=='n']<-scales::pvalue(t.test(x = inds_$ju[inds_$TRT01PN == 1], y = inds_$ju[inds_$TRT01PN == 2], alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)$p.value)
  # Hodges-Lehmann estimate and CI
  hlest<-HodgesLehmann(x = inds_$ju[inds_$TRT01PN == 1], y = inds_$ju[inds_$TRT01PN == 2], conf.level = 0.95)

  covsuq6$hodl[row.names(covsuq6)=='medq']<-paste(sprintf("%.2f", hlest[1]), ' (', sprintf("%.2f", hlest[2]), ', ', sprintf("%.2f", hlest[3]), ')', sep='')

  covsuq6$pvl_1[is.na(covsuq6$pvl_1)]<-""
  covsuq6$pvl_2[is.na(covsuq6$pvl_2)]<-""
  covsuq6$hodl[is.na(covsuq6$hodl)]<-""
  covsuq6$seclb<-''

  # bring section label column to the front
  covsuq8<-covsuq6[, c(8, 1:7)]
  covsuq8<-covsuq8 %>% select(c(1:length(covsuq8)), Arm_A=V1, Arm_B=V2, Overall=V3)

  #section label
  covseclb<-covsuq8[row.names(covsuq6)=='n',]
  covseclb[, c(2, 3, 4, 5)]<-''
  covseclb$seclb<-get_label(slcontv_[p])

  covsuq9<-rbind(covseclb, covsuq8)
  covsuq9$pvl_1[covsuq9$rwlb=='n']<-''
  covsuq9$pvl_2[covsuq9$rwlb=='n']<-''
  
  section_count<-section_count+1
  covsuq9$pgn<-ifelse(section_count<=5, 1, 2)
  blnkRow$pgn<-ifelse(section_count<=5, 1, 2)

  alsumry<-rbind(alsumry, covsuq9, blnkRow)
  alsumry$rwlb<-StrAlign(alsumry$rwlb, "\\l")  
}


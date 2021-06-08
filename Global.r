rm(list=ls())
library(shiny)
library(plotly)
library(data.table)
library(DT)
library(tidyr)
library(shinythemes)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
)

dat<- read.table('dat.txt',h=T)
library(tidyverse)
######################3
dat<- dat %>% mutate(sdhg= sdr_2_hap/sdr_2_geno)
dats=aggregate(cbind(mr_2_True ,    mr_2_hap ,   mr_2_geno ,sdr_2_True  ,  sdr_2_hap ,  sdr_2_geno,sdhg,corhg, hapb,genp )~nrep+  Nvals+  NCD +pAvals +pBvals +pCvals+ pDvals +    rab +   rcd,dat,mean)
dat2 =dats%>% filter (pAvals<=pCvals)
datfile<- dats%>% mutate(pc=0.5*(pAvals+pCvals),pp=paste0(pAvals,"-",pCvals)) %>%dplyr::select(pp,pc,NCD:genp)
names(datfile)<- c('parent_al_fr',"PMcross","samplesize",'pMA','pNA','pMB','pNB','rab','rcd','mr_2_True','mr_2_hap','mr_2_geno','sdr_2_True','sdr_2_hap','sdr_2_geno','sdhg','corhg','hapbpvalue','genbpvalue')
dat22=dat2 %>% mutate(pc=0.5*(pAvals+pCvals),pp=paste0(pAvals,"-",pCvals)) %>% dplyr::select(NCD,pAvals,pCvals,pp,pc,rab,mr_2_True:genp)
dat22[,7:ncol(dat22)]<- round(dat22[,7:ncol(dat22)],3)
dat22[,1]<- factor(dat22[,1])

colnames(dat22)[6:16]<- c("Dprime",'Tr','Ha','Ge','sdt','sdh','sdg','sdhg','corhg','hapb','genp')
newdat<- dat22
#newdat=newdat %>% filter(pAvals<0.5,pCvals<0.5)
newdat=newdat %>% filter(pAvals%in%c(0.05,0.15,0.25,0.35,0.45),pCvals%in%c(0.05,0.15,0.25,0.35,0.45))
newdatm<-newdat[,1:9]
pvaluebias<- newdat[,c(1:6,15:16)]## p value for bias for haplotype and genotype
rpandcor<- newdat[,c(1:6,13:14)]#3 ratio of precission and correlation between genotype and haplotyp
newdatma<-newdatm


    ### Here standard devation
newdatsd<- newdat[,c(1:6,10:12)]
## next change both data into long form
newdatml <- gather(newdatm ,Method, r2value, Tr:Ge, factor_key=TRUE)
newdatsd2 <- gather(newdatsd ,Method, sd, sdt:sdg, factor_key=TRUE)
newdatpv <- gather(pvaluebias ,Method,biaspvalue, hapb:genp, factor_key=TRUE)


newdatmsd<- cbind(newdatml,sd=newdatsd2[,8])
newdatmsd[,2]<- factor(newdatmsd[,2])
newdatmsd[,4]<- factor(newdatmsd[,4])
newdatmsd[,6]<- factor(newdatmsd[,6])
newdatmsd$Method<-  factor( newdatmsd$Method)
newdatmsd[,1]<- paste0(newdatmsd[,1],newdatmsd$Method)
newdatmsd[,1]<- factor(newdatmsd[,1])
newdatmsd[,1]<-factor(newdatmsd[,1], c("100Ge", "100Ha", "100Tr", "300Ge", "300Ha", "300Tr","900Tr", "900Ha" ,"900Ge" , "1800Tr" ,"1800Ha","1800Ge",
                                       "2700Tr" ,"2700Ha","2700Ge"  ))


datmeansd<- newdatmsd

dataeqfr<- newdatmsd[newdatmsd[,2]==newdatmsd[,3],]
newdatmsdd<- cbind(newdatmsd,Method2=paste0(newdatmsd[,7],newdatmsd[,1]))
newdatmsdd[,10]<- factor(newdatmsdd[,10],levels(newdatmsdd[,10])[c(3,4,1,2,7,8,6,5,11,12,9,10)])
newdatmsdd<- newdatmsdd[newdatmsdd[,7]!='Tr',]
write.table(newdatmsdd,'meansd.txt',row.names=F,col.names=T,quote=F)

## Data preparation for bias and mean sqaure error
newdatm$biash<- abs(newdatm$Tr -newdatm$Ha)
newdatm$biasG<- abs(newdatm$Tr -newdatm$Ge)
####
newdatm$biash<- newdatm$Tr -newdatm$Ha
newdatm$biasG<- newdatm$Tr -newdatm$Ge
newdatsd<- newdat[,c(1:6,10:12)]
newdatpv
newdatsd$sdratio<- ifelse (newdatsd$sdh==0,0, (newdatsd$sdh/newdatsd$sdg))

newdatsdm<- cbind(newdatsd,newdatm[,7:11])
####
newdatsdonly<- newdatsdm[,1:9]# standard devaiton only
newdatbias<- newdatsdm[,c(1:6,14:15)]# bias only
#newdatmsdratio<- newdatsdm[,c(1:6,10)]#std ratio
pvaluebias<- newdat[,c(1:6,15:16)]## p value for bias for haplotype and genotype
rpandcor<- newdat[,c(1:6,13:14)]#3
newdatmsdratio<- rpandcor
names(newdatmsdratio)[7:8]<- c('sdratio','corhapgeno')
newdatmsdratio[,7:8]<- signif(newdatmsdratio[,7:8],2)
### re structure the data to long format
newdatsdg<- gather(newdatsdonly,method,sd,sdh:sdg)
newdatbiasg<- gather(newdatbias,method,Bias,biash:biasG)# Bias long form
newdatpv <- gather(pvaluebias ,Method,biaspvalue, hapb:genp, factor_key=TRUE)


#newdatmsg<- gather(newdatms,method,Sdratio,sdratio)# mean sqaure long from
newdatfinal<- cbind(newdatsdg,Bias=newdatbiasg[,8],Biaspvalue=newdatpv[,8])## Bias and precission onlu
newdatfinal$method<- factor(newdatfinal$method)
levels(newdatfinal$method)[levels(newdatfinal$method)=="sdg"] ="Ge"
levels(newdatfinal$method)[levels(newdatfinal$method)=="sdh"] ="Ha"
datbiassd<- newdatfinal %>% mutate(sizemethod=paste0(NCD,method))## data for bias and sd
datbiassd$sizemethod<- factor(datbiassd$sizemethod)
levels(datbiassd$sizemethod)
datbiassd$sizemethod <- factor(datbiassd$sizemethod,levels(datbiassd$sizemethod)[c(1:2,7:10,3:6)])
datbiassd[,6]<- factor(datbiassd[,6])



## Descriptive summary for the paper ( mean bias for Haplotype)
Ha=datbiassd %>%filter(method=='Ha') %>%  dplyr::select(Bias)
mean(abs(Ha[,1]))
max(abs(Ha[,1]))
Ge=datbiassd %>%filter(method=='Ge') %>%  dplyr::select(Bias)
mean(abs(Ge[,1]))
max(abs(Ge[,1]))

##################### Bias with 250 sample size
Ha250=datbiassd %>%filter(method=='Ha',NCD==900) %>%  dplyr::select(Bias)
mean(abs(Ha250[,1]))

Ge250=datbiassd %>%filter(method=='Ge',NCD==900) %>%  dplyr::select(Bias)
mean(abs(Ge250[,1]))

## with 2000 sample size
Ha2000=datbiassd %>%filter(method=='Ha',NCD==2700) %>% dplyr:: select(Bias)
mean(abs(Ha2000[,1]))

Ge2000=datbiassd %>%filter(method=='Ge',NCD==2700) %>% dplyr:: select(Bias)
mean(abs(Ge2000[,1]))
sd((Ge2000[,1]))




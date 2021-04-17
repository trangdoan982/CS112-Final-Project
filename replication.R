###############################################################################################################################################################
#This is the full replication file for Charnysh and Finkel. "The Death Camp Eldorado: Political and Economic Effects of Mass Violence." APSR (forthcoming). ###                
###############################################################################################################################################################

#This file replicates the main empirical analysis in the article
#as well as the additional analysis in the online appendix. 

################################################
#Loading libraries and mail replication files ##
################################################

rm(list=ls())

#install required packages if they are missing:
list.of.packages <- c("stargazer","visreg","spdep","maptools","rgdal","maptools","sandwich","lmtest","RCurl", "SnowballC","wordcloud","RColorBrewer","tm","foreign")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)

### change path to user's local directory
load("/Users/vcharnysh/Dropbox/TREBLINKA/Accept_Revisions_June2017/CharnyshFikelMaterials/Replication/MainDataset.RData")

#Two main datasets are loaded, crd for communities before 1999 and pol for communities after 1999

crd$distRail45KM<-crd$distRail45/1000 #Meters to km 


summary(crd$distTreb[crd$distTreb <=50 & crd$miasto==0])
sd(crd$distTreb[crd$distTreb <=50 & crd$miasto==0])

summary(pol$CampDistKM[pol$CampDistKM <=50 & pol$type!="urban"])
sd(pol$CampDistKM[pol$CampDistKM <=50 & pol$type!="urban"]) 

summary(crd$distRail45KM[crd$distTreb <=50 & crd$miasto==0])
sd(crd$distRail45KM[crd$distTreb <=50 & crd$miasto==0]) 


#########################################################################
############# TABLE 1: OLS, consumption and productive assets  ##########
#########################################################################
#Radios in 1976
regRad<-lm(Radio76 ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regRad) 

#TVs in 1976
regTV<-lm(TV76 ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(regTV) 

#In handicrafts per 1000 people in 1982 
regPrivHand<-lm(Handicr82s ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regPrivHand)  

#Private shops per 1000 people in 1982
crd$PunktyPTs<-(crd$Pukty82-crd$PuktyUs82)/(crd$Pop7980/1000)
regPunktyS<-lm(PunktyPTs ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regPunktyS)  

#Trade volume in Zl. in 1982
crd$TradePP82<-crd$Trade82/crd$Pop7980
regSprzedVol<-lm(TradePP82 ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regSprzedVol)  

#Cattle per 100 ha in 1976
regBydlo<-lm(bydlo~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regBydlo) 

stargazer(regRad, regTV, regPrivHand, regPunktyS, regSprzedVol, regBydlo)

#############################################################################################
######### TABLE 2: Logistic regression, errors corrected for overdispersion #################
#############################################################################################

crd$DwelN4570<-crd$Dwel88-crd$Dw4570 #dwellings not built in 1945-1970

logH50Alog<-glm(cbind(Dw4570, DwelN4570) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(logH50Alog) 

logH50Blog<-glm(cbind(Dw4570, DwelN4570) ~log(distTreb)+log(distRail45KM)+ Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(logH50Blog)  

logH50Clog<-glm(cbind(Dw4570, DwelN4570) ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(logH50Clog)  

## 60 km, only General Government
logH60Clog<-glm(cbind(Dw4570, DwelN4570) ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=60 & crd$miasto==0 & crd$GG==1,], family= quasibinomial)
summary(logH60Clog)  

#70 km, only General Government
logH70Clog<-glm(cbind(Dw4570, DwelN4570) ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=70 & crd$miasto==0 & crd$GG==1,], family= quasibinomial)
summary(logH70Clog) 

stargazer(logH50Alog, logH50Blog, logH50Clog , logH60Clog, logH70Clog)


##################################################################################
####### TABLE 3: Logistic regression, errors corrected for overdispersion  #######
##################################################################################

crd$nonblacha88<-crd$Bud88-crd$blacha88 #roofs not made of metal

regBlach1log<-glm(cbind(blacha88, nonblacha88)~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family=quasibinomial)
summary(regBlach1log) 

regBlach2log<-glm(cbind(blacha88, nonblacha88) ~log(distTreb)+log(distRail45KM), data=crd[crd$distTreb <=50 & crd$miasto==0,], family=quasibinomial)
summary(regBlach2log) 

regBlach3log<-glm(cbind(blacha88, nonblacha88) ~log(distTreb)+log(distRail45KM)+ log(CityDistKm) +Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,], quasibinomial)
summary(regBlach3log) 

#60 km, only GG
regBlach60GGlog<-glm(cbind(blacha88, nonblacha88) ~log(distTreb)+log(distRail45KM)+log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=60 & crd$miasto==0 & crd$GG==1,], quasibinomial)
summary(regBlach60GGlog) 

#70 km, only GG
regBlach70GGlog<-glm(cbind(blacha88, nonblacha88) ~log(distTreb)+log(distRail45KM)+ log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=70 & crd$miasto==0 & crd$GG==1,], quasibinomial)
summary(regBlach70GGlog)

stargazer(regBlach1log, regBlach2log, regBlach3log, regBlach60GGlog, regBlach70GGlog)

##########################################################################################
############# FIGURE 3: Investment in Real Estate and Distance to Treblinka   ############
##########################################################################################

#### PLOTTING THE PREDICTED VALUES FROM REGRESSIONS EXECUTED ABOVE

visreg(logH50Alog, "distTreb", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="Dwellings built in 1945-1970 (1988)", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"), xlim=c(5, 50), ylim=c(0.44, 0.58))

visreg(regBlach1log, "distTreb", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="Dwellings with metal roofs (1988)", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

###############################################################################
############# TABLE 4: Logit Regression, Parliamentary Election 2001 ##########
###############################################################################

pol$NonLPR2001<-pol$Valid2001-pol$LPR2001
LPR2001_50Alog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Alog) 

LPR2001_50Blog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Blog)  

LPR2001_50Clog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50Clog)  

LPR2001_60GGlog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=60 & pol$type!="urban" & pol$GG==1,], quasibinomial)
summary(LPR2001_60GGlog)  

LPR2001_70GGlog<-glm(cbind(LPR2001, NonLPR2001) ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=70 & pol$type!="urban" & pol$GG==1,], quasibinomial)
summary(LPR2001_70GGlog)  

stargazer(LPR2001_50Alog, LPR2001_50Blog, LPR2001_50Clog, LPR2001_60GGlog, LPR2001_70GGlog)

#######################################################################################
############# FIGURE 4: Support for the LPR and Distance to Treblinka    ##############
#######################################################################################

visreg(LPR2001_50Alog, "CampDistKM", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="LPR vote choice (2001)", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

###############################################################################
############# TABLE 5: Logit Regression, Parliamentary Election 2001 ##########
###############################################################################

PiS2001<-glm(cbind(PiS2001, Valid2001-PiS2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2001)    

AWSP2001<-glm(cbind(AWSP2001, Valid2001-AWSP2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(AWSP2001)  

PO2001<-glm(cbind(PO2001, Valid2001-PO2001)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PO2001)  

#PiS, AWSP & PO
all2001<-glm(cbind(PiS2001 + AWSP2001+ PO2001, Valid2001-(PiS2001 + AWSP2001+ PO2001))~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(all2001)  

pol$NonTurn2001<-pol$Elig2001-pol$Turn2001
turnout2001<-glm(cbind(Turn2001, NonTurn2001) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(turnout2001)  

stargazer(PiS2001, AWSP2001, PO2001, all2001, turnout2001)

########################################################################
############# ANALYSIS IN ONLINE APPENDIX STARTS HERE ##################
########################################################################

########################################################################
############# Word-stem count analysis for Figure A1 and Figure A2  ####
########################################################################

#LOAD FILES FOR TEXT ANALYSIS - FIRST CHANGE DIRECTORY TO THE LOCAL DIRECTORY
load("/Users/vcharnysh/Dropbox/TREBLINKA/Accept_Revisions_June2017/CharnyshFikelMaterials/Replication/textStemmed.RData")

#Creating a list of stopwords to remove from the text
stops<-c("ale", "bez","dla", "jak", "jaka", "jaki", "jako","jednak", "albo", "ani","ich", "jej", "juz", "byc","byl", "byla", "bylo","byli", "byloby", "byly", "cala","cali", "oto","choc", "oraz", "opoka", "opopki", "nasz", "nas", "naszych", "naszym", "natomiast", "nawet","nia", "nic", "nich", "nie", "niech", "niego", "oraz", "ona", "pod", "poza", "sie", "tez", "taki","takich", "tam", "ten", "tej","tego","tylko", "tyle","tym", "tych","wobec","wraz","xii", "przez","ktorym", "ktorych", "ktori", "kto", "ktorej", "ktore", "ktora", "cos", "lub", "jest", "czy", "itd", "niz", "gdy", "tak", "sie", "mozna", "musi", "nam", "jego","moze", "wiec", "the", "przed", "maja", "wsrod", "teraz", "przy", "jezeli", "bedzie", "nad", "sobie", "nie", "sie", "###", "bowiem", "bylby", "aby", "bardziej", "bardzo", "oczywiscie", "wszystkie", "wszystkich", "nalezy", "mniej", "mamy", "ktory", "tymczasem","jeszcze", "trzeba", "wiecej","wiele","wielu", "gdyby", "gdzie", "xii", "vii", "viii", "iii", "sama", "samo", "sam", "sobie", "siebie", "swe","swego", "swoja", "swoje", "swoich", "swoim", "swych", "warto","zamiast", "dlatego", "jeden", "duzo", "innych", "ina","inne", "jakie", "jedna", "jedynie", "kazdy", "kazdym", "nim", "str", "potem", "mln", "coraz", "nasze", "taka", "takie", "takze", "usd", "ktorzy", "poprzez", "czyli", "iii")

dtm<- DocumentTermMatrix(docs, control = list(removePunctuation=T, stopwords=stops, removeNumbers=T))#, 

#Organize terms by frequency
freq <- colSums(as.matrix(dtm))   
ord <- order(freq, decreasing = TRUE)  
####################################################################################
#### Figure A1: Word Cloud Constructed from the Five Issues of Opoka w Kraju  ######
####################################################################################

set.seed(123)   
wordcloud(names(freq), freq, min.freq=15, colors=brewer.pal(8, "Accent"))   #

##################################################################################################
####### Figure A2: Text Analysis of Opoka w Kraju between December 2000 and December 2001 ########
##################################################################################################

#Create dictionaries
jews<-c("zyd", "zydzi", "zydom","zydowski", "zydow") 
religion<-c("kosciol", "katolicki", "katolik", "aborcja", "eutanazja") 
eu<-c("unia","unii", "europejski","europa", "brukseli")

#Get word counts
zydSt<-inspect(DocumentTermMatrix(docs, control = list(stopwords = stops, dictionary=jews, removePunctuation=T, removeNumbers=T)))
relSt<-inspect(DocumentTermMatrix(docs, control = list(stopwords = stops, dictionary=religion, removePunctuation=T, removeNumbers=T)))
EUSt<-inspect(DocumentTermMatrix(docs, control = list(stopwords = stops, dictionary=eu, removePunctuation=T, removeNumbers=T)))
 
dates<-c("Dec 2000", "Apr 2001", "Jul 2001", "Sep 2001", "Dec 2001")
plot(1:5, (zydSt[,1]+zydSt[,2]+zydSt[,3]+zydSt[,4]+zydSt[,5]), col="red", type="l", lwd=2, ylab="word count", xlab="", ylim=c(0,60), xaxt = 'n', main="Content of 'Opoka w Kraju'")
lines(1:5, (relSt[,1]+relSt[,2]+relSt[,3]+relSt[,4]+relSt[,5]), col="blue", lty=3, lwd=2)
lines(1:5, (EUSt[,1]+EUSt[,2]+EUSt[,3]+EUSt[,4]+EUSt[,5]), col="purple", lty=6, lwd=2)
axis(1, at=1:5, labels=dates[1:5])
abline(v=4, col="red", lwd=2) #Line on the Sep. 2001 election month
legend(60, c("Jews, Jewish", "Catholic, Church, Abortion, Euthanasia", "EU, European, Europe, Brussels"), lty=c(1,3,6), col=c("red", "blue", "purple"), cex=.73, bty="n")

########################################################################
############# Table A4: OLS Regression, Human Capital ##################
########################################################################

crd$shareSec78<-(crd$HighEd78+ crd$SecEd78)/crd$pop1978
summary(crd$shareSec78[crd$distTreb <=50 &crd$miasto==0])
sd(crd$shareSec78[crd$distTreb <=50 &crd$miasto==0])


regEdu1<-lm(shareSec78~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regEdu1) #NOT ROBUST ERRORS
cov1         <- vcovHC(regEdu1, type = "HC1")
robust_se1    <- sqrt(diag(cov1)) #ROBUST ERRORS
wald_results1 <- waldtest(regEdu1, vcov = cov1)

regEdu2<-lm(shareSec78~log(distTreb)+log(distRail45KM) +log(CityDistKm), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regEdu2) 
cov2         <- vcovHC(regEdu2, type = "HC1")
robust_se2    <- sqrt(diag(cov2))#ROBUST ERRORS
wald_results2 <- waldtest(regEdu2, vcov = cov2) 

regEdu3<-lm(shareSec78 ~log(distTreb)+log(distRail45KM) +log(CityDistKm), data=crd[crd$distTreb <=60 &crd$miasto==0 & crd$GG==1,])
summary(regEdu3) 
cov3         <- vcovHC(regEdu3, type = "HC1")
robust_se3    <- sqrt(diag(cov3))#ROBUST ERRORS
wald_results3 <- waldtest(regEdu3, vcov = cov3) 

crd$shareSec88<-(crd$HighEd88 + crd$SecEd88)/crd$Pop19Ov88
summary(crd$shareSec88[crd$distTreb <=50 &crd$miasto==0])
sd(crd$shareSec88[crd$distTreb <=50 &crd$miasto==0])

regEdu4<-lm(shareSec88 ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regEdu4) 
cov4         <- vcovHC(regEdu4, type = "HC1")
robust_se4   <- sqrt(diag(cov4))#ROBUST ERRORS
wald_results4 <- waldtest(regEdu4, vcov = cov4) 

regEdu5<-lm(shareSec88 ~log(distTreb)+log(distRail45KM) +log(CityDistKm), data=crd[crd$distTreb <=50 &crd$miasto==0,])
summary(regEdu5) 
cov5         <- vcovHC(regEdu5, type = "HC1")
robust_se5   <- sqrt(diag(cov5))#ROBUST ERRORS
wald_results5 <- waldtest(regEdu5, vcov = cov5) 

regEdu6<-lm(shareSec88 ~log(distTreb)+log(distRail45KM) +log(CityDistKm), data=crd[crd$distTreb <=60 &crd$miasto==0 & crd$GG==1,])
summary(regEdu6,robust=T) 
cov6         <- vcovHC(regEdu6, type = "HC1")
robust_se6   <- sqrt(diag(cov6))#ROBUST ERRORS
wald_results6 <- waldtest(regEdu6, vcov = cov6) 

stargazer(regEdu1, regEdu2, regEdu3, regEdu4, regEdu5, regEdu6, se=list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5, robust_se6))

#############################################################################
###### Table A5: OLS Regression, Income & Entrepreneurship in 1995 ##########
#############################################################################

pol$IncTaxPC95<-as.numeric(as.character(pol$IncTax95))/pol$Pop95
summary(pol$IncTaxPC95[pol$CampDistKM <=50 & pol$type!="urban"])
sd(pol$IncTaxPC95[pol$CampDistKM <=50 & pol$type!="urban"])

pol$PrivEntPC95<-pol$PrivEnt95/(pol$Pop95/1000)
summary(pol$PrivEntPC95[pol$CampDistKM <=50 & pol$type!="urban"])
sd(pol$PrivEntPC95[pol$CampDistKM <=50 & pol$type!="urban"])
incReg50<-lm(log(IncTaxPC95) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(incReg50)  

incReg50C<-lm(log(IncTaxPC95) ~log(CampDistKM)+log(distRail45)+log(CityDistKm), data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(incReg50C)  

incReg60C<-lm(log(IncTaxPC95) ~log(CampDistKM)+log(distRail45)+log(CityDistKm), data=pol[pol$CampDistKM <=60 & pol$type!="urban" & pol$GG==1,])
summary(incReg60C)  

entReg50<-lm(log(PrivEntPC95) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(entReg50)  

entReg50C<-lm(log(PrivEntPC95) ~log(CampDistKM)+log(distRail45)+log(CityDistKm), data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(entReg50C) 

entReg60C<-lm(log(PrivEntPC95) ~log(CampDistKM)+log(distRail45)+log(CityDistKm), data=pol[pol$CampDistKM <=60 & pol$type!="urban" & pol$GG==1,])
summary(entReg60C) 

stargazer(incReg50, incReg50C, incReg60C, entReg50, entReg50C, entReg60C)

##########################################################
### TABLE A6: ALTERNATIVE FUNCTIONAL FORMS of DISTANCE ###
##########################################################

logH50A<-glm(cbind(Dw4570, DwelN4570) ~distTreb, data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(logH50A) 

logH50Asq<-glm(cbind(Dw4570, DwelN4570) ~distTreb+I(distTreb^2), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(logH50Asq)

regBlach1<-glm(cbind(blacha88, nonblacha88)~distTreb, data=crd[crd$distTreb <=50 & crd$miasto==0,], family=quasibinomial)
summary(regBlach1) 

regBlach1sq<-glm(cbind(blacha88, nonblacha88)~distTreb+I(distTreb^2), data=crd[crd$distTreb <=50 & crd$miasto==0,], family=quasibinomial)
summary(regBlach1sq) 

LPR2001_50A<-glm(cbind(LPR2001, NonLPR2001) ~CampDistKM, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50A) 

LPR2001_50sq<-glm(cbind(LPR2001, NonLPR2001) ~CampDistKM+I(CampDistKM^2), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2001_50sq) 

stargazer(logH50A, logH50Asq, regBlach1, regBlach1sq, LPR2001_50A, LPR2001_50sq)

############################################################
### FIGURE A4: Interpreting Transformations of Distance  ###
############################################################

regBlach1log<-glm(cbind(blacha88, nonblacha88)~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family=quasibinomial)
summary(regBlach1log) 

visreg(regBlach1log, "distTreb", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="Model with log-transformed distance", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

visreg(regBlach1sq, "distTreb", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="Model with a second-order polynomial", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

visreg(regBlach1, "distTreb", scale="response", ylab="Proportion", xlab="Distance to Treblinka, km", main="Model with no transformation ", fill.par=list(density = 15, angle = 90, col="blue"), line.par=list(col="black"))

##################################################################
###### TABLE A7: OLS Regression, Investment in New Houses ########
##################################################################
crd$shareDwel<-crd$Dw4570/crd$Dwel88
summary(crd$shareDwel[crd$distTreb <=50 & crd$miasto==0])
sd(crd$shareDwel[crd$distTreb <=50 & crd$miasto==0])

logH50Aols<-lm(shareDwel ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(logH50Aols) #NOT ROBUST SES
cov1         <- vcovHC(logH50Aols, type = "HC1")
robust_se1    <- sqrt(diag(cov1)) ##ROBUST SES
wald_results1 <- waldtest(logH50Aols, vcov = cov1) 

logH50Bols<-lm(shareDwel ~log(distTreb)+log(distRail45KM)+ Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(logH50Bols)   
cov2         <- vcovHC(logH50Bols, type = "HC1")
robust_se2    <- sqrt(diag(cov2))
wald_results2 <- waldtest(logH50Bols, vcov = cov2) 

logH50Cols<-lm(shareDwel ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(logH50Cols) 
cov3         <- vcovHC(logH50Cols, type = "HC1")
robust_se3    <- sqrt(diag(cov3))
wald_results3 <- waldtest(logH50Cols, vcov = cov3) 

#60 km, only GG
logH60Cols<-lm(shareDwel ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=60 & crd$miasto==0 & crd$GG==1,])
summary(logH60Cols)   
cov4         <- vcovHC(logH60Cols, type = "HC1")
robust_se4    <- sqrt(diag(cov4))
wald_results4 <- waldtest(logH60Cols, vcov = cov4) 

#70 km, only GG
logH70Cols<-lm(shareDwel ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=70 & crd$miasto==0 & crd$GG==1,])
summary(logH70Cols)
cov5         <- vcovHC(logH70Cols, type = "HC1")
robust_se5   <- sqrt(diag(cov5))
wald_results5 <- waldtest(logH70Cols, vcov = cov5) 

stargazer(logH50Aols, logH50Bols, logH50Cols, logH60Cols, logH70Cols, se= list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5))


####################################################################
###### TABLE A8:OLS Regression, Investment in Better Roofs #########
####################################################################
crd$shareBlacha88<-crd$blacha88/crd$Bud88
summary(crd$shareBlacha88[crd$distTreb <=50 & crd$miasto==0])
sd(crd$shareBlacha88[crd$distTreb <=50 & crd$miasto==0])

regBlach1ols<-lm(shareBlacha88 ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(regBlach1ols) #NOT ROBUST SES 
cov1         <- vcovHC(regBlach1ols, type = "HC1")
robust_se1    <- sqrt(diag(cov1)) #ROBUST SES
wald_results1 <- waldtest(regBlach1ols, vcov = cov1) 

regBlach2ols<-lm(shareBlacha88 ~log(distTreb)+log(distTreb)+log(distRail45KM), data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(regBlach2ols)  
cov2         <- vcovHC(regBlach2ols, type = "HC1")
robust_se2    <- sqrt(diag(cov2))
wald_results2 <- waldtest(regBlach2ols, vcov = cov2) 

regBlach3ols<-lm(shareBlacha88 ~log(distTreb)+log(distTreb)+log(distRail45KM)+ log(CityDistKm) +Destr46, data=crd[crd$distTreb <=50 & crd$miasto==0,])
summary(regBlach3ols) 
cov3         <- vcovHC(regBlach3ols, type = "HC1")
robust_se3    <- sqrt(diag(cov3))
wald_results3 <- waldtest(regBlach3ols, vcov = cov3) 

regBlach60GGols<-lm(shareBlacha88 ~log(distTreb)+log(distRail45KM)+log(CityDistKm)+ Destr46, data=crd[crd$distTreb <=60 & crd$miasto==0 & crd$GG==1,])
summary(regBlach60GGols)   
cov4         <- vcovHC(regBlach60GGols, type = "HC1")
robust_se4    <- sqrt(diag(cov4))
wald_results4 <- waldtest(regBlach60GGols, vcov = cov4) 

regBlach70GGols<-lm(shareBlacha88 ~log(distTreb)+log(distRail45KM)+ log(CityDistKm)+Destr46, data=crd[crd$distTreb <=70 & crd$miasto==0 & crd$GG==1,])
summary(regBlach70GGols)       
cov5         <- vcovHC(regBlach70GGols, type = "HC1")
robust_se5    <- sqrt(diag(cov5))
wald_results5 <- waldtest(regBlach70GGols, vcov = cov5) 

stargazer(regBlach1ols, regBlach2ols, regBlach3ols, regBlach60GGols, regBlach70GGols,se= list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5))

####################################################################################
###### TABLE A9: OLS Regression, Support for the LPR in the 2001 Election ##########
####################################################################################

pol$PctLPR2001<-pol$LPR2001/pol$Valid2001
summary(pol$PctLPR2001[pol$CampDistKM <=50 & pol$type!="urban"])
sd(pol$PctLPR2001[pol$CampDistKM <=50 & pol$type!="urban"])

LPR2001_50ols1<-lm(PctLPR2001 ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(LPR2001_50ols1)      #NOT ROBUST SES
cov1         <- vcovHC(LPR2001_50ols1, type = "HC1")
robust_se1    <- sqrt(diag(cov1)) #ROBUST SES
wald_results1 <- waldtest(LPR2001_50ols1, vcov = cov1) 

LPR2001_50ols2<-lm(PctLPR2001 ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(LPR2001_50ols2)       
cov2         <- vcovHC(LPR2001_50ols2, type = "HC1")
robust_se2    <- sqrt(diag(cov2))
wald_results2 <- waldtest(LPR2001_50ols2, vcov = cov2) 

LPR2001_50ols3<-lm(PctLPR2001 ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",])
summary(LPR2001_50ols3)  
cov3         <- vcovHC(LPR2001_50ols3, type = "HC1")
robust_se3    <- sqrt(diag(cov3))
wald_results3 <- waldtest(LPR2001_50ols3, vcov = cov3) 

LPR2001_60GGols<-lm(PctLPR2001 ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=60 & pol$type!="urban" & pol$GG==1,])
summary(LPR2001_60GGols)  
cov4         <- vcovHC(LPR2001_60GGols, type = "HC1")
robust_se4    <- sqrt(diag(cov4))
wald_results4 <- waldtest(LPR2001_60GGols, vcov = cov4) 

LPR2001_70GGols<-lm(PctLPR2001 ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data=pol[pol$CampDistKM <=70 & pol$type!="urban" & pol$GG==1,])
summary(LPR2001_70GGols)  
cov5         <- vcovHC(LPR2001_70GGols, type = "HC1")
robust_se5    <- sqrt(diag(cov5))
wald_results5 <- waldtest(LPR2001_70GGols, vcov = cov5) 

stargazer(LPR2001_50ols1, LPR2001_50ols2, LPR2001_50ols3, LPR2001_60GGols, LPR2001_70GGols, se=list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5))

################################################################################################
########################### TABLE A11: Spatial Error Regression ################################
################################################################################################

######## DWELLINGS BUILT IN 1945-1970  #######

homeSp50 <-crd[crd$distTreb <=50 & crd$miasto==0,]
map_crdH50 <- coordinates(homeSp50) 
W_contH50 <- poly2nb(homeSp50, queen=T)
W_cont_matH50 <- nb2listw(W_contH50, style="W", zero.policy=TRUE) 
lmHouses<-lm(shareDwel ~log(distTreb), data=homeSp50) 
summary(lmHouses)   
lm.morantest(lmHouses, W_cont_matH50) 

house.semH50a <- errorsarlm(shareDwel ~log(distTreb), data = homeSp50, listw=W_cont_matH50, zero.policy=T, tol.solve=1e-15)
summary(house.semH50a)
house.semH50b <- errorsarlm(shareDwel ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data = homeSp50, listw=W_cont_matH50, zero.policy=T, tol.solve=1e-15)
summary(house.semH50b) 

######## DWELLINGS WITH METAL ROOFS  #######
lmRoofs<-lm(shareBlacha88 ~log(distTreb), data=homeSp50) 
summary(lmRoofs)   
lm.morantest(lmRoofs, W_cont_matH50) 

lm.LMtests(lmRoofs, W_cont_matH50, test="all")

roof.semH50a <- errorsarlm(shareBlacha88 ~log(distTreb), data = homeSp50, listw=W_cont_matH50, zero.policy=T, tol.solve=1e-15)
summary(roof.semH50a) 
roof.semH50b <- errorsarlm(shareBlacha88 ~log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data = homeSp50, listw=W_cont_matH50, zero.policy=T, tol.solve=1e-15)
summary(roof.semH50b) 


######## SUPPORT FOR THE LPR #######
treb<-pol[pol$CampDistKM <=50 & pol$type!="urban",] ## Create matrix of polygon centroids
map_crd <- coordinates(treb) 
W_cont <- poly2nb(treb, queen=T)
##NEXT WE ASSIGN SPATIAL WEIGHTS
W_cont_mat <- nb2listw(W_cont, style="W", zero.policy=TRUE) 
## Local Autocorrelation: Local Moran's I (normality assumption)
lm1 <- localmoran(treb$PctLPR2001, listw=W_cont_mat, zero.policy=T)
treb$lm1 <- abs(lm1[,4]) ## Extract z-scores
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(treb, zcol="lm1", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T)
LPR2001_50ols1<-lm(PctLPR2001 ~log(CampDistKM), data=treb) 
summary(LPR2001_50ols1)  
lm.morantest(LPR2001_50ols1, W_cont_mat) 

lm.LMtests(LPR2001_50ols1, W_cont_mat, test="all") 

##Spatially lagged error model, 50 km radius    
mod.sema <- errorsarlm(PctLPR2001 ~log(CampDistKM), data = treb, listw=W_cont_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sema) 
mod.semb <- errorsarlm(PctLPR2001 ~log(CampDistKM)+log(distRail45)+log(CityDistKm)+ EllDist01, data = treb, listw=W_cont_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.semb) 

stargazer(house.semH50a, house.semH50b, roof.semH50a, roof.semH50b, mod.sema, mod.semb)

################################################################################################
############# TABLE A11: Logit Regression, Vote for Endecja and BNM in 1928 ####################
################################################################################################
vote1928<-read.csv("election1928.csv") 
head(vote1928)

vote1928$shareBM<-vote1928$BlockMniejsz/vote1928$Valid1928
summary(vote1928$shareBM[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])
sd(vote1928$shareBM[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])

vote1928$shareEndecja<-vote1928$Endecja/vote1928$Valid1928
summary(vote1928$shareEndecja[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])
sd(vote1928$shareEndecja[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w"])

vote1928$nonBM<-vote1928$Valid1928-vote1928$BlockMniejsz
vote1928$nonEndecja<-vote1928$Valid1928-vote1928$Endecja

reg50ND<-glm(cbind(Endecja, nonEndecja) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w",], quasibinomial)
summary(reg50ND) 

reg60ND<-glm(cbind(Endecja, nonEndecja) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=60 & vote1928$Type=="w",], quasibinomial)
summary(reg60ND) 

reg50BM<-glm(cbind(BlockMniejsz, nonBM) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=50 & vote1928$Type=="w",], quasibinomial)
summary(reg50BM) 

reg60BM<-glm(cbind(BlockMniejsz, nonBM) ~log(Distance.to.Treblinka), data=vote1928[vote1928$Distance.to.Treblinka<=60 & vote1928$Type=="w",], quasibinomial)
summary(reg60BM)

stargazer(reg50ND, reg60ND, reg50BM, reg60BM)


########################################################################################################
############# TABLE A12: Logit Regression, Support for Right-Wing Parties in 1997, 2005, 2015 ##########
########################################################################################################

crd$nonAWS1997<-crd$Valid97-crd$AWS1997

#regAWS1997a<-glm(cbind(AWS1997, nonAWS1997) ~log(distTreb), data=crd[crd$distTreb <=50 &crd$miasto==0,], quasibinomial)
#summary(regAWS1997a) 

regAWS1997b<-glm(cbind(AWS1997, nonAWS1997) ~log(distTreb)+Okreg1997, data=crd[crd$distTreb <=50 &crd$miasto==0,], quasibinomial)
summary(regAWS1997b) 

pol$NonLPR2005<-pol$Valid2005-pol$LPR2005

#LPR2005a<-glm(cbind(LPR2005, NonLPR2005) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
#summary(LPR2005a)

LPR2005b<-glm(cbind(LPR2005, NonLPR2005) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(LPR2005b)

pol$NonPiS2005<-pol$Valid2005-pol$PiS2005
#PiS2005a<-glm(cbind(PiS2005, NonPiS2005) ~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
#summary(PiS2005a)

PiS2005b<-glm(cbind(PiS2005, NonPiS2005) ~log(CampDistKM)+ EllDist01, data=pol[pol$CampDistKM <=50 & pol$type!="urban",],quasibinomial)
summary(PiS2005b) 

pol$NonPiS2015<-pol$Valid2015-pol$PiS2015
PiS2015a<-glm(cbind(PiS2015, NonPiS2015)~log(CampDistKM), data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2015a)  

PiS2015b<-glm(cbind(PiS2015, NonPiS2015)~log(CampDistKM)+ ElDist15, data=pol[pol$CampDistKM <=50 & pol$type!="urban",], quasibinomial)
summary(PiS2015b)   

stargazer(regAWS1997b, LPR2005b, PiS2005b, PiS2015a, PiS2015b)

#############################################################
###### Table A13: Logit Regression, Migration (1988)  #######
#############################################################
mig<-read.csv("Migration88.csv")
head(mig)

summary(mig$FromBirthPct1988[mig$Type1988=="w" & mig$DistanceTreblinka<=50])
sd(na.omit(mig$FromBirthPct1988[mig$Type1988=="w" & mig$DistanceTreblinka<=50]))

mig$town<-ifelse(mig$Type1988=="w", 0, 1)
mig$NumFromBirth88<-round(mig$TotPop1988*(mig$FromBirthPct1988/100))
mig$NumNotFromBirth88<-mig$TotPop1988-mig$NumFromBirth88

regM1r<-glm(cbind(NumFromBirth88, NumNotFromBirth88) ~ log(DistanceTreblinka), data=mig[mig$Type1988=="w" & mig$DistanceTreblinka<=50,],family=quasibinomial)
summary(regM1r)

regM2r<-glm(cbind(NumFromBirth88, NumNotFromBirth88)~log(DistanceTreblinka)+ log(DistanceRailway)+log(DistToCityKM), mig[mig$Type1988=="w" & mig$DistanceTreblinka<=50,],family=quasibinomial)
summary(regM2r)

regM3r<-glm(cbind(NumFromBirth88, NumNotFromBirth88)~log(DistanceTreblinka)+ log(DistanceRailway)+log(DistToCityKM), mig[mig$Type1988=="w" & mig$DistanceTreblinka<=60 & mig$GG==1,],family=quasibinomial)
summary(regM3r)

regInter1<-glm(cbind(NumFromBirth88, NumNotFromBirth88)~log(DistanceTreblinka)+ log(DistanceRailway) +town+I(town*log(DistanceTreblinka))+log(DistToCityKM), mig[mig$DistanceTreblinka<=50,],family=quasibinomial)
summary(regInter1)

regInter2<-glm(cbind(NumFromBirth88, NumNotFromBirth88)~log(DistanceTreblinka)+ log(DistanceRailway)+town+I(town*log(DistanceTreblinka))+log(DistToCityKM), mig[mig$DistanceTreblinka<=60 & mig$GG==1,],family=quasibinomial)
summary(regInter2) 

stargazer(regM1r, regM2r, regM3r, regInter1, regInter2)

##################################################################################
###### Table A14: Logit Regression, Demographic Characteristics in 1946  #########
##################################################################################
d46<-read.csv("census1946.csv")
head(d46)
#Distance to the nearest railway station is taken from Skorowski, Stanislaw (ed.) 1948. Podzial Administracyjny Rzeczypospolitej Polskiej. Warszawa. Because distance is equal to zero in some cases, we add 1 (km) before taking the logarithm. 

summary(d46$shareMen[d46$DistTreblinka<=50 & d46$type46=="w"])
sd(d46$shareMen[d46$DistTreblinka <=50 & d46$type46=="w"])
d46$PctOver59<-d46$Over59/d46$ogolem
summary(d46$PctOver59[d46$DistTreblinka<=50 & d46$type46=="w"])
sd(d46$PctOver59[d46$DistTreblinka<=50 & d46$type46=="w"])

d46$women<-d46$ogolem-d46$mezczyzni
reg1<-glm(cbind(women, mezczyzni)~log(DistTreblinka)+log(distRailway48+1)+log(ogolem), data=d46[d46$DistTreblinka<=50 & d46$type46=="w",], quasibinomial)
summary(reg1) 

reg1b<-glm(cbind(women, mezczyzni)~log(DistTreblinka)+log(distRailway48+1)+log(ogolem), data=d46[d46$DistTreblinka<=60 & d46$type46=="w",], quasibinomial)
summary(reg1b)

d46$NotOver59<-d46$ogolem -d46$Over59
reg2<-glm(cbind(Over59, NotOver59)~log(DistTreblinka)+log(distRailway48+1)+log(ogolem), data=d46[d46$DistTreblinka<=50 & d46$type46=="w",], quasibinomial)
summary(reg2) 

reg2b<-glm(cbind(Over59, NotOver59)~log(DistTreblinka)+log(distRailway48+1)+log(ogolem), data=d46[d46$DistTreblinka<=60 & d46$type46=="w",], quasibinomial)
summary(reg2b) 

stargazer(reg1, reg1b, reg2, reg2b)

##################################################################################
###### Table A15: Logit Regression, Demographic Characteristics in 1995  #########
##################################################################################

regGender1<-glm(cbind(Men70O,Wom70O) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(regGender1) 

regGender2<-glm(cbind(Men6569, Wom6569) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(regGender2) 

regGender3<-glm(cbind(Men6064, Wom6064) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(regGender3) 

regGender4<-glm(cbind(Men5560, Wom5560) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(regGender4) 

regGender5<-glm(cbind(Men3554, Wom3554) ~log(distTreb), data=crd[crd$distTreb <=50 & crd$miasto==0,], family= quasibinomial)
summary(regGender5) 


stargazer(regGender1, regGender2, regGender3, regGender4,regGender5)

###########################################################
################# Table A16: Placebo Test  ################
###########################################################

LPRPl1<-glm(cbind(LPR2001, NonLPR2001) ~log(PlacDist), data= pol[pol$PlacDist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl1)  

LPRPl1b<-glm(cbind(LPR2001, NonLPR2001) ~log(PlacDist)+log(distRail45)+log(CityDistKm), data=pol[pol$PlacDist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl1b)  

LPRPl2<-glm(cbind(LPR2001, NonLPR2001) ~log(Plac2Dist), data= pol[pol$Plac2Dist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl2) 

LPRPl2b<-glm(cbind(LPR2001, NonLPR2001) ~log(Plac2Dist)+log(distRail45)+log(CityDistKm), data=pol[pol$Plac2Dist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl2b)  

LPRPl3<-glm(cbind(LPR2001, NonLPR2001) ~log(Plac3Dist), data= pol[pol$Plac3Dist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl3)  

LPRPl3b<-glm(cbind(LPR2001, NonLPR2001) ~log(Plac3Dist)+log(distRail45)+log(CityDistKm), data=pol[pol$Plac3Dist <=50 & pol$type!="urban",],quasibinomial)
summary(LPRPl3b)  

stargazer(LPRPl1, LPRPl1b, LPRPl2, LPRPl2b, LPRPl3, LPRPl3b)

###############################################################################
################# Figure A6: Distribution of 20-km Averages  ##################
###############################################################################

##### ANALYSIS FOR HOUSES AND ROOFS
m2<-read.csv("distMatTreblinkaMGm1990s.csv") ##Load matrix of distances between all gminas
head(m2) #
rownames(m2)<-m2[,1]
m2<-m2[,-1] #Remove first column (bc now this info is in rownames)
dim(m2) #139 139
#Get list of communities within 20-km radius for each community
Hbelow20km<-apply(m2, 2, function(x) rownames(m2)[which(x<20000)]) 
Hbelow20km <-Hbelow20km[-which(lengths(Hbelow20km)<6)] ##Cut off groups with low N bc they are on the edge of the radius (min. six units)

### CALCULATE MEANS FOR SHARE DWELLINGS BUILT IN 1945-1970
b20MeanHouses<-sapply(Hbelow20km, function(x) mean(crd$shareDwel[which((crd$nr_statyst %in% x) & crd$miasto==0)]))
m20housesMat<-as.data.frame(cbind(names(b20MeanHouses), b20MeanHouses))
colnames(m20housesMat)<-c("nr_statyst", "mean20kmHouses")
head(m20housesMat)
m20housesMat$nr_statyst<-gsub("X","", m20housesMat$nr_statyst)
m20housesMat2<-merge(as.data.frame(crd[,c(which(names(crd)=="nr_statyst" | names(crd)=="PctBuilt4570" |  names(crd)=="miasto" | names(crd)=="distTreb"))]), m20housesMat, by="nr_statyst")
head(m20housesMat2)
m20housesMat2$mean20kmHouses <-as.numeric(as.character(m20housesMat2$mean20kmHouses))

### FIGURE A6a, TOP LEFT GRAPH in Figue A6
scatter.smooth(m20housesMat2$distTreb[m20housesMat2$distTreb<=50 & m20housesMat2$miasto==0], m20housesMat2$mean20kmHouses[m20housesMat2$distTreb <=50 & m20housesMat2$miasto==0], xlab="Distance to Treblinka, km", ylab="20-km averages", main="Dwellings Built in 1945-70 (N=55)", pch=20)

### CALCULATE MEANS FOR SHARE DWELLINGS WITH METAL ROOFS 
b20MeanRoofs<-sapply(Hbelow20km, function(x) mean(crd$shareBlacha88[which((crd$nr_statyst %in% x) & crd$miasto==0)]))
m20roofsMat<-as.data.frame(cbind(names(b20MeanRoofs), b20MeanRoofs))
colnames(m20roofsMat)<-c("nr_statyst", "mean20kmRoofs")
head(m20roofsMat)
m20roofsMat$nr_statyst<-gsub("X","", m20roofsMat$nr_statyst)
m20roofsMat2<-merge(as.data.frame(crd[,c(which(names(crd)=="nr_statyst" |names(crd)=="PctMetalRoofs" |  names(crd)=="miasto" | names(crd)=="distTreb"))]), m20roofsMat, by="nr_statyst")
head(m20roofsMat2)
m20roofsMat2$mean20kmRoofs <-as.numeric(as.character(m20roofsMat2$mean20kmRoofs))

### FIGURE A6b, TOP RIGHT GRAPH in Figue A6
scatter.smooth(m20roofsMat2$distTreb[m20roofsMat2$distTreb<=50 & m20roofsMat2$miasto==0], m20roofsMat2$mean20kmRoofs[m20roofsMat2$distTreb<=50 & m20roofsMat2$miasto==0], xlab="Distance to Treblinka, km", ylab="20-km averages", main="Dwellings with Metal Roofs (N=55)", pch=20)


##### SAME ANALYSIS FOR LPR VOTE, DIFFERENT FILES BECAUSE ADMIN BOUNDARIES CHANGED
##Load matrix of distances between all gminas
m<-read.csv("distMatTreblinkaM.csv")
rownames(m)<-m[,1]
m<-m[,-1] ##Remove first column, this info is in rownames
dim(m) #129 129
#Get list of communities within 20-km radius for each community
below20km<-apply(m, 2, function(x) rownames(m)[which(x<20000)]) #class list of SP_ID
#Cut off groups with low N bc they are on the edge of the radius (min. six units)
below20km<-below20km[-which(lengths(below20km)<6)] 

#Calculate means for rural units 
b20MeanLPR<-sapply(below20km, function(x) mean(pol$PctLPR2001[which((pol$SP_ID_1 %in% x) & pol$type!="urban")]))
m20mat<-as.data.frame(cbind(names(b20MeanLPR), b20MeanLPR))
colnames(m20mat)<-c("SP_ID_1", "mean20kmLPR")
head(m20mat) #NA's?
m20mat$SP_ID_1<-gsub("X","", m20mat$SP_ID_1)

#subset treblinka for the two main columns and merge with m20mat
m20mat2<-merge(as.data.frame(pol[,c(which(names(pol)=="SP_ID_1" | names(pol)=="CampDistKM" | names(pol)=="type"))]), m20mat, by="SP_ID_1")
head(m20mat2)
m20mat2$mean20kmLPR<-as.numeric(as.character(m20mat2$mean20kmLPR))

### FIGURE A6c, BOTTOM GRAPH in Figue A6
scatter.smooth(m20mat2$CampDistKM[m20mat2$CampDistKM<=50 & m20mat2$type!="urban"], m20mat2$mean20kmLPR[m20mat2$CampDistKM<=50 & m20mat2$type!="urban"], , xlab="Distance to Treblinka, km", ylab="20-km averages", pch=20, main="LPR Vote Choice (N=57)")






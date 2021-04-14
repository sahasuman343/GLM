#' Author: Suman Saha
#' Date: 13/04/21
library(MASS)
library(car)
library(tidyverse)
library(faraway)
library(pROC)
data("birthwt")
attach(birthwt)
dim(birthwt)
str(birthwt)
str(birthwt)
colnames(birthwt)
#[1] "low"   "age"   "lwt"   "race"  "smoke" "ptl"   "ht"    "ui"    "ftv"   "bwt"

birthwt[,c("low","race","smoke","ht","ui")]=lapply(birthwt[,c("low","race","smoke","ht","ui")],factor)
#EDA
  
barplot(table(low),xlab="low",ylab = "frequency")#
barplot(table(race),xlab="Race",ylab = "frequency")
barplot(table(smoke),xlab="Smoke",ylab = "frequency")
barplot(table(ptl),xlab="number of previous premature labours",ylab = "frequency")#
barplot(table(ht),xlab="history of hypertension",ylab = "frequency")
barplot(table(ui),xlab="presence of uterine irritability",ylab = "frequency")
barplot(table(ftv),xlab="number of physician visits during the first trimester",ylab = "frequency")#
options(contrasts = c("contr.treatment", "contr.poly"))

race <- factor(race, labels = c("white", "black", "other"))
barplot(table(bwt$race),xlab="Race",ylab = "frequency")#
table(ptl)
ptd <- factor(ptl > 0)
table(ftv)
ftv <- factor(ftv)
levels(ftv)[-(1:2)] <- "2+"
table(ftv)
bwt <- data.frame(low = factor(low), age, lwt, race,
smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach(); rm(race, ptd, ftv)


#model
mod1<- glm(low ~., family = binomial, data = bwt)
summary(mod1, cor = F)
est_table=cbind.data.frame(names(mod1$coefficients),mod1$coefficients,confint(mod1))
rownames(est_table)=c()
est_table

mod2=glm(low ~ age + lwt + smoke + ptd +race+ ht + ui + ftv + age:ftv+smoke:ui, family = binomial,data=bwt)
summary(mod2)
est_table2=cbind.data.frame(names(mod2$coefficients),mod2$coefficients,confint(mod2))
rownames(est_table2)=c()
est_table2
#Compare Models
anova(mod1,mod2,test = "LR")
Anova(mod2,type = 3,test.statistic = "LR",contrasts=list(age=contr.sum,ui=contr.sum,ftv=contr.sum,smoke=contr.sum))

pred=predict(mod2,type="link")
devres=residuals(mod2)
plot(x=pred,y=devres,xlab = expression(hat(eta)),ylab = "Std Deviance Residual")

bwt <- mutate(bwt, residuals=residuals(mod2), linpred=predict(mod2))
gdf <- group_by(bwt, cut(linpred, breaks=unique(quantile(linpred,(1:10)/11))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
diagdf
plot(residuals ~ linpred, diagdf, xlab="linear predictor")

#check outlier
halfnorm(hatvalues(mod2))
#Predictive diagnostics
confmat=function(model,pi_0=0.5){
  p_hat=predict(model,type="response")
  pred_class=ifelse(p_hat<pi_0,0,1)
  obs_class=model$y
  tp=sum(ifelse(pred_class==1 & obs_class==1,1,0))
  tn=sum(ifelse(pred_class==0 & obs_class==0,1,0))
  fp=sum(ifelse(pred_class==1 & obs_class==0,1,0))
  fn=sum(ifelse(pred_class==0 & obs_class==1,1,0))
  cm=matrix(c(tn,fn,fp,tp),2,2)
  return(cm)
}

cm=confmat(mod2)
cm
mcr=(cm[1,2]+cm[2,1])/(sum(cm))
tcr=1-mcr

tpr=function(cm){cm[2,2]/(cm[2,1]+cm[2,2])}
fpr=function(cm){cm[1,2]/(cm[1,1]+cm[1,2])}

bin=seq(0,1,by=0.1)
TPR=c()
FPR=c()
for( i in seq(along=bin)){
  cm=confmat(mod2,pi_0=bin[i])
  TPR[i]=tpr(cm)
  FPR[i]=fpr(cm)
}
par(mfrow=c(1,1))
plot(FPR,TPR,type="l",ylim = c(0,1),xlim = c(0,1))
abline(0,1,lty=2,col="red")
#AUC
p_hat=predict(mod2,type="response")
pred_class=ifelse(p_hat<0.5,0,1)
acctual_class=as.numeric(bwt$low)
auc(roc(acctual_class,pred_class))





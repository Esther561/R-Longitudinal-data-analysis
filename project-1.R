pd1 <- read.csv(file.choose(),header=TRUE)
names(pd1)
dim(pd1)
str(pd1)

OwnerType1<-NULL
OwnerType1<-(pd1$OwnerType=="ORG")*1
pd2<-cbind(OwnerType1, pd1)

pd<-pd2[,c("prjId","commits","Time","CmtCmnt","OwnerType","OwnerType1")]
str(pd)


head(pd)
tail(pd)


table(pd$OwnerType)
table(pd$Time)

# Calculate statistical summaries -----------------------------------------

# Average (mean)
mean(pd$commits)

# Standard deviation (sd)
sd(pd$commits)

# Median
median(pd$commits)


# Variance
var(pd$commits)
# Minimum and maximum
min(pd$commits) 

max(pd$commits)






#par('mar')
#par(mar = rep(2, 4))



qqnorm(log(pd$commits+1))
qqline(log(pd$commits+1))
hist(log(pd$commits+1))
plot(density(log(pd$commits+1)))


pd10<-pd[1:80,]

library("lattice")
xyplot(commits~Time | prjId, data=pd10, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(-1000, 11000), as.table=T)



pd.USR<-pd[pd$OwnerType=="USR",]
interaction.plot(pd10$Time, pd10$prjId, pd10$commits) 

library(gplots)
plotmeans(pd.USR$commits~ pd.USR$Time, ylab="commits", main="Heterogeineity of commits Over Time", data=pd.USR, lwd = 10, barwidth=5,  n.label=FALSE)


#install.packages("nparLD")
library("nparLD")
myplot <- nparLD(pd$commits~ pd$Time * pd$OwnerType, data = pd, subject = "prjId", description = FALSE)
plot(myplot)





hist(pd$commits)
hist(pd$CmtCmnt)

hist(log(pd$commits+1))
hist(log(pd$CmtCmnt+1))

shapiro.test(pd$commits)
shapiro.test(log(pd$commits+1))



#install.packages("lawstat")
library("lawstat")
levene.test(pd$commits,pd$Time,location=c("mean"))
levene.test(pd$commits,pd$Time,location=c("median"))
levene.test(pd$commits,pd$Time,location=c("trim.mean"))

lmMod <- lm(commits ~Time, data=pd) # initial model

lmMod <- lm(log(commits+1) ~Time, data=pd) # initial model

#residual vs predicted
residualData<-resid(lmMod)
predicted<-predict(lmMod)
plot(predicted,residualData, xlab="Predicted Values", ylab="Residuals", col="red")

#Residual Plot
plot(pd$Time,residualData)
abline(0, 0, col="red")





colSums(is.na(pd2))

colSums(is.na(pd))



#Model A
library(nlme)
model.a <- lme(log(commits+1)~ 1, pd, random= ~1 |prjId)
summary(model.a)

VarCorr(model.a)
mm=VarCorr(model.a)
ICC<-as.numeric(mm[1,1])/(as.numeric(mm[2,1])+as.numeric(mm[1,1]))

#Model B
model.b <- lme(log(commits+1) ~Time  , data=pd, random= ~ Time | prjId, method="ML")
summary(model.b)
mm1=VarCorr(model.b)
print(mm1)


fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + pd$Time[1:8]*fixef.b[[2]]
plot(pd$Time[1:8], fit.b, ylim=c(0, 5), type="b", 
     ylab="predicted commits", xlab="time")   
title("Model B \n Unconditional growth model")

#Model C
model.c <- lme(log(commits+1) ~OwnerType*Time , data=pd, random= ~ Time | prjId, method="ML")
summary(model.c)
mm2=VarCorr(model.c)
print(mm2)
#Pseudo R2
r2<- (as.numeric(mm1[1,1])-as.numeric(mm2[1,1]))/as.numeric(mm1[1,1])
print(r2)


#Model D
model.d <- lme(log(commits+1) ~ log(CmtCmnt+1)*Time+OwnerType*Time , data=pd, random= ~ Time | prjId, method="ML",control = lmeControl(opt = "optim"))
summary(model.d)
VarCorr(model.d)

#Model E
model.e <- lme(log(commits+1) ~ log(CmtCmnt+1)*Time+OwnerType , data=pd, random= ~ Time | prjId, method="ML",control = lmeControl(opt = "optim"))
summary(model.e)
VarCorr(model.e)



#Standard errpr calcultion in r
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/



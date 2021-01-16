### GDP Data Import In R ###
setwd("E:/GDP")
data=read.csv("GDP new final.csv")
View(data)
df=data[,-1]
dim(df)
head(df)
str(df)
y=df[,17]
head(y)
x=df[,-17]
head(x)
### Log trancformation all varibles 
y=log(y)
x=cbind(log(x$x1),log(x$x2),log(x$x3),log(x$x4),log(x$x5),log(x$x6),log(x$x7),
        log(x$x8),log(x$x9),log(x$x10),log(x$x11),log(x$x12),log(x$x13),log(x$x14),
        log(x$x15),log(x$x16))
head(x)
x=as.data.frame.matrix(x)
summary(x)
x$V14=ifelse(is.na(x$V14),median(x$V14,na.rm = T),x$V14)
summary(x)

sum(is.na(x))
df1=data.frame(y,x)

model=lm(y~.,data = df1)
summary(model)
### Normality check 
shapiro.test(residuals(model))# H0: data is normal

plot(data[,1],data[,18],ylab="GDP",xlab = "Year")
hist(y)

# To check Autocorrelation 
library(zoo)
library(lmtest)
acf(y)
dwtest(model) # Ho: data is no autocorrelation
# above check the dwtest and H0 is reject i.e in data autocorrelatin.
bgtest(model)
acf(data[,18])
# To remove autocorrelation 
## AR(1) model lag=1
e=model$residuals
e=as.vector(e);e
e1=e[1:58]
e2=e[2:59]
d=(sum(e1^2)+sum(e2^2)-(2*sum(e1*e2)))/(sum(e^2))
d
rho=sum(e1*e2)/sum(e^2)
rho
x=as.vector(x)
class(x)

yone=y[1]*sqrt(1-rho^2)
y_1=y[2:59]-rho*y[1:58]
y_1=append(y_1,yone,after = 0)
end(y_1)
acf(y_1)
z=rho*x[1:58,1:16]
w=x[2:59,1:16]
x_11=x[1,1:16]*sqrt(1-rho^2)
x_12=w-z

x_1=rbind(x_11,x_12)
head(x_1)
dim(x_1)

data1=data.frame(y_1,x_1)
model1=lm(y_1~.,data = data1)
summary(model1)
dwtest(model1)
## AR(2) model lag=2e=model$residuals
e_1=model1$residuals;e_1
sum(is.na(e_1))
e_1=as.vector(e_1);e_1
e11=e_1[1:58];e11
e21=e_1[2:59];e21
d=(sum(e11^2)+sum(e21^2)-(2*sum(e11*e21)))/(sum(e_1^2))
d
rho1=sum(e11*e21)/sum(e_1^2)
rho1
#x=as.vector(x)
#class(x)
y_1one=y_1[1]*sqrt(1-rho1^1)
y_11one=y_1[2:59]-rho1*y_1[1:58]
y_11=append(y_11one,y_1one,after = 0)
end(y_11)
acf(y_11)
x_one=x_1[1,1:16]*sqrt(1-rho1^2)
z1=rho1*x_1[1:58,1:16]
w1=x_1[2:59,1:16]
x_1one=w1-z1
dim(x_1one)
x_11=rbind(x_one,x_1one)
dim(x_11)
data2=data.frame(y_11,x_11)
dim(data2)
model2=lm(y_11~.,data = data2)
summary(model2)
dwtest(model2)


###Again AR(3)
#e_11=model2$residuals;e_11
#sum(is.na(e_11))
#e_11=as.vector(e_11);e_11
#e111=e_11[1:57];e111
#e211=e_11[2:58];e211
#d2=(sum(e111^2)+sum(e211^2)-(2*sum(e111*e211)))/(sum(e_11^2))
#d2
#rho3=sum(e111*e211)/sum(e_11^2)
#rho3
#x=as.vector(x)
#class(x)
#y_111one=y_11[1]*sqrt(1-rho3^2)
#y_111=y_11[2:58]-rho3*y_11[1:57]
#y_111=append(y_111,y_111one,after = 0)
#end(y_111)
#acf(y_111)
#z2=rho3*x_11[1:57,1:16]
#w2=x_11[2:58,1:16]
#x_121=w2-z2
#x_11one=x_11[1,1:16]*sqrt(1-rho3^2)
#x_111=rbind(x_11one,x_121)
#dim(x_111)
#data3=data.frame(y_111,x_111)
#model3=lm(y_111~.,data = data3)
#summary(model3)
#dwtest(model3)
### Check Heteroscedasticity

library(lmtest)
gqtest(model2)
#white.test(x_11,y_11)# H0:data is homoscedasticity

### To check multicollinearity 

library("faraway")
faraway::vif(model2) # The VIF > 5. i.e there is multicollinearity
# above data are violate the problem of multicollinearity.
# To remove the multicollinearity we use Principal Component Analysis



###########################################################################
### Principal Component Analysis ###
str(data2)
R=round(cor(data2[,-1]),2)
R
a=as.matrix((data2[,-1]))
head(a)
dim(a)
pc<-princomp(a)
summary(pc)
screeplot(pc,type = "line")

E=eigen(R)
head(E)
end(E)
EV=round(E$values,3)
EV
end(EV)

for(i in 1:3)
{
  var=(EV/(sum(EV)))*100
}
var
CV=cumsum(var)
CV
wt=E$vectors
head(wt)
dim(wt)
comp=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
comp
df=data.frame(comp,round(EV,3),round(var,3),round(CV,3))
df
### THe above first five princ comp explains more variability then we select first five PC's.
## Using eigen value critea (EV>0.7)
wt=data.frame(wt)
head(wt)
head(a)
dim(a)
Z=a%*%as.matrix(wt[1:16,1:2])
head(Z)
dim(Z)
Y=data2[,1]
end(Y)
data3=data.frame(Y,Z)
str(data3)
head(data3)
#data2=data.frame(Y,Z123)

lm.fit=lm(Y~.,data=data3)
lm.fit
sum=summary(lm.fit)   ## from here we can see the significance of the PC's
sum
betas=t(as.vector(sum$coefficients[,1]))

intercept=betas[1];intercept
beta=betas[-1];beta

##### to find the final coefficient of real variables
beta=as.matrix(beta)
betan=as.matrix(wt[,1:2])%*%(beta)
betan



















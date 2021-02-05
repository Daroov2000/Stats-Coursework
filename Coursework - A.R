# Question 1
attach(protein)

# Part a
lapply(protein[2:10], summary)

#Part b
par(mfrow=c(3,3))
par(mar = c(5, 5, 2, 1))
lapply(2:10,function(x) hist(protein[,x],main=NULL,xlab=colnames(protein[x])))

# Part c
cor(Fr.Veg,protein[,2:9])

#Part d
par(mfrow=c(3,3))
par(mar = c(5, 5, 2, 1))
lapply(2:9,function(x) plot(Fr.Veg, protein[,x],main=NULL,ylab=colnames(protein[x])))

# Part e
ci<-function(x,data){
  t<-t.test(data[,x], alternative="t", mu=mean(data[,x]),conf.level=0.95,var.equal=TRUE)
  paste('95% Confidence Interval for',colnames(data[x]),'is',t$conf.int[1],',',t$conf.int[2])}
lapply(2:10, function(x) ci(x,protein))

# Part f
t.test(Starch,Nuts,alternative="g", mu=0,paired=TRUE,conf.level=0.95,var.equal=TRUE)

par(mfrow=c(1,1))
qqnorm(Starch, pch = 1, frame = TRUE)
qqline(Starch, col = "steelblue", lwd = 2)
shapiro.test(Starch)

qqnorm(Nuts, pch = 1, frame = TRUE)
qqline(Nuts, col = "steelblue", lwd = 2)
shapiro.test(Nuts)

fligner.test(Starch, Nuts)
var.test(Starch,Nuts,ratio=1, alternative="t")

# Question 2
attach(DartPoints)

# Part a - Text

# Part b
par(mar = c(5,4,1,1))
layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,0,3,3,3,3,3,0,4,4,4,4,4,4,4,0,
                5,5,5,5,5,5,5,0,0,6,6,6,6,6,6,6,0,7,7,7,7,7,7,7,0),3, 17, byrow=TRUE))
lapply(c(2,10:15),function(x) boxplot(Length~DartPoints[,x],xlab=colnames(DartPoints[x])))

par(mfrow=c(2,3))
par(mar = c(5, 5, 1, 1))
lapply(4:9,function(x) plot(Length,DartPoints[,x],ylab=colnames(DartPoints[x])))

# Part c
cor(Length,DartPoints[,4:9],use='complete.obs')

#Part d
par(mfrow=c(2,2))
par(mar = c(5, 5, 3, 2))
breaks <- c(2,4,6,8,10,12,14,16,18,30)
tags <- c("[2-4)", "[4-6)", "[6-8)", "[8-10)", "[10-12)","[12-14)", "[14-16)","[16-18)", "[18-30)")

E<-DartPoints[which(Blade.Sh == 'E'),]
group_tagsE <- cut(E$Weight, breaks=breaks,include.lowest=TRUE, right=FALSE, labels=tags)
barplot(table(group_tagsE)/length(E$Weight),main="Blade Shape E",xlab='Weight',ylab="Relative Frequency",ylim=c(0,0.3))
I<-DartPoints[which(Blade.Sh == 'I'),] 
group_tagsI <- cut(I$Weight, breaks=breaks,include.lowest=TRUE, right=FALSE, labels=tags)
barplot(table(group_tagsI)/length(I$Weight),main="Blade Shape I",xlab='Weight',ylab="Relative Frequency",ylim=c(0,0.8))
R<-DartPoints[which(Blade.Sh == 'R'),] 
group_tagsR <- cut(R$Weight, breaks=breaks,include.lowest=TRUE, right=FALSE, labels=tags)
barplot(table(group_tagsR)/length(R$Weight),main="Blade Shape R",xlab='Weight',ylab="Relative Frequency",ylim=c(0,0.8))
S<-DartPoints[which(Blade.Sh == 'S'),] 
group_tagsS <- cut(S$Weight, breaks=breaks,include.lowest=TRUE, right=FALSE, labels=tags)
barplot(table(group_tagsS)/length(S$Weight),main="Blade Shape S",xlab='Weight',ylab="Relative Frequency",ylim=c(0,0.4))

# Part e
summary(DartPoints)

DP1<-DartPoints[c(1:27,29:37,39:67,69:91),c(2:5,7:15)]
summary(DP1)

library(MASS)
fit <- lm(Weight ~.,DP1)
step <- stepAIC(fit, direction="both")

anova(step)
summary(step)

#Part f
par(mfrow=c(2,2))
plot(step)

#Part g
summary(step)

#Part h
options(warn=-1)
predict(step,data.frame(Length=70,Width=60,Thickness=50,Blade.Sh='R',Should.Sh='S',
                        Should.Or='B',Haft.Or='P'),interval='prediction')

range(Width)
range(Thickness)

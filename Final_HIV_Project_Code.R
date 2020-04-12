# STP 429: Project #1
# Brandon Vermeer, Carson Mohr, Corey Coole, James Altman, Danny Breyfogle
# Spring 2017

# Prep data and create aggregrate
df <- read.delim("compressed_mortality.txt")
df <- df[,!names(df)%in%c("Notes","Crude.Rate","Year.Code","Age.Group.Code")]
df <- df[!(df$Population=="Not Applicable"&df$Age.Group=="Not Stated"),]
df$HIV <- ifelse(df$ICD.Sub.Chapter.Code=="B20-B24",1,0)
aggdf <- aggregate(df$Deaths,list(df$Population,df$Year,df$Age.Group,df$HIV),sum)
names(aggdf) <- c("Population","Year","Age.Group","HIV","Deaths")
aggdf$Population <- as.numeric(paste(aggdf$Population))
aggdf$Rate <- (aggdf$Deaths/aggdf$Population)*100000
aggdf$Rate.Trans <- sqrt(aggdf$Rate)

# Create two seperate data sets for HIV & non-HIV cases
aggdf.HIV <- aggdf[which(aggdf$HIV==1),]
aggdf.nonHIV <- aggdf[which(aggdf$HIV==0),]

# Generate models
add.mod.HIV <- lm(aggdf.HIV$Rate~aggdf.HIV$Year+aggdf.HIV$Age.Group)
int.mod.HIV <- lm(aggdf.HIV$Rate~aggdf.HIV$Year+aggdf.HIV$Age.Group+aggdf.HIV$Year*aggdf.HIV$Age.Group)
add.mod.nonHIV <- lm(aggdf.nonHIV$Rate~aggdf.nonHIV$Year+aggdf.nonHIV$Age.Group)
int.mod.nonHIV <- lm(aggdf.nonHIV$Rate~aggdf.nonHIV$Year+aggdf.nonHIV$Age.Group+aggdf.nonHIV$Year*aggdf.nonHIV$Age.Group)

# Transformed models
add.mod.HIV.Trans <- lm(aggdf.HIV$Rate.Trans~aggdf.HIV$Year+aggdf.HIV$Age.Group)
int.mod.HIV.Trans <- lm(aggdf.HIV$Rate.Trans~aggdf.HIV$Year+aggdf.HIV$Age.Group+aggdf.HIV$Year*aggdf.HIV$Age.Group)
add.mod.nonHIV.Trans <- lm(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year+aggdf.nonHIV$Age.Group)
int.mod.nonHIV.Trans <- lm(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year+aggdf.nonHIV$Age.Group+aggdf.nonHIV$Year*aggdf.nonHIV$Age.Group)

# For plotting purposes...
Age.Groups <- as.vector(unique(aggdf.HIV$Age.Group))
Age.Groups <- Age.Groups[c(1:2,9,3:8,10:13)]
par(mar=c(5,4,4,13),xpd=T)

# Plot additive model for HIV by age group
yhat.add.HIV <- predict(add.mod.HIV)
plot(aggdf.HIV$Rate~aggdf.HIV$Year,pch="",ylim=c(-1.5,7.5),main="HIV Death Rates For Younger Age Groups (Additive Model)",xlab="Year",ylab="Rate")
for (i in 1:7) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.add.HIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.HIV$Rate~aggdf.HIV$Year,pch="",ylim=c(-0.5,14.5),main="HIV Death Rates For Older Age Groups (Additive Model)",xlab="Year",ylab="Rate")
for (i in 8:13) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.add.HIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot interactive model for HIV by age group
yhat.int.HIV <- predict(int.mod.HIV)
plot(aggdf.HIV$Rate~aggdf.HIV$Year,pch="",ylim=c(0,7.5),main="HIV Death Rates For Younger Age Groups (Interactive Model)",xlab="Year",ylab="Rate")
for (i in 1:7) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.int.HIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.HIV$Rate~aggdf.HIV$Year,pch="",ylim=c(0,15),main="HIV Death Rates For Older Age Groups (Interactive Model)",xlab="Year",ylab="Rate")
for (i in 8:13) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.int.HIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot additive model for non-HIV by age group
yhat.add.nonHIV <- predict(add.mod.nonHIV)
plot(aggdf.nonHIV$Rate~aggdf.nonHIV$Year,pch="",ylim=c(-200,850),main="Non-HIV Death Rates For Younger Age Groups (Additive Model)",xlab="Year",ylab="Rate")
for (i in 1:7) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.add.nonHIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.nonHIV$Rate~aggdf.nonHIV$Year,pch="",ylim=c(0,16000),main="Non-HIV Death Rates For Older Age Groups (Additive Model)",xlab="Year",ylab="Rate")
for (i in 8:13) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.add.nonHIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot interactive model for non-HIV by age group
yhat.int.nonHIV <- predict(int.mod.nonHIV)
plot(aggdf.nonHIV$Rate~aggdf.nonHIV$Year,pch="",ylim=c(0,800),main="Non-HIV Death Rates For Younger Age Groups (Interactive Model)",xlab="Year",ylab="Rate")
for (i in 1:7) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.int.nonHIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.nonHIV$Rate~aggdf.nonHIV$Year,pch="",ylim=c(0,16000),main="Non-HIV Death Rates For Older Age Groups (Interactive Model)",xlab="Year",ylab="Rate")
for (i in 8:13) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.int.nonHIV[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot transformed additive model for HIV by age group
yhat.add.HIV.Trans <- predict(add.mod.HIV.Trans)
plot(aggdf.HIV$Rate.Trans~aggdf.HIV$Year,pch="",ylim=c(0,3),main="HIV Death Rates For Younger Age Groups (Square Root Transform, Additive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 1:7) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.add.HIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.HIV$Rate.Trans~aggdf.HIV$Year,pch="",ylim=c(0,4),main="HIV Death Rates For Older Age Groups (Square Root Transform, Additive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 8:13) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.add.HIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot transformed interactive model for HIV by age group
yhat.int.HIV.Trans <- predict(int.mod.HIV.Trans)
plot(aggdf.HIV$Rate.Trans~aggdf.HIV$Year,pch="",ylim=c(0,3),main="HIV Death Rates For Younger Age Groups (Square Root Transform, Interactive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 1:7) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.int.HIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.HIV$Rate.Trans~aggdf.HIV$Year,pch="",ylim=c(0,4),main="HIV Death Rates For Older Age Groups (Square Root Transform, Interactive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 8:13) {
  ii <- which(aggdf.HIV$Age.Group==Age.Groups[i])
  points(aggdf.HIV$Year[ii],aggdf.HIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.HIV$Year[ii],yhat.int.HIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot transformed additive model for non-HIV by age group
yhat.add.nonHIV.Trans <- predict(add.mod.nonHIV.Trans)
plot(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year,pch="",ylim=c(0,30),main="Non-HIV Death Rates For Younger Age Groups (Square Root Transform, Additive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 1:7) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.add.nonHIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year,pch="",ylim=c(0,130),main="Non-HIV Death Rates For Older Age Groups (Square Root Transform, Additive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 8:13) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.add.nonHIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)

# Plot transformed interactive model for non-HIV by age group
yhat.int.nonHIV.Trans <- predict(int.mod.nonHIV.Trans)
plot(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year,pch="",ylim=c(0,30),main="Non-HIV Death Rates For Younger Age Groups (Square Root Transform, Interactive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 1:7) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.int.nonHIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[1:7],col=2:8,pch=16)
plot(aggdf.nonHIV$Rate.Trans~aggdf.nonHIV$Year,pch="",ylim=c(0,130),main="Non-HIV Death Rates For Older Age Groups (Square Root Transform, Interactive Model)",xlab="Year",ylab="Transformed Rate")
for (i in 8:13) {
  ii <- which(aggdf.nonHIV$Age.Group==Age.Groups[i])
  points(aggdf.nonHIV$Year[ii],aggdf.nonHIV$Rate.Trans[ii],col=i+1,pch=16)
  lines(aggdf.nonHIV$Year[ii],yhat.int.nonHIV.Trans[ii],col=i+1,lty=1)
}
legend("right",inset=c(-0.76,0),bty="n",Age.Groups[8:13],col=9:14,pch=16)
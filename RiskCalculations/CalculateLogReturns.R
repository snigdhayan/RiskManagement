library(quantmod)

# Get data from FRED and preprocess it
myWilsh <- getSymbols("WILL5000IND",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("DEXUSUK",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("DEXJPUS",src="FRED",auto.assign=FALSE)
myWilsh <- na.omit(myWilsh)
#myWilsh$DEXJPUS <- 1/myWilsh$DEXJPUS
#wilsh <- myWilsh["1979-12-31/2017-12-31"]
wilsh <- myWilsh["1979-12-31/2020-12-31"]
names(wilsh) <- "TR"

# Calculate daily log returns and discrete returns
logret <- diff(log(wilsh))
head(logret,3) 

logret <- diff(log(wilsh))[-1]
round(head(logret,3),6) 

ret <- exp(logret) - 1
round(tail(ret,3),6)


# Calculate weekly, monthly, quarterly and yearly versions thereof
logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)

ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1

# Calculate mean and standard deviation
mu <- mean(logret)
sig <- sd(logret)

# Calculate value at risk (VaR)
confidence <- 0.99
significance <- 1 - confidence
var <- qnorm(significance,mu,sig)
HFvar <- 1000 * ( exp(var)-1 ) 

# Calculate expected shortfall (ES)
es <- mu-sig*dnorm(qnorm(significance,0,1),0,1)/significance
HFes <- 1000 * ( exp(es)-1 )

# Calculate VaR and ES via simulation assuming normality - Method 1
RNGkind(sample.kind= "Rounding")
set.seed(123789)
rvec <- rnorm(100000,mu,sig)
var_sim1 <- quantile(rvec,significance)
es_sim1 <- mean(rvec[rvec<var_sim1])

# Calculate VaR and ES via simulation not assuming normality - Method 2
RNGkind(sample.kind= "Rounding")
set.seed(123789)
rvec <- sample(as.vector(logret),100000,replace=TRUE)
var_sim2 <- quantile(rvec,significance)
es_sim2 <- mean(rvec[rvec<var_sim2])

# Check normality of log returns
library(moments)
rvec <- as.vector(logret)
skew <- skewness(rvec)
kurt <- kurtosis(rvec)
JB <- jarque.test(rvec)

# Fit rescaled t-distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)

# Calculate VaR and ES via simulation assuming rescaled t-distribution - Method 3
RNGkind(sample.kind= "Rounding")
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
var_sim3 <- quantile(rvec,significance)
es_sim3 <- mean(rvec[rvec<var_sim3])

# Calculate VaR and ES for long term horizon
horizon <- 10

# Method 1 - direct sampling from rescaled t-distribution (ignoring the time dependence)
RNGkind(sample.kind= "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:horizon) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
var_sim4 <- quantile(rvec,significance)
es_sim4 <- mean(rvec[rvec<var_sim4]) 

# Method 2 - Identically Independently Distributed (the days in a block are not necessarily consecutive)
RNGkind(sample.kind= "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:horizon) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
var_sim5 <- quantile(rvec,significance)
es_sim5 <- mean(rvec[rvec<var_sim5])

# Method 3 - independent block (blocks are independent but the days in a block are consecutive)
RNGkind(sample.kind= "Rounding")
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-(horizon-1),by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:horizon) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
var_sim6 <- quantile(rvec,significance)
es_sim6 <- mean(rvec[rvec<var_sim6])

# Find autocorrelation function of log returns
acf_logret <- acf(logret)
acf_abs_logret <- acf(abs(logret))
plot(acf_abs_logret)


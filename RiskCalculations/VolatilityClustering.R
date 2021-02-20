library(quantmod)

# Get data from FRED and preprocess data
myWilsh <- getSymbols("WILL5000IND",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("DEXUSUK",src="FRED",auto.assign=FALSE)
#myWilsh <- getSymbols("DEXJPUS",src="FRED",auto.assign=FALSE)
myWilsh <- na.omit(myWilsh)
#myWilsh$DEXJPUS <- 1/myWilsh$DEXJPUS
#wilsh <- myWilsh["1979-12-31/2017-12-31"]
wilsh <- myWilsh["1979-12-31/2020-12-31"]
names(wilsh) <- "TR"

# Calculate its daily log returns
logret <- diff(log(wilsh))[-1]

# Graph the autocorrelation function of log returns
acf(logret)

# Graph the autocorrelation function of |log returns|
acf(abs(logret))

# Estimate the GARCH(1,1) –t model using the “rugarch” package
library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])

# The estimated parameters are in
fit.garch@fit$coef

# Save the output of the estimation
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c("logret","s","z")

# Examine the acf of the “z” column to check if the GARCH model has captured volatility clustering
acf(save1$z)
acf(abs(save1$z))

# Use the function “ugarchboot” to simulate 1-day outcomes
RNGkind(sample.kind= "Rounding")
set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp")

# Save the simulated outcomes in the vector rvec
rvec <- boot.garch@fseries

# Calculate the VaR and ES at the 95% confidence level
confidence = 0.95
significance = 1 - confidence
VaR <- quantile(rvec,significance)
ES <- mean(rvec[rvec<VaR])

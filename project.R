rm(list=ls())

library(fitdistrplus)
library(ggplot2)
library(gtools)
library(leaps)
library(effects)

dowfile = "~/Desktop/525/project/dow.csv"

df = read.csv(dowfile)
tickersplit = split(df, dowdf$TICKER)
par(mfrow=c(6,5))
tickerplot = function(ticker) {
  ret = tickersplit[[ticker]]$RET
  fitnorm<-fitdist(ret,"norm")
  qqcomp(list(fitnorm), main=paste(ticker," AIC:",round(fitnorm$aic, 2)))
  print(summary(fitnorm))
  print(ticker)
}
#just the 30 dow component names, not the Dow itself
tickers = unique(df$TICKER)
tickers = tickers[tickers != "DIA"]
tickers = as.character(tickers)

# A QQ plot of all the 30 Dow components
sapply(tickers, tickerplot)


# a baseline QQ plot for comparison, with 251 gaussian returns
par(mfrow=c(1,1))
fitnorm<-fitdist(rnorm(251),"norm")
qqcomp(list(fitnorm), main=paste("Gaussian Returns"," AIC:",round(fitnorm$aic, 2)))

#make a matrix with 31 columns, 251 rows = 251 trading days
# first column is DIA ie. Dow Jones index
# rest 30 columns are the 30 DOW component stocks
dow = matrix(0,nrow=251,ncol=31)

#populate the matrix
dow[,1]=tickersplit[["DIA"]]$RET
for(i in 2:31) {
  myticker = tickers[i-1]
  dow[,i] = tickersplit[[myticker]]$RET
}
colnames(dow) <- c("DIA", tickers)

# also make a datafame, since some functions in R don't operate on matrices
dowdf = data.frame(dow) 

#variable selection
forward_varsec = summary(regsubsets(x=x,y=dow[,1],method="forward", nbest=1,nvmax=30, all.best=FALSE))
backward_varsec = summary(regsubsets(x=x,y=dow[,1],method="backward", nbest=1,nvmax=30,all.best=FALSE))

#display subset of selected covariates 
forward_varsec$outmat
backward_varsec$outmat

# Display increasing Rsquare, as more & more covariates are included in model
plot(1:30, forward_varsec$rsq)

# As seen from the plot, 
# we get about 90% Rsq from just 3 stocks, 
# Lets build a linear model to predict Dow Jones Returns,
# from the 3 best (forward selection) predictors
mod1 = lm(dow[,"DIA"]~dow[,"MMM"]+dow[,"BA"]+dow[,"V"])
summary(mod1)
anova(mod1)
#residual plot & diagnostics ( lecture notes Chp 11)
plot(mod1)

# Now, lets see if we can construct a portfolio with similar returns as the Dow
# We will use the coefficients from the linear model
myportfolio = 0.32*dow[, "MMM"] + 0.17*dow[, "BA"]  + 0.26*dow[, "V"]

# So we have constructed a portfolio with only three companies, 3M, Boeing & Visa.
# Lets add myporfolio to the dataframe & plot its returns alongwith the Dow over 2018
dowdf$myportfolio = myportfolio
ggplot(dowdf, aes(x=1:251, y=dowdf$DIA)) + geom_smooth(method="loess", span = 0.5, color="blue") + geom_smooth(span=0.6, data=dowdf, aes(x=1:251, y=dowdf$DIA), color="red")

#So we see the Dow in blue tracks the returns of myportfolio in red very closely.
#In fact, if we match the span of the 2 loess curves, we cannot distinguish beween the 2 curves!
# We are able to visualize them apart only because the 2 loess curves have different spans.

#Nondiversification vs Diversification
# First person buys 1 stock, IBM
# Second person buys all 30 stocks ie. DIA index
# They both hold it for a week
# Lets compare their returns
n = 7 #1 week
returns = matrix(0, nrow=n*2, ncol=2)
colnames(returns) <- c("Returns", "Portfolio")
returns[,1] = c(dow[, "IBM"][1:n], dow[, "DIA"][1:n])
returns[,2] = c(rep(1, n),rep(2, n))
returns = data.frame(returns)
returns$Returns = as.double(as.character( returns$Returns ))
returns$Portfolio = factor( returns$Portfolio )
res.aov <- aov(Returns~Portfolio, data = returns)
summary(res.aov)
#plot(res.aov)
TukeyHSD(res.aov)
boxplot(Returns~Portfolio, data = returns)
# Tukey vs Welch's t test since homogaenity of variance not met
t.test(returns$Returns[returns$Portfolio=="1"],returns$Returns[returns$Portfolio=="2"])

# Cell means model
lm1 = lm(returns$Returns~returns$Portfolio - 1)
summary(lm1)
# REgular linear model
lm2 = lm(Returns~Portfolio,data=returns)
# effects plot
plot(allEffects(lm2))
# mean only model
lm3 <- lm(Returns~1,data=returns)
summary(lm3)
#compare
anova(lm2,lm3)


# Lets now construct 10 combinations of 3 Dow stocks, and see if the mean returns are statistically different
# Each portfolio is a linear combination of 3 different Dow stocks
# 5 choose 3 = 10
# 1-way anova lect notes 12
threecomb = combinations(5,3,tickers)
nc3 = 10
returns = matrix(0, nrow=251*nc3, ncol=2)
colnames(returns) <- c("Returns", "Portfolio")

s = 1
for(i in 1:nc3) {
  threetickers = threecomb[i,]
  ticker1 = threetickers[1]
  ticker2 = threetickers[2]
  ticker3 = threetickers[3]
  # portfolio is simple linear combination of 3 tickers
  portfolio = dow[, ticker1] + dow[, ticker2] + dow[, ticker3]
  returns[s:(s+250),1] = portfolio
  returns[s:(s+250),2] = rep(i, 251)
  s = s + 251
}

returns = data.frame(returns)
returns$Returns = as.double(as.character( returns$Returns ))
returns$Portfolio = factor( returns$Portfolio )
res.aov <- aov(Returns~Portfolio, data = returns)
summary(res.aov)
plot(res.aov)
TukeyHSD(res.aov)
boxplot(Returns~Portfolio, data = returns)
plot(allEffects(lm(Returns~Portfolio,data=returns)))












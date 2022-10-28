#Waite First Securities | Exploratory Data Analysis - Statistical Tables

rm = (list = ls(all = TRUE)) #clears all memory
options(digits=12) #keeps 12 digits in memory, otherwise summary statistics may be off due to rounding

#Set working directory by clicking on Session --> set working directory --> to source file location

stockdata <- read.csv("WFS_Stockdata.csv", header=TRUE)  # imports and renames dataset
stockdata <- stockdata[-1,] #removes Row containing Dec 2014 - NA 
colnames(stockdata) # prints column names on the screen
head(stockdata)

round(summary(stockdata[,"GSPC_percent_return"]), 1)
round(summary(stockdata[,"AAPL_percent_return"]), 1)
round(summary(stockdata[,"INTC_percent_return"]), 1)
round(summary(stockdata[,"KR_percent_return"]), 1)

round(mean(stockdata[,"GSPC_percent_return"], na.rm = TRUE), 3)
round(mean(stockdata[,"AAPL_percent_return"], na.rm = TRUE), 3)
round(mean(stockdata[,"INTC_percent_return"], na.rm = TRUE), 3)
round(mean(stockdata[,"KR_percent_return"], na.rm = TRUE), 3)

#Standard Deviations
round(sqrt(var(stockdata[,"GSPC_percent_return"], na.rm = TRUE)), 3)
round(sqrt(var(stockdata[,"AAPL_percent_return"], na.rm = TRUE)), 3)
round(sqrt(var(stockdata[,"INTC_percent_return"], na.rm = TRUE)), 3)
round(sqrt(var(stockdata[,"KR_percent_return"], na.rm = TRUE)), 3)

#correlation coefficient
round(cor(stockdata$GSPC_percent_return, stockdata$AAPL_percent_return), 2)
round(cor(stockdata$GSPC_percent_return, stockdata$INTC_percent_return), 2)
round(cor(stockdata$GSPC_percent_return, stockdata$KR_percent_return), 2)

 
#Scatterplots
#install.packages("tidyverse") #install tidyverse --only need to do this once
library(tidyverse) #load tidyverse package -- need to load every time

ggplot(data = stockdata) + geom_point(mapping = aes(x = GSPC_percent_return, y = AAPL_percent_return)) 
ggplot(data = stockdata) + geom_point(mapping = aes(x = GSPC_percent_return, y = INTC_percent_return))
ggplot(data = stockdata) + geom_point(mapping = aes(x = GSPC_percent_return, y = KR_percent_return))


#APPLE

#Extract variables to be used in the analyses
SNP500_returns <- stockdata[,"GSPC_percent_return"] #no number before comma means select all rows
Apple_returns <- stockdata[,"AAPL_percent_return"]

#Regress Apple stock returns on SNP500 stock returns
mod.1 <- lm(Apple_returns ~ SNP500_returns)
#Present Parameter Estimates, Coefficient of Determination, etc.
summary(mod.1)

# Plot data and regression line
plot(SNP500_returns, Apple_returns)
abline(mod.1, col="red")

#INTEL

#Extract variables to be used in the analyses
SNP500_returns <- stockdata[,"GSPC_percent_return"] #no number before comma means select all rows
INTC_returns <- stockdata[,"INTC_percent_return"]

#Regress Intel stock returns on SNP500 stock returns
mod.2 <- lm(INTC_returns ~ SNP500_returns)
#Present Parameter Estimates, Coefficient of Determination, etc.
summary(mod.2)

# Plot data and regression line
plot(SNP500_returns, INTC_returns)
abline(mod.2, col="red")

#KROGER

#Extract variables to be used in the analyses
SNP500_returns <- stockdata[,"GSPC_percent_return"] #no number before comma means select all rows
KR_returns <- stockdata[,"KR_percent_return"]

#Regress Kroger stock returns on SNP500 stock returns
mod.3 <- lm(KR_returns ~ SNP500_returns)
#Present Parameter Estimates, Coefficient of Determination, etc.
summary(mod.3)

# Plot data and regression line
plot(SNP500_returns, KR_returns)
abline(mod.3, col="red")

#Geometric Mean Return for S&P 500
str(stockdata$GSPC.Stock.Return) #Confirm that variable called Return is numeric

#Create a new variable that represents the (return in month t) + 1 
newstockdata <- transform(stockdata,GSPC.Stock.Returnplus1 = GSPC.Stock.Return + 1)

#value of geometric mean shows up in the global environments
geomeanreturn <- exp(mean(log(newstockdata$GSPC.Stock.Returnplus1))) - 1

#Geometric Mean Return for Apple
str(stockdata$AAPL.Stock.Return)
newstockdata <- transform(stockdata,AAPL.Stock.Returnplus1 = AAPL.Stock.Return + 1)
geomeanreturn <- exp(mean(log(newstockdata$AAPL.Stock.Returnplus1))) - 1

#Geometric Mean Return for Intel
str(stockdata$INTC.Stock.Return)
newstockdata <- transform(stockdata,INTC.Stock.Returnplus1 = INTC.Stock.Return + 1)
geomeanreturn <- exp(mean(log(newstockdata$INTC.Stock.Returnplus1))) - 1

#Geometric Mean Return for Kroger
str(stockdata$KR.Stock.Return)
newstockdata <- transform(stockdata,KR.Stock.Returnplus1 = KR.Stock.Return + 1)
geomeanreturn <- exp(mean(log(newstockdata$KR.Stock.Returnplus1))) - 1





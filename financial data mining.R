library(tidyquant)
library(tidyverse)
library(ggplot2)
library(latticeExtra)
library(TTR)

require(quantmod)

#Getting the prices of the stock
stock_prices <-tq_get("SPY", get = "stock.prices",
         from = "1994-01-01",
         to = "2020-04-01")

#Getting the Ra of the stock
stock_returns_monthly <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")
Ra<-stock_returns_monthly[,3]


str1<-vector("double", 26)
amt1<-vector("double", 26)
p<-1000
j<-0
l<-0
x<-0
sum<-0
for(i in 1:nrow(Ra))
{
  j=j+1

  r=Ra[i,1]*p
  p=r+p
  if(j==12)
  {
    #Finding the yearly Ra 
    x<-x+1
    l<-l+1
    j<-0
    str1[[l]]<-p
    amt1[[l]]<-p-x*12000-sum
    sum=sum+amt1[[l]]
  }
  p<-p+1000
}

print(p)

amt1<-unlist(amt1)
years<-c(1994:2019)
#plotting the yearly Ra for Strategy 1
plot(years,amt1,ylim=c(-119000,290000),main=" Returns of Strategy 1 for 26 years", xlab="Years",ylab="Returns Value")






stock_prices <-tq_get("SPY", get = "stock.prices",
                      from = "1993-03-01",
                      to = "2020-04-01")

sma_value<-SMA(stock_prices[,6],n=200)

stock_prices<-cbind(stock_prices,sma_value)


stock_prices<-stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select     = open:sma_value, 
               mutate_fun = to.period, 
               period     = "months")

str2<-vector("double", 26)
sma<-stock_prices[,9]
stock_close<-stock_prices[,6]
stock_amt<-0
savings_amt<-0
x<-0
j<-0
l<-0
sum<-0
for(i in 1:nrow(stock_close))
{
  j<-j+1
  if(stock_close[i,1]>sma[i,1])
  {
    stock_amt=stock_amt+savings_amt+1000
    savings_amt=0
    r=Ra[i,1]*stock_amt
    stock_amt=r+stock_amt
    print(paste("stock_amt=",stock_amt))
    if(j==12)
    {
      #Finding the yearly Ra 
      x<-x+1
      l<-l+1
      j<-0
      str2[[l]]<-stock_amt
      amt[[l]]<-stock_amt-x*12000-sum
      sum=sum+amt[[l]]
    }
  }
  else
  {
    savings_amt=savings_amt+stock_amt+1000
    stock_amt=0
    print(paste("savings_amt=",savings_amt))
    if(j==12)
    {
      #Finding the yearly Ra 
      x<-x+1
      l<-l+1
      j<-0
      str2[[l]]<-savings_amt
      amt[[l]]<-savings_amt-x*12000-sum
      sum=sum+amt[[l]]
    }
  }
}
print(savings_amt)
years<-c(1994:2019)
str1<-unlist(str1)
str2<-unlist(str2)
yearly_Values<-cbind(years,str1,str2)


amt<-unlist(amt)
#plotting the yearly Ra for Strategy 2



plot(years,amt,main=" Returns of Strategy 2 for 26 years", xlab="Years",ylab="Returns Value")
# Plot with both the strategies:
strategy1<-amt1
strategy2<-amt
data <- data.frame(years,strategy1,strategy2)


strategy1 <- xyplot(strategy1 ~ years, data, type = "l" , lwd=2, col="steelblue",ylim=c(-119000,290000))
strategy2 <- xyplot(strategy2 ~ years, data, type = "l", lwd=2, col="pink",ylim=c(-119000,290000))


doubleYScale(strategy1, strategy2, add.ylab2 = TRUE)

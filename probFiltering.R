options(warn=-1)

# get the necessary libraries
library('rvest')
library('cumstats')
library('dygraphs')
library('htmltools')
library('xts')


coinList<- read.table('C:/Users/lgatarek/Dropbox/Rcodes/tradingTool/labofdata/bitcoin/coinList.txt', sep=",")
coins <- coinList$x
subDir <- Sys.Date()
PRICES <- NULL

for (coinId in c(1:2,4)){
url <- paste("https://coinmarketcap.com/currencies/", coins[coinId], "/historical-data/?start=20170401&end=", gsub('-','',subDir), sep = '')


if (coins[coinId] == 'BitcoinCash'){
  url <- paste("https://coinmarketcap.com/currencies/bitcoin-cash/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'EthereumClassic'){
  url <- paste("https://coinmarketcap.com/currencies/ethereum-classic/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'Bytecoin'){
  url <- paste("https://coinmarketcap.com/currencies/bytecoin-bcn/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'StellarLumens'){
  url <- paste("https://coinmarketcap.com/currencies/stellar/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'MetaverseETP'){
  url <- paste("https://coinmarketcap.com/currencies/metaverse/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'iocoin'){
  url <- paste("https://coinmarketcap.com/currencies/iocoin/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'NAVCoin'){
  url <- paste("https://coinmarketcap.com/currencies/nav-coin/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}

if (coins[coinId] == 'TheChampCoin'){
  url <- paste("https://coinmarketcap.com/currencies/the-champcoin/historical-data/?start=20130428&end=", gsub('-','',subDir), sep = '')
}



webpage <- read_html(url)
# get the closing prices

priceTable <- html_text(html_nodes(webpage,'td'))

dates<-NULL
opens<-NULL
highs<-NULL
lows<-NULL
closes<-NULL
volumes<-NULL
marketCaps<-NULL

noObs <- length(priceTable) / 7

for (i in 1:noObs){
  
  dates[i] <- priceTable[(i-1) * 7 + 1]
  opens[i] <- as.double(priceTable[(i-1) * 7 + 2])
  highs[i] <- as.double(priceTable[(i-1) * 7 + 3])
  lows[i] <- as.double(priceTable[(i-1) * 7 + 4])
  closes[i] <- as.double(priceTable[(i-1) * 7 + 5])
  volumes[i] <- as.double(gsub(",","",priceTable[(i-1) * 7 + 6]))
  marketCaps[i] <- as.double(gsub(",","",priceTable[(i-1) * 7 + 7]))
}

dates<-rev(dates)
opens <- rev(opens)
highs <- rev(highs)
lows <- rev(lows)
closes <- rev(closes)
volumes <- rev(volumes)
marketCaps <- rev(marketCaps)

# get the trade dates in the right format
d<-gsub(',', '', dates)
d<-gsub(' ', '/', d)

d <- gsub('Jan','01',d)
d <- gsub('Feb','02',d)
d <- gsub('Mar','03',d)
d <- gsub('Apr','04',d)
d <- gsub('May','05',d)
d <- gsub('Jun','06',d)
d <- gsub('Jul','07',d)
d <- gsub('Aug','08',d)
d <- gsub('Sep','09',d)
d <- gsub('Oct','10',d)
d <- gsub('Nov','11',d)
d <- gsub('Dec','12',d)

dates <-as.Date(d,"%m/%d/%Y")

PRICES <- cbind(PRICES,  closes / closes[1])
}

daily_ts <- xts(PRICES, order.by = dates) 

K = GG.ComT(daily_ts,1,4)

colnames(daily_ts)<-c('BTC','ETH','XRP')  

dygraph(daily_ts, ylab="value", main = 'Cryptocurrency prices normalized with 01/04/2017 rates') %>% 
  dySeries("BTC",label="Bitcoin", col = "black") %>%
  dySeries("XRP",label="Ripple", col = "green") %>%
  dySeries("ETH",label="Trend", col = "red")

# TO DO: add the common trend

#   
# data(benchmark)
# x=seq(1,6689,by=23) ## monthly data
# global=data.frame(benchmark[x,2:4])
# Kasa.ComT (global,2,4)
# ## Plot the Common Trend
# K=GG.ComT (global,2,4)
# Date=benchmark[x,1]
# plotComT(K,1,x.axis=Date,approx.ticks=12,
#          legend=c("S&P 500 Price index", "Common Trend"),
#          main="Extract Common Trend(s) from Benchmark Markets",
#          ylab="Price", xlab="Time" )

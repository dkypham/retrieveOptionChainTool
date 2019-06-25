# General-purpose data wrangling
library(tidyverse)
# Parsing of HTML/XML files
library(rvest)
# To get stock prices
library(quantmod)

library(reader)

# ======= constants =======
file.working.directory = "/Users/dkypham/Documents/retrieve_option_chain_tool/"
logs.directory = paste0(file.working.directory, "scripts/logs/")
dir.data = "data/"
dir.data.path = paste0(file.working.directory, dir.data)
dir.stock.price = "stock_price/"
dir.option.chain = "option_chains/"
dir.calls = "calls/"
dir.puts = "puts/"
option.chain.year = "2019"
contract.name.length = 6

# # read tickers from file
tickers_list = readLines("tickers.txt")

CheckDirExists <- function( curr.path , desired.path ) {
  path = paste0( curr.path , desired.path )
  if ( !file.exists(path) ) {
    dir.create(path)
  }
}

# ======= check to make sure data folder exists =======
# commented out for speed
CheckDirExists(file.working.directory , dir.data )

# ======= defined functions =======

GetExpiryDate <- function( contract.name) {
  return (substring( rownames(contract.name)[1], nchar(ticker) + 1,
                           nchar(ticker) + contract.name.length ))
}

WriteStockPriceData <- function(ticker , stock.path, retrieval.time) {
  # ======= BEGIN READ =======
  # get the data
  stock.price.table = getQuote(ticker, src="yahoo",what=standardQuote())
  
  # clean up table, only get what's needed
  # Mapping
  #   1 - trade time
  #   2 - last price
  #   3 - change
  #   4 - % change
  #   5 - open
  #   6 - high
  #   7 - low
  #   8 - volume
  stock.price.table <- stock.price.table[, -c(3:7)]
  
  # add timeRetrieved column
  stock.price.table$timeRetrieved <- retrieval.time
  # ======= END READ =======
  
  # ======= BEGIN WRITE =======
  # check if /data/*ticker*/stock_price/*date*.csv exists, if not, create it
  # extract the first 6 numbers from retrival time (the date)
  stock.price.file.time = substr(retrieval.time, 1, 6)
  stock.price.file.name = paste0(stock.price.file.time, "-", ticker, ".csv")
  stock.path = paste0(stock.path, stock.price.file.name)
  
  if (!file.exists(stock.path) )
  {
    # if file doesn't exist, create it
    write.csv(stock.price.table, file = stock.path )
  } else {
    # if file does exist, only append 1 row
    write.table(stock.price.table, file = stock.path, append = TRUE, 
                sep=',',row.names=T,col.names=F )
  }
  # ======= END WRITE =======

}

SaveStockPrice <- function(ticker , path, retrieval.time) {
  # main function called to retrieve stock prices
  
  # check if /data/*ticker*/stock_price folder exists, if not, create it
  CheckDirExists(path , dir.stock.price)
  # set working directory to /data/*ticker*/stock_price/
  stock.path = paste0(path, dir.stock.price)
  
  # get data and write to file
  WriteStockPriceData(ticker , stock.path, retrieval.time)
}

WriteOptionChainData <- function(ticker, option.path, retrieval.time) {
  # ======= BEGIN READ =======
  # get the data
  options.table = getOptionChain(ticker, option.chain.year)
  num.chains = length(options.table) - 1

  for (i in 1:num.chains) {
    
    #iterate through expiries
    # get calls and puts table
    calls.table <- options.table[[i]][1]
    puts.table <- options.table[[i]][2]
    
    # extract the expiry date, all contracts have same expiry in table
    expiry.date = GetExpiryDate(calls.table[[1]][1])
    
    # check if option_chain/*date*/ exists
    CheckDirExists( option.path , expiry.date )
    
    # write calls
    if ( !is.null(calls.table[[1]][1]) ) {
      # check if option_chain/*date*/calls/ exists
      CheckDirExists( option.path , paste0(expiry.date, "/", dir.calls))
      # add retrieval time column
      calls.table$timeRetrieved <- retrieval.time
      calls.path = paste0(option.path , expiry.date, "/", dir.calls, 
                          retrieval.time, ".csv")
      # clean up table, only get what's needed
      # Mapping
      #   1 - Strike
      #   2 - Last
      #   3 - change
      #   4 - bid
      #   5 - ask
      #   6 - volume
      #   7 - OI
      #   8 - timeRetrieved
      calls.table = lapply(calls.table, "[", -c(3))
      write.csv(calls.table, file = calls.path )
    }
    
    # write puts
    # check if table is null first
    if ( !is.null(puts.table[[1]][1]) ) {
      # check if option_chain/*date*/puts/ exists
      CheckDirExists( option.path , paste0(expiry.date, "/", dir.puts))
      # add retrieval time column
      puts.table$timeRetrieved <- retrieval.time
      puts.path = paste0(option.path , expiry.date, "/", dir.puts, 
                         retrieval.time, ".csv")
      # clean up table, only get what's needed
      # Mapping
      #   1 - Strike
      #   2 - Last
      #   3 - change
      #   4 - bid
      #   5 - ask
      #   6 - volume
      #   7 - OI
      #   8 - timeRetrieved
      puts.table = lapply(puts.table, "[", -c(3))
      write.csv(puts.table, file = puts.path )
    }
  }
}

SaveOptionChain <- function(ticker, path, retrieval.time) {
  # main function called to retrieve stock prices
  
  # check if /data/*ticker*/option_chain folder exists, if not, create it
  CheckDirExists(path , dir.option.chain )
  # set working directory to /data/*ticker*/option_chains/
  option.path = paste0(path, dir.option.chain)
  
  # get data and write to file
  WriteOptionChainData(ticker, option.path, retrieval.time)
}

SaveLog <- function(ticker, path) {
  current_time <- Sys.time()
  msg <- paste0("Retrieved ", ticker, " stock and option data at time: ", 
                current_time, ".")
  
  file.path = paste0(path, "ROCT_log.txt")
}

# ======= iterate through desired tickers =======
for ( ticker in tickers_list ) {
  dir.ticker = paste0(ticker, "/")
  retrieval.time = format(Sys.time(), "%y%m%d-%H%M%S")
  
  # check if /data/*ticker*/ exists
  CheckDirExists(dir.data.path , dir.ticker)
  # set working directory to /data/*ticker*/
  path = paste0(dir.data.path , dir.ticker)
  
  # ======= GET STOCK PRICE =======
  SaveStockPrice(ticker , path, retrieval.time)

  # ======= GET OPTION CHAIN =======
  SaveOptionChain( ticker , path, retrieval.time)
  
  # ======= LOGGING =======
  #SaveLog( ticker , logs.directory)
  
}

print("Done!")
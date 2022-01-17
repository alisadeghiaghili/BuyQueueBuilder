setwd("E:\\Alerts\\WriteData\\Queue-Buy")

source("loadLibs.R")
source("loadFuncs.R")
source("systemConfigs.R")

AnyDataTries <- 0

while(!file.exists(OrderTransactionsRDSPath)) {
  if(AnyDataTries >= AnyDataMaxTries){
    stop("Reached to AnyDataMaxTries Number with no data")
  }
  
  AnyDataTries <- AnyDataTries + 1
  Sys.sleep(AnyDataSleepTimeInMins * 60)
}

NewDataTries <- 0

BuyOrderTransactions <- loadOrderTransactions(OrderTransactionsRDSPath = OrderTransactionsRDSPath) %>% 
  extractOrderSidesData(side = 0)
NumOfBuyOrderTransactionsRows <- nrow(BuyOrderTransactions)
NumOfProcessedRows <- 0

while(NewDataTries < NewDataMaxTries) {
  # tic()
  if(NumOfProcessedRows >= NumOfBuyOrderTransactionsRows) {
    PreviousRunLastOrder <- readRDS("LastRun.RDS")$MaxDatabaseID
    BuyOrderTransactions <- loadOrderTransactions(OrderTransactionsRDSPath = OrderTransactionsRDSPath) %>% 
      extractOrderSidesData(side = 0) %>% 
      filter(DatabaseID > PreviousRunLastOrder)
    
    if(nrow(BuyOrderTransactions) == 0) {
      print(paste("No NewData on", Sys.time()))
      if(as_hms(Sys.time()) >= as_hms("19:00:00")) {
        NewDataTries <- NewDataTries + 1
        print(paste("end of data on", Sys.time()))
      }
      Sys.sleep(NewDataSleepTimeInMins * 60)
    } else {
      NumOfBuyOrderTransactionsRows <- nrow(BuyOrderTransactions)
      NumOfProcessedRows <- 1
      NewDataTries <- 0
      while(NumOfProcessedRows < NumOfBuyOrderTransactionsRows) {
        buildQueue(data = BuyOrderTransactions[NumOfProcessedRows:min((NumOfProcessedRows + 100), NumOfBuyOrderTransactionsRows), ], side = 0)
        NumOfProcessedRows <- min((NumOfProcessedRows + 100), NumOfBuyOrderTransactionsRows)
        print(paste("NewData Added on", Sys.time()))
      }
    }
  } else {
    NumOfProcessedRows <- 1
    while(NumOfProcessedRows < NumOfBuyOrderTransactionsRows) {
      buildQueue(data = BuyOrderTransactions[NumOfProcessedRows:min((NumOfProcessedRows + 100), NumOfBuyOrderTransactionsRows), ], side = 0)
      NumOfProcessedRows <- min((NumOfProcessedRows + 100), NumOfBuyOrderTransactionsRows)
      print(paste("NewData Added on", Sys.time()))
    }
  }
  # toc()
}

print("Reached to NewDataMaxTries Number without any changes")
loadOrderTransactions <- function(OrderTransactionsRDSPath) {
  while(!file.exists(OrderTransactionsRDSPath)) {
    Sys.sleep(time = 2)
  }
  
  readRDS(file = OrderTransactionsRDSPath) %>% 
    mutate(OrderDateTime = OrderTime,
           OrderTime = as_hms(OrderDateTime)) %>% 
    rename(BrokerID = Broker,
           CustomerID = Customer,
           CommodityID = Commodity) %>% 
    return()
}
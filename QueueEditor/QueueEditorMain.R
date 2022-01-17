setwd("E:\\Alerts\\WriteData\\Queue-Buy\\QueueEditor")

source("loadLibs.R")
source("loadFuncs.R")
source("systemConfigs.R")

BuyQueue <- loadQueue(filePath = BuyQueuePath)

BuyQueue %>% 
  distinct() %>% 
  group_by(OrderDate, ContractID, OrderTime, Price) %>% 
  arrange(OrderDate, ContractID, OrderTime, desc(Price), QueuePosition) %>%
  View()
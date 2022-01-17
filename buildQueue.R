buildQueue <- function(data, side){
  QueueImported <- FALSE
  if(file.exists("LastRun.RDS")) {
    beforeMadeQueue <- readRDS("LastRun.RDS")$Queue %>% 
      filter(OrderDate == Sys.Date())
    tMinusOne <- readRDS("LastRun.RDS")$LastMoment %>% as_hms()
    QueueImported <- TRUE
  }
  
  if(!QueueImported) {
    Queue <- data[0,]
  } else {
    Queue <- beforeMadeQueue[0,]
  }
  
  for (contract in unique(data$ContractID)) {
    currentRow <- 1
    times <- unique(data[data$ContractID == contract, ]$OrderTime) %>% sort() %>% 
      as_hms()
    for (time in times) {
      if (side == 1) {
        QueueMomentSnapshot <- data[data$ContractID == contract & data$OrderTime == time, ] %>%
          arrange(Price) 
      } else {
        QueueMomentSnapshot <- data[data$ContractID == contract & data$OrderTime == time, ] %>%
          arrange(desc(Price))
      }
      
      if (currentRow == 1) {
        if(!QueueImported) {
          rowNum <- nrow(QueueMomentSnapshot)
          Queue[currentRow:(rowNum + currentRow - 1), ] <- QueueMomentSnapshot
          
        } else {
          
          QueuetMinusOne <- beforeMadeQueue[beforeMadeQueue$ContractID == contract & beforeMadeQueue$OrderTime >= tMinusOne, ]
          
          if (side == 1) {
            QueueMomentSnapshot <- bind_rows(QueueMomentSnapshot, QueuetMinusOne) %>% 
              arrange(OrderTime, Price)
          } else {
            QueueMomentSnapshot <- bind_rows(QueueMomentSnapshot, QueuetMinusOne) %>% 
              arrange(OrderTime, desc(Price))
          }
          
          QueueMomentSnapshot <- QueueMomentSnapshot %>% 
            group_by(OrderbookID) %>% 
            arrange(DatabaseID) %>% 
            slice(n()) %>% 
            ungroup() %>% 
            filter(Status != 1) %>% 
            filter(DataID != 13) %>% 
            filter(Quantity !=0)
          
          rowNum <- nrow(QueueMomentSnapshot)
          # Queue[currentRow:(rowNum + currentRow - 1), ] <- QueueMomentSnapshot %>% mutate(OrderTime = time)
          QueueMomentSnapshot <- QueueMomentSnapshot %>% 
            mutate(OrderTime = as_hms(time))
          Queue <- rbind(Queue, QueueMomentSnapshot)
        }
        
      } else {
        
        QueuetMinusOne <- Queue[Queue$ContractID == contract & Queue$OrderTime == tMinusOne, ]
        
        if (side == 1) {
          QueueMomentSnapshot <- bind_rows(QueueMomentSnapshot, QueuetMinusOne) %>% 
            arrange(OrderTime, Price)
        } else {
          QueueMomentSnapshot <- bind_rows(QueueMomentSnapshot, QueuetMinusOne) %>% 
            arrange(OrderTime, desc(Price))
        }
        
        QueueMomentSnapshot <- QueueMomentSnapshot %>% 
          group_by(OrderbookID) %>% 
          arrange(DatabaseID) %>% 
          slice(n()) %>% 
          ungroup() %>% 
          filter(Status != 1) %>% 
          filter(DataID != 13) %>% 
          filter(Quantity !=0)
        
        rowNum <- nrow(QueueMomentSnapshot)
        QueueMomentSnapshot <- QueueMomentSnapshot %>% 
          mutate(OrderTime = as_hms(time))
        Queue <- rbind(Queue, QueueMomentSnapshot)
        # Queue[currentRow:(rowNum + currentRow - 1), ] <- QueueMomentSnapshot %>% mutate(OrderTime = time)
        
      }
      
      currentRow <- currentRow + rowNum
      tMinusOne <- time
    }
  }
  
  rm(QueuetMinusOne)
  
  if (side == 1) {
    
    Queue <- Queue %>% 
      arrange(ContractID, OrderTime, Price) %>% 
      group_by(CommodityID, ContractID, OrderTime) %>% 
      mutate(QueuePosition = row_number())
    
  } else {
    
    Queue <- Queue %>% 
      arrange(ContractID, OrderTime, desc(Price)) %>% 
      group_by(CommodityID, ContractID, OrderTime) %>% 
      mutate(QueuePosition = row_number())
  }
  
  if(file.exists("LastRun.RDS")){
    Queue <- Queue %>% 
      mutate(OrderTime = as_hms(OrderTime))
    Queue <- rbind(beforeMadeQueue, Queue)
  }
  
  list(Queue = Queue %>% mutate(OrderTime = as_hms(OrderTime)), 
       MaxDatabaseID = Queue$DatabaseID %>% max(), 
       LastMoment = tMinusOne %>% as_hms()) %>% 
    saveRDS(file = "LastRun.RDS")
  
}
extractOrderSidesData <- function(data, side) {
  data %>% 
    filter(OrderSide == side) %>% 
    return()
}
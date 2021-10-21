#limpieza del global environment
rm(list = ls())

#agregar librerias
library(stringr)
library(dplyr)
library(tidyr)

#importar base de datos
BusinessBIBA_Challenge <- read_csv("BusinessBIBA_Challenge.csv", 
                                   col_types = cols(...1 = col_skip()))

contador <- 1
for (i in 1:nrow(BusinessBIBA_Challenge)) {
  if(i == 1) {
    BusinessBIBA_Challenge$noEvents[i] <- contador
  } else if(isTRUE(BusinessBIBA_Challenge$ID[i] == BusinessBIBA_Challenge$ID[i - 1])) {
    contador <- contador + 1
    BusinessBIBA_Challenge$noEvents[i] <- contador
  } else {
    contador <- 1
    BusinessBIBA_Challenge$noEvents[i] <- contador
  }
}

maxIndex <- aggregate(noEvents ~ ID ,BusinessBIBA_Challenge, max)
BusinessBIBA_Challenge <- left_join(BusinessBIBA_Challenge, maxIndex, by = "ID")
BusinessBIBA_Challenge <- plyr::rename(BusinessBIBA_Challenge, c("noEvents.x" = "noEvents", "noEvents.y" = "MaxEvents"))

CP <- select(BusinessBIBA_Challenge, c("ID", "CP"))
CP <- CP %>% filter(!is.na(CP))
TXN <- BusinessBIBA_Challenge %>% filter (!is.na(TXN)) %>% group_by(ID) %>% summarise(TXN = sum(TXN),
                                                                             TIMES_USED = length(ID))
CARD_TYPE <- BusinessBIBA_Challenge %>% filter(STATUS=="APPROVED") %>% select(ID, INTEREST_RATE, CAT, AMOUNT,MOTIVE)
CARD_TYPE <- plyr::rename(CARD_TYPE, c("MOTIVE" = "CARD_TYPE"))
DS <- select(BusinessBIBA_Challenge, c("ID", "DELIVERY_SCORE"))
DS <- DS %>% filter(!is.na(DELIVERY_SCORE))

BD <- left_join(maxIndex, CP, by = "ID") %>% 
  left_join(., CARD_TYPE, by = "ID") %>%
  left_join(., DS, by = "ID") %>%
  left_join(., TXN, by = "ID")

write.csv(BD, "BDprocessed_AVH.csv")
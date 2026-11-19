library(stringr)
library(dplyr)
bilbasedf<-readRDS("teslaerfrabilbasen.rds")


private<-bilbasedf[is.na(bilbasedf$CVR), ]
saveRDS(private, "private.rds")

bilbasedf_clean<-bilbasedf[!is.na(bilbasedf$CVR), ]
bilbasedf_clean<-bilbasedf_clean[!is.na(bilbasedf_clean$forhandlernavn),]

#har fjernet alle NA vi ikke vil have.

#nu skal vi have cleanet vores tal for characters

bilbasedf_clean$pris<-gsub("[^0-9]", "", bilbasedf_clean$pris)
bilbasedf_clean$pris<-as.numeric(bilbasedf_clean$pris)
bilbasedf_clean$aargang<-as.numeric(str_extract(bilbasedf_clean$aargang, "[0-9]{4}"))

bilbasedf_clean$km<-(str_remove_all(bilbasedf_clean$km, "[^0-9]"))
bilbasedf_clean$km<-as.numeric(bilbasedf_clean$km)

bilbasedf_clean<-bilbasedf_clean %>%
  rename(raekkevidde_km = raekkevidde)
bilbasedf_clean$raekkevidde_km<-(str_remove_all(bilbasedf_clean$raekkevidde_km, "[^0-9]"))
bilbasedf_clean$raekkevidde_km <-
  as.numeric(bilbasedf_clean$raekkevidde_km)

bilbasedf_clean$postnr.<-NA
bilbasedf_clean$postnr.<-as.numeric(str_extract(bilbasedf_clean$forhandleradresse, "[0-9]{4}"))

bilbasedf_clean$carid<-as.numeric(bilbasedf_clean$carid)

saveRDS(bilbasedf_clean, "bilbasetesla.rds")                                   


# Er der biler med samme ID?
sum(duplicated(bilbasedf_clean$carid))

# Undersøg pris, kilometer og årgang
summary(bilbasedf_clean[, c("pris", "km", "aargang")])

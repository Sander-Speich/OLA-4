autoscout<-readRDS("teslaerfraautoscout.rds")


autoscout$pris<-as.numeric(str_remove_all(autoscout$pris, "[^0-9]")) 
autoscout$aargang<-as.numeric(str_extract(autoscout$aargang, "[0-9]{4}")) 
autoscout$km<-as.numeric(str_remove_all(autoscout$km,  "[^0-9]"))


autoscout<-autoscout %>% rename(motorpower_kw = motorpower)
autoscout$motorpower_kw<-as.numeric(str_extract(autoscout$motorpower_kw,  "[0-9]+"))


autoscout<-autoscout %>% rename(pris_DKK = pris)
#omregne prisen til DKK
autoscout$pris_DKK<-autoscout$pris_DKK*7.47
#runde af
autoscout$pris_DKK<-round(autoscout$pris_DKK)

saveRDS(autoscout, "autoscout_clean.rds")

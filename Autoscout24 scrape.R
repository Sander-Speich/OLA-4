library(chromote)
library(rvest)
library(dplyr)
library(logr)


# init log
log_open(file_name = "autoscoutlog")

# init chromote
options(chromote.headless = FALSE)
autoscoutchromesession <- ChromoteSession$new()
#b$close()
#b$parent$close()

# find startlink
autourl<-"https://www.autoscout24.de/lst/tesla?sort=standard&desc=0&ustate=N%2CU&cy=D&ocs_listing=include&damaged_listing=exclude&page="
endurl<-"&atype=C&source=listpage_pagination&search_id=1vqbs66e6l3"

#Vi henter første side
autoscoutside1<-paste0(autourl, 1, endurl)

autoscoutchromesession$Page$navigate((autoscoutside1))

Sys.sleep(5)

#Se siden
autoscoutchromesession$view()

html <- autoscoutchromesession$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

page <- read_html(html)


carlist <- page %>%
  html_elements("article[data-testid='list-item']")

#ramte vi rigtigt?

length(carlist)
#20 biler det passer med hjemmesiden ok



#VI STARTER MED AT HENTE ALLE VORES ATTRIBUTTER FOR 1 BIL FØR VI KØRER VORES LOOP DET GØR DET HELE MERE OVERSKUELIGT

#piller første bil ud
car1 <- carlist[[1]]

#Piller modelnavnet ud
model <- car1 %>%
  html_element("[class*='ListItemTitle_title']") %>%
  html_text(trim = TRUE)

#vi henter variant
variant <- car1 %>%
  html_element("[class*='ListItemTitle_subtitle']") %>%
  html_text(trim = TRUE)


#Så prisen, den står bare som almindelig tekst
pris <- car1 %>%
  html_element("[data-testid='regular-price']") %>%
  html_text(trim = TRUE)

#for detaljerne behøves vi ikke sortere de ligger i præcis samme li class som hver sin tekst
detaljer <- car1 %>%
  html_elements("[class*='ListItemPill_text']") %>%
  html_text(trim = TRUE)

#Vi fik en liste, så vi laver detaljerne til hver sin variabel
#årgang var den første
aargang <- detaljer[1]
km <- detaljer[2]
braendstof <- detaljer[3]
motorpower <- detaljer[4]

#Og lokationen for bilen
lokation <- car1 %>%
  html_element("[data-testid='dealer-address']") %>%
  html_text(trim = TRUE)


#Få den unikke ID til linket
carID <- car1 %>%
  html_attr("data-guid")


#vi tester lige at sætte alle oplysningerne ind i df med 1 bil.
tesla1auto<-data.frame(model, variant, pris,aargang,km,braendstof,lokation,carID)

teslamedalleoplysningerauto<-data.frame()

teslamedalleoplysningerauto<-rbind(teslamedalleoplysningerauto, tesla1auto)


#laver en tom dataframe vi kan proppe bilerne ind i
teslaerfraautoscout <- data.frame()

saveRDS(teslaerfraautoscout, "teslaerfraautoscout.rds")

autoscoutchromesession$view()

# Funktion der henter et bestemt element.
# Hvis elementet ikke findes, returneres NA. 
#Vi havde store problemer ved scraping af detaljerne til bilerne som årgang, km tal osv.
#Det skyldes at nogen biler havde fx producentverficering. SÅ der kom et nyt element ind og rykkede alle vores værdier 1 kollone hver gang
#Vi kører denne for at undgå det.
hent_element <- function(car, selector) {
  
  element <- car %>%
    html_element(selector)
  
  if (is.na(element)) {
    return(NA_character_)
  }
  
  html_text(element, trim = TRUE)
}
#det oprindelige webscrape loop på autoscout
#Vi sætter Limit til 28 fordi vi har fundet ud af der er 28 sider med Teslaer
limitauto <- 46
#i in 1-28
for (i in 1:limitauto) {
  
  #Byg linket til siden går fra page 1-28
  url <- paste0(autourl, i, endurl)
  
  #Gå ind på siden loopet er nået til 
  autoscoutchromesession$Page$navigate(url)

  #Vent mellem 3 og 7 sekunder
  Sys.sleep(sample(3:7, 1))
  
  #Scrape HTML
  html <- autoscoutchromesession$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  page <- read_html(html)
  
  #Find alle biler på siden ved at bruge article vi fandt tidligere
  carlist <- page %>%
    html_elements("article[data-testid='list-item']")
  
  
  #Gå igennem hver bil på siden - Så det her er altså et loop efter vi har scrapet for at hente de informationer ud vi skal bruge 
  for (j in 1:seq_along(carlist)) { #Vi blev ved med at få calist[j] out of bounds. Så nu siger vi uanset carlist skal du køre videre
    #kører altså length af carlist (20) cars igennem hvor hver enkelt ny hedder car
    car <- carlist[[j]]
    #samme udtræk som før
    model <- car %>%
      html_element("[class*='ListItemTitle_title']") %>%
      html_text(trim = TRUE)
    
    variant <- car %>%
      html_element("[class*='ListItemTitle_subtitle']") %>%
      html_text(trim = TRUE)
    
    pris <- car %>%
      html_element("[data-testid='regular-price']") %>%
      html_text(trim = TRUE)
    
    aargang <- hent_element(
      car,
      "[data-testid='VehicleDetails-calendar']"
    )
    
    km <- hent_element(
      car,
      "[data-testid='VehicleDetails-mileage_odometer']"
    )
    
    braendstof <- hent_element(
      car,
      "[data-testid='VehicleDetails-gas_pump']"
    )
    
    motorpower <- hent_element(
      car,
      "[data-testid='VehicleDetails-speedometer']"
    )
    
    lokation <- car %>%
      html_element("[data-testid='dealer-address']") %>%
      html_text(trim = TRUE)
    
    carID <- car %>%
      html_attr("data-guid")
    
    #Lav bilen til én række
    tesla <- data.frame(
      carID,
      model,
      variant,
      pris,
      aargang,
      km,
      braendstof,
      motorpower,
      lokation
    )
    
    #Tilføj bilen til vores samlede dataframe
    teslaerfraautoscout <- rbind(teslaerfraautoscout, tesla)
  }
}

saveRDS(teslaerfraautoscout, "teslaerfraautoscout.rds")


library(writexl)

write_xlsx(
  teslaerfraautoscout,
  "teslaerfraautoscout.xlsx"
)

#tjekker for dubletter.
teslaerfraautoscout <- teslaerfraautoscout[912:1822, ]
teslaerfraautoscout[duplicated(teslaerfraautoscout$carID), ]
# Find alle rækker (både 1. og 2. gang), hvor carID er en dublet, og sortér dem efter carID
dublet_rækker <- teslaerfraautoscout[
  teslaerfraautoscout$carID %in% teslaerfraautoscout$carID[duplicated(teslaerfraautoscout$carID)], 
]
# Sortér efter carID, så ens ID'er står lige under hinanden
dublet_rækker[order(dublet_rækker$carID), ]
teslaerfraautoscout <- teslaerfraautoscout[!duplicated(teslaerfraautoscout$carID), ]






for (i in (1:limit)) {
  #build url
  url=paste0(baseurl,i)
  bilbasechromesession$Page$navigate(url)
  Sys.sleep(sample(3:7,1,replace = F))
  
  # chrome to site
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  Sys.sleep(1)
  
  # revest the static content
  page <- read_html(html)
  carlist=page %>% html_elements("article")
  log_print(url)
  tryCatch({
    prices <- page %>% html_nodes("[class*='Listing_price']") %>% html_text()
    make_model <- page %>% html_nodes("[class*='Listing_makeModel']") %>% html_text()
    
    tmpdf = data.frame(
      prices=prices,
      make_model=make_model
    )
    
    cardf=rbind(cardf, tmpdf)
  }, error = function(e){
    log_print(str_c("Error at iteration", i, ":", conditionMessage(e)))
    return(i)
  }
  )
}



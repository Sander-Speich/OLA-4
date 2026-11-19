library(chromote)
library(rvest)
library(dplyr)
library(logr)


# init log
log_open(file_name = "carlog")

# init chromote
options(chromote.headless = FALSE)
bilbasechromesession <- ChromoteSession$new()
#b$close()
#b$parent$close()

# find startlink
baseurl="https://www.bilbasen.dk/brugt/bil/tesla?includeengroscvr=true&includeleasing=false&page="



#Vi henter første side
urlside1<-paste0(baseurl, 1)

bilbasechromesession$Page$navigate((urlside1))

Sys.sleep(5)

#Se siden
bilbasechromesession$view()

html <- bilbasechromesession$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

page <- read_html(html)


carlist <- page %>%
  html_elements("article[data-e2e='search-result-card']")

#ramte vi rigtigt?

length(carlist)
#30 biler det passer med hjemmesiden ok



#VI STARTER MED AT HENTE ALLE VORES ATTRIBUTTER FOR 1 BIL FØR VI KØRER VORES LOOP DET GØR DET HELE MERE OVERSKUELIGT


#piller første bil ud
car1 <- carlist[[1]]

#Piller modelnavnet ud
model <- car1 %>%
  html_element("[class*='Listing_makeModel'] h3") %>%
  html_text(trim = TRUE)

#Vi vil også gerne have variant, men den ligger desværre uden sit eget tag, men bare som løs tekst.
#Vi henter så bare alt tekst fra Listing_makeModel og fjerne model, da vi har hentet den ovenfor
variant <- car1 %>%
  html_element("[class*='Listing_makeModel']") %>%
  html_text(trim = TRUE)

variant <- trimws(sub(model, "", variant)) #Fjern "model" altså vores modelnavn og erstat med "" ingenting


#Så prisen, den står bare som almindelig tekst
pris <- car1 %>%
  html_element("[class*='Listing_price']") %>%
  html_text(trim = TRUE)

#for detaljerne behøves vi ikke sortere de ligger i præcis samme li class som hver sin tekst
detaljer <- car1 %>%
  html_elements("[class*='Listing_details'] li") %>%
  html_text(trim = TRUE)

#Vi fik en liste, så vi laver detaljerne til hver sin variabel
#årgang var den første
aargang <- detaljer[1]
km <- detaljer[2]
raekkevidde <- detaljer[3]
braendstof <- detaljer[4]

#Den lange beskrivelse skal også med
beskrivelse <- car1 %>%
  html_element("[class*='Listing_description']") %>%
  html_text(trim = TRUE)

#Og lokationen for bilen
lokation <- car1 %>%
  html_element("[class*='Listing_location'] span") %>%
  html_text(trim = TRUE)

#Pille linket til første for at tjekke om det virker
link <- car1 %>%
  html_element("a[data-e2e='search-result-link']") %>%
  html_attr("href")
#[1] "https://www.bilbasen.dk/brugt/bil/tesla/model-3/standard-range-rwd-4d/7041380"


#Få den unikke ID til slut i linket (7041380)
carid <- basename(link)


#Vi skal også have info på forhandler, det jeg kan finde er URL til logoet som står ovenfor hver bil
forhandlerlogo <- car1 %>%
  html_element("img[alt='Forhandlerlogo']") %>%
  html_attr("src")



#Vi skulle finde mere forhandlerinfo, så vi scraper nu hver bils link
#Vi starter igen med at teste det hele virker på 1 bil

link1 <- teslaerfrabilbasen$link[1]

bilbasechromesession$Page$navigate(link1)

Sys.sleep(5)

html <- bilbasechromesession$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

bilside <- read_html(html)
#Vi kigger hver gang nøje på htmlen fra udviklerværktøjet. Vi vil gerne finde den simpleste, men det element vi vælger skal vi også være sikre på
#ikke kan poppe op andre steder der ikke er relevant i htmlen. Den her er perfekt fordi den siger "sellerinfocomponen" så vi ved vi har med sælgerinfo.
forhandlernavn<-bilside%>%
  html_element("[class*='bas-MuiVipSectionComponent-sectionHeader bas-MuiSellerInfoComponent-headerStyles']")%>%
  html_text(trim=TRUE)
#Her er de dejlige ved os og vælger at skrive selleradress ved selve adressen
forhandleradresse<-bilside%>%
  html_element("[data-e2e='seller-address']")%>%
  html_text(trim=TRUE)

forhandleradresse<-bilside%>%
  html_element("[data-e2e='seller-address']")%>%
  html_text(trim=TRUE)
#Den her er også lækker. "sellerinfocomponent"
CVRforhandler<-bilside%>%
  html_element("[class*='bas-MuiSellerInfoComponent-cvr']")%>%
  html_text(trim=TRUE)
CVR <- trimws(sub("CVR-nr.:", "", CVRforhandler))

#Vi vil også gerne have tlf.nr. men den er lidt tricky. det gemmer sig i en knap man skal klikke på for at få vist tlfnr. inde på bilbasen.
#Vi tester hvad vi får hvis vi henter på knappens class
bilside %>%
  html_elements("[class*='bas-MuiButton-label']") %>%
  html_text(trim = TRUE)
#Vi fik vildt mange, fordi mange titler bruger denne type knap

#Vi skal bruge denne psykokode. Men den siger egentlig bare:
#helt normal som altid. MEN vi tilføjer array.from så vi får et array istedet for nodelist
#De ligner hinanden, men .find virker kun på arrays
#.find - find 1 værdi i vores array. "x" tag 1 element x.innertext se på teksten i det 1 element. .trim fjern mellemrum.
bilbasechromesession$Runtime$evaluate(
  "Array.from(document.querySelectorAll('[class*=\"bas-MuiButton-label\"]')) 
   .find(x => x.innerText.trim() === 'Vis telefonnummer') 
   .click()" 
)
#Det virker simelthen ikke grundet cloudfare turnstile.


#vi tester lige at sætte alle oplysningerne ind i df med 1 bil.
tesla1<-data.frame(model, variant, pris,aargang,km,raekkevidde,braendstof,beskrivelse,lokation,link,carid,forhandlerlogo,forhandlernavn,forhandleradresse,CVR)

teslamedalleoplysninger<-data.frame()

teslamedalleoplysninger<-rbind(teslamedalleoplysninger, tesla1)






#laver en tom dataframe vi kan proppe bilerne ind i
teslaerfrabilbasen <- data.frame()
#Laver 3 nye tomme kolonner til de oplysninger vi kun kan finde inde på bilens egen side via linket
teslaerfrabilbasen$forhandlernavn <- NA
teslaerfrabilbasen$forhandleradresse <- NA
teslaerfrabilbasen$CVR <- NA
saveRDS(teslaerfrabilbasen, "teslaerfrabilbasen.rds")

bilbasechromesession$view()

#Nå endnu et bump. Vi har vildt mange NA'er på forhandlernavn. 562 NA'er, men 376 har CVR. Det efterlader os med umiddelbart 180 privatpersoner, passer meget godt med hjemmesidens count. 
#Derudover har vi 24 der har adresse når vi ikke har CVR eller forhandlernavn. 


#de 24 var bare ugyldige links. 

#Vi tjekker de 376 de er med sikkerhed forhandlere da de har cvr nr. 

#----TJEKKER HVORDAN VI FINDER VORES NA VÆRDIER----------
manglernavn <- which(
  is.na(teslaerfrabilbasen$forhandlernavn) &
    !is.na(teslaerfrabilbasen$CVR)
)
i <- manglernavn[1]
teslaerfrabilbasen[i, c(
  "link",
  "forhandleradresse",
  "CVR"
)]
link <- teslaerfrabilbasen$link[i]
bilbasechromesession$Page$navigate(link)
Sys.sleep(5)
bilbasechromesession$view()
html <- bilbasechromesession$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value
bilside <- read_html(html)
forhandlernavn <- bilside %>%
  html_element("[data-e2e='vip-section-dealer-info'] h2") %>%
  html_text(trim = TRUE)
forhandlernavn
#tjekker vores 4 manglende fra manglernavn loopet
i <- 589
#VI KAN IKKE FÅ LOV AT TILGÅ DERES LINKS
link <- teslaerfrabilbasen$link[i]

bilbasechromesession$Page$navigate(link)
Sys.sleep(5)
bilbasechromesession$view()
#-----TJEKKER HVORDAN VI FINDER VORES NA VÆRDIER-----

#------TJEKKER VORES ANTAGEDE PRIVATPERSONER, DER DERFOR IKKE HAR FORHANDLERNAVN
privatekandidater <- which(
  is.na(teslaerfrabilbasen$forhandlernavn) &
    !is.na(teslaerfrabilbasen$forhandleradresse) &
    is.na(teslaerfrabilbasen$CVR)
)

length(privatekandidater)
# bør være 162

i <- privatekandidater[1]

teslaerfrabilbasen[i, c(
  "carid",
  "link",
  "forhandleradresse"
)]

link <- teslaerfrabilbasen$link[i]

bilbasechromesession$Page$navigate(link)
Sys.sleep(5)
bilbasechromesession$view()

html <- bilbasechromesession$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

bilside <- read_html(html)

privat <- bilside %>%
  html_element("[data-e2e='vip-section-private-info'] h2") %>%
  html_text(trim = TRUE)

privat #YEEES DET PASSER VI KØRER VORES FORHÅBENTLIGT SIDSTE LOOP "PRIVATLOOP"
#------TJEKKER VORES ANTAGEDE PRIVATPERSONER, DER DERFOR IKKE HAR FORHANDLERNAVN

#Vi kører vores loop for privatpersoner
privatekandidater <- which(
  is.na(teslaerfrabilbasen$forhandlernavn) &
    !is.na(teslaerfrabilbasen$forhandleradresse) &
    is.na(teslaerfrabilbasen$CVR)
)

for (i in privatekandidater) {
  
  tryCatch({
    
    link <- teslaerfrabilbasen$link[i]
    
    bilbasechromesession$Page$navigate(link)
    Sys.sleep(sample(3:7, 1))
    
    html <- bilbasechromesession$Runtime$evaluate(
      "document.documentElement.outerHTML"
    )$result$value
    
    bilside <- read_html(html)
    
    forhandlernavn <- bilside %>%
      html_element("[data-e2e='vip-section-private-info'] h2") %>%
      html_text(trim = TRUE)
    
    teslaerfrabilbasen$forhandlernavn[i] <- forhandlernavn
    
  }, error = function(e) {
    # Hvis én annonce fejler, fortsætter loopet
  })
}



#Vi kører vores loop med den nye "data-e2e='vip-section-dealer-info'" for at få de resterende forhandlernavne ud
manglernavn <- which(
  is.na(teslaerfrabilbasen$forhandlernavn) &
    !is.na(teslaerfrabilbasen$CVR)
)

for (i in manglernavn) { #Så vi kun kører alle vores NA'er hvor der var cvr allerede. altså de 376 vi testede på ovenfor.
      
      tryCatch({ #returnerer bare NA hvis der ikke er et navn
        
        link <- teslaerfrabilbasen$link[i]
        
        bilbasechromesession$Page$navigate(link)
        Sys.sleep(sample(3:7, 1))
        
        html <- bilbasechromesession$Runtime$evaluate(
          "document.documentElement.outerHTML"
        )$result$value
        
        bilside <- read_html(html)
        
        forhandlernavn <- bilside %>%
          html_element("[data-e2e='vip-section-dealer-info'] h2") %>%
          html_text(trim = TRUE)
        
        teslaerfrabilbasen$forhandlernavn[i] <- forhandlernavn
        
      }, error = function(e) {
        # Hvis én annonce fejler, fortsætter loopet
      })
    }
saveRDS(teslaerfrabilbasen, "teslaerfrabilbasen.rds")

# Hvor mange mangler stadig forhandlernavn? #Vi har løst 372 af 376
sum(is.na(teslaerfrabilbasen$forhandlernavn))

table(
  navn = is.na(teslaerfrabilbasen$forhandlernavn),
  adresse = is.na(teslaerfrabilbasen$forhandleradresse),
  CVR = is.na(teslaerfrabilbasen$CVR)
)
#Tjekker lige de 4 vi mangler
manglernavnCVR <- which(
  is.na(teslaerfrabilbasen$forhandlernavn) &
    !is.na(teslaerfrabilbasen$CVR)
)

teslaerfrabilbasen[
  manglernavnCVR,
  c("carid", "link", "forhandleradresse", "CVR")
]

table(
  navn=is.na(teslaerfrabilbasen$forhandlernavn)
)
#Vi har reduceret NA'erne til 29stks. Som jeg egentlig bare tænker vi piller ud. Vi kan ikke tilgå linkene til bilerne.
#FALSE  TRUE 
#790    29
fjernedebiler<- teslaerfrabilbasen[ #gemmer de 29 NA biler bare fordi
    is.na(teslaerfrabilbasen$forhandlernavn),
  ]
#fjerner NA'er
teslaerfrabilbasen <- teslaerfrabilbasen[
  !is.na(teslaerfrabilbasen$forhandlernavn),
]
saveRDS(teslaerfrabilbasen, "teslaerfrabilbasen.rds")
saveRDS(fjernedebiler, "biler_fjernet.rds")



#så kører vi "linkloopet" efter oprindelige webscrapeloop nedenfor. Dette tilgår linksne fra vores i forvejen scrapede biler.
#Gå igennem alle biler vi allerede har scrapet
for (i in 760:nrow(teslaerfrabilbasen)) {
  
  #Hent linket til bil nummer i
  link <- teslaerfrabilbasen$link[i]
  
  #Gå ind på bilens egen side
  bilbasechromesession$Page$navigate(link)
  
  #Vent mellem 3 og 7 sekunder
  Sys.sleep(sample(3:7, 1))
  
  #Hent HTML fra bilens side
  html <- bilbasechromesession$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  bilside <- read_html(html)
  
  #Hent forhandlernavn
  forhandlernavn <- bilside %>%
    html_element("[class*='bas-MuiSellerInfoComponent-headerStyles']") %>%
    html_text(trim = TRUE)
  
  #Hent forhandleradresse
  forhandleradresse <- bilside %>%
    html_element("[data-e2e='seller-address']") %>%
    html_text(trim = TRUE)
  
  #Hent CVR
  CVRforhandler <- bilside %>%
    html_element("[class*='bas-MuiSellerInfoComponent-cvr']") %>%
    html_text(trim = TRUE)
  
  CVR <- trimws(sub("CVR-nr.:", "", CVRforhandler))
  
  #Gem oplysningerne på den rigtige bil/række
  teslaerfrabilbasen$forhandlernavn[i] <- forhandlernavn
  teslaerfrabilbasen$forhandleradresse[i] <- forhandleradresse
  teslaerfrabilbasen$CVR[i] <- CVR
}





#det oprindelige webscrape loop
#Vi sætter Limit til 28 fordi vi har fundet ud af der er 28 sider med Teslaer
limit <- 28
#i in 1-28
for (i in 1:limit) {
  
  #Byg linket til siden går fra page 1-28
  url <- paste0(baseurl, i)
  
  #Gå ind på siden loopet er nået til 
  bilbasechromesession$Page$navigate(url)
  
  #Vent mellem 3 og 7 sekunder
  Sys.sleep(sample(3:7, 1))
  
  #Scrape HTML
  html <- bilbasechromesession$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  page <- read_html(html)
  
  #Find alle biler på siden ved at bruge article vi fandt tidligere
  carlist <- page %>%
    html_elements("article[data-e2e='search-result-card']")
  
  
  #Gå igennem hver bil på siden - Så det her er altså et loop efter vi har scrapet for at hente de informationer ud vi skal bruge 
  for (j in 1:length(carlist)) {
    #kører altså length af carlist (30) cars igennem hvor hver enkelt ny hedder car
    car <- carlist[[j]]
    #samme udtræk som før
    model <- car %>%
      html_element("[class*='Listing_makeModel'] h3") %>%
      html_text(trim = TRUE)
    
    variant <- car %>%
      html_element("[class*='Listing_makeModel']") %>%
      html_text(trim = TRUE)
    
    variant <- trimws(sub(model, "", variant))
    
    pris <- car %>%
      html_element("[class*='Listing_price']") %>%
      html_text(trim = TRUE)
    
    detaljer <- car %>%
      html_elements("[class*='Listing_details'] li") %>%
      html_text(trim = TRUE)
    
    aargang <- detaljer[1]
    km <- detaljer[2]
    raekkevidde <- detaljer[3]
    braendstof <- detaljer[4]
    
    beskrivelse <- car %>%
      html_element("[class*='Listing_description']") %>%
      html_text(trim = TRUE)
    
    lokation <- car %>%
      html_element("[class*='Listing_location'] span") %>%
      html_text(trim = TRUE)
    
    link <- car %>%
      html_element("a[data-e2e='search-result-link']") %>%
      html_attr("href")
    
    carid <- basename(link)
    
    forhandlerlogo <- car %>%
      html_element("img[alt='Forhandlerlogo']") %>%
      html_attr("src")
    
    #Lav bilen til én række
    tesla <- data.frame(
      carid,
      model,
      variant,
      pris,
      aargang,
      km,
      raekkevidde,
      braendstof,
      beskrivelse,
      lokation,
      link,
      forhandlerlogo
    )
    
    #Tilføj bilen til vores samlede dataframe
    teslaerfrabilbasen <- rbind(teslaerfrabilbasen, tesla)
  }
}

saveRDS(teslaerfrabilbasen, "teslaerfrabilbasen.rds")

write.csv2(teslaerfrabilbasen, "teslascrapingfrabilbasenalle")













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
library(writexl)

write_xlsx(
  teslaerfrabilbasen,
  "teslaerfrabilbasen.xlsx"
)

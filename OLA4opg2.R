# =====================================================================
# Opgave 2.2 - Gemme bilerne i databasen
#
# Skemaet er oprettet på forhånd med bilbasen_ddl.sql (opgave 2.1),
# så VI bestemmer datatyperne og ikke R-driveren. carid er primærnøgle.
# Scriptet:
#   1) renser de scrapede data
#   2) deler dem op i de fire tabeller (Forhandlere, Modeller, Biler, Priser)
#   3) laver INSERT-statements, gemmer dem i bilbasen_inserts.sql (GitHub)
#   4) kører dem i databasen
# =====================================================================

library(DBI)
library(RMySQL)
library(dplyr)

# ---------------------------------------------------------------------
# 1) Indlæs og rens data
# ---------------------------------------------------------------------
teslaerfrabilbasen <- readRDS("teslaerfrabilbasen.rds")

# Kun biler vi kunne tilgå (de 29 uden forhandlernavn er fjernet)
teslaerfrabilbasen <- teslaerfrabilbasen %>%
  filter(!is.na(forhandlernavn))

# Hvornår blev data scrapet? Ret til jeres egen scrape-dato
scrapedato <- "2026-11-11 16:50:00"

renset <- teslaerfrabilbasen %>%
  mutate(
    carid = as.integer(carid),
    # "179.900 kr." -> 179900  (fjern alt der ikke er et tal)
    pris = as.integer(gsub("[^0-9]", "", pris)),
    # "150.000 km" -> 150000
    km = as.integer(gsub("[^0-9]", "", km)),
    # 10 biler mangler rækkevidde, så "El" er havnet i raekkevidde-kolonnen
    braendstof = ifelse(raekkevidde == "El", "El", braendstof),
    raekkevidde = ifelse(raekkevidde == "El", NA, raekkevidde),
    # "614 km rækkevidde" -> 614
    raekkevidde = as.integer(gsub("[^0-9]", "", raekkevidde)),
    # "2/2021" -> "2021-02-01"
    foersteregistrering = as.character(as.Date(paste0("1/", aargang), format = "%d/%m/%Y")),
    # Uden CVR = privat sælger (navnet gemmes ikke, det er persondata)
    saelgertype = ifelse(is.na(CVR), "privat", "forhandler"),
    # "Tesla Model 3" -> "Model 3"
    modelnavn = trimws(sub("Tesla", "", model))
  )

# ---------------------------------------------------------------------
# 2) Del data op i de fire tabeller
# ---------------------------------------------------------------------

# Forhandlere: én række pr. afdeling (samme CVR kan have flere afdelinger)
Forhandlere <- renset %>%
  filter(saelgertype == "forhandler") %>%
  group_by(CVR, ForhandlerNavn = forhandlernavn, Adresse = forhandleradresse) %>%
  summarise(LogoURL = first(na.omit(forhandlerlogo)), .groups = "drop") %>%
  mutate(ForhandlerID = row_number()) %>%
  select(ForhandlerID, CVR, ForhandlerNavn, Adresse, LogoURL)

# Modeller: én række pr. model + variant
Modeller <- renset %>%
  distinct(Model = modelnavn, Variant = variant) %>%
  mutate(ModelID = row_number(), Maerke = "Tesla") %>%
  select(ModelID, Maerke, Model, Variant)

# Biler: slå ForhandlerID og ModelID op
Biler <- renset %>%
  left_join(Forhandlere, by = c("CVR",
                                "forhandlernavn" = "ForhandlerNavn",
                                "forhandleradresse" = "Adresse")) %>%
  left_join(Modeller, by = c("modelnavn" = "Model", "variant" = "Variant")) %>%
  transmute(
    CarID = carid,
    ModelID,
    ForhandlerID,
    SaelgerType = saelgertype,
    FoersteRegistrering = foersteregistrering,
    Braendstof = braendstof,
    RaekkeviddeKm = raekkevidde,
    Lokation = lokation,
    Link = link
  )

# Priser: det der kan ændre sig fra scrape til scrape (pris-tabellen)
Priser <- renset %>%
  transmute(
    CarID = carid,
    ScrapeDato = scrapedato,
    Pris = pris,
    Km = km,
    Beskrivelse = beskrivelse
  )

# Tjek: ingen biler må mangle ModelID, og carid skal være unik
stopifnot(!any(is.na(Biler$ModelID)))
stopifnot(!any(duplicated(Biler$CarID)))

#tjek hvor mange biler der mangler ModelID og carid
sum(is.na(Biler$ModelID))        # antal biler uden model
sum(duplicated(Biler$CarID))     # antal dubletter af CarID

# ---------------------------------------------------------------------
# 3) Forbind til databasen og lav INSERT-statements
# ---------------------------------------------------------------------
con <- dbConnect(
  MySQL(),
  host = "localhost",
  user = "root",
  password = "password",     # ret til jeres eget
  dbname = "bilbasen"
)
dbSendQuery(con, "SET NAMES utf8mb4")   # så æøå og emojis gemmes korrekt

# sqlAppendTable laver et INSERT INTO ... VALUES (...) ud fra en dataframe
inserts <- c(
  "USE bilbasen;",
  paste0(sqlAppendTable(con, "Forhandlere", Forhandlere, row.names = FALSE), ";"),
  paste0(sqlAppendTable(con, "Modeller", Modeller, row.names = FALSE), ";"),
  paste0(sqlAppendTable(con, "Biler", Biler, row.names = FALSE), ";"),
  paste0(sqlAppendTable(con, "Priser", Priser, row.names = FALSE), ";"),
  # Link hver bil til sin seneste pris
  "UPDATE Biler b SET SenestePrisID = (SELECT MAX(p.PrisID) FROM Priser p WHERE p.CarID = b.CarID);"
)

# Gem INSERT-statements som fil til GitHub
writeLines(enc2utf8(inserts), "bilbasen_inserts.sql", useBytes = TRUE)

# ---------------------------------------------------------------------
# 4) Kør dem i databasen
# ---------------------------------------------------------------------
for (sql in inserts[-1]) {      # spring "USE bilbasen;" over, vi er allerede forbundet
  dbSendQuery(con, sql)
}

# Tjek at det hele er kommet ind
dbGetQuery(con, "SELECT COUNT(*) AS antal FROM Forhandlere")
dbGetQuery(con, "SELECT COUNT(*) AS antal FROM Modeller")
dbGetQuery(con, "SELECT COUNT(*) AS antal FROM Biler")
dbGetQuery(con, "SELECT COUNT(*) AS antal FROM Priser")

dbGetQuery(con, "
  SELECT b.CarID, m.Model, m.Variant, f.ForhandlerNavn, p.Pris, p.Km, b.Sold
  FROM Biler b
  JOIN Modeller m ON b.ModelID = m.ModelID
  LEFT JOIN Forhandlere f ON b.ForhandlerID = f.ForhandlerID
  JOIN Priser p ON b.SenestePrisID = p.PrisID
  LIMIT 10
")

dbDisconnect(con)

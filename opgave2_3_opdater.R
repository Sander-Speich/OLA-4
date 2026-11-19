# =====================================================================
# Opgave 2.3 - Opdatere databasen ud fra den simulerede kørsel
#
#   1) Konstruer kørsel-II ud fra kørslen i opgave 1.1:
#        - scrapedato én dag senere
#        - 5 biler mangler (solgt)
#        - 3 biler har ændret pris
#        - 2 nye biler
#   2) Hent forrige kørsel fra databasen (data fra opgave 2.2)
#   3) Sammenlign og find a) nye, b) ændrede priser, c) solgte biler
#   4) Opdater databasen (solgte biler slettes ikke, men får Sold = 1)
# =====================================================================

library(DBI)
library(RMySQL)
library(dplyr)

scrapedato_ny <- "2026-11-12 16:50:00"    # én dag efter første kørsel

# ---------------------------------------------------------------------
# 1) Konstruer kørsel-II
# ---------------------------------------------------------------------

# Kørsel-I: jeres scrape fra opgave 2.1 (kun de felter der skal i Priser)
korsel1 <- readRDS("teslaerfrabilbasen.rds") %>%
  filter(!is.na(forhandlernavn)) %>%
  transmute(
    CarID = as.integer(carid),
    Pris = as.integer(gsub("[^0-9]", "", pris)),
    Km = as.integer(gsub("[^0-9]", "", km)),
    Beskrivelse = beskrivelse
  )

set.seed(2026)   # så de samme biler bliver valgt hver gang

# c) 5 biler fjernes -> de er "solgt"
solgte_id <- sample(korsel1$CarID, 5)
korsel2 <- korsel1 %>%
  filter(!CarID %in% solgte_id)

# b) 3 biler får sat prisen 5.000 kr. ned
aendret_id <- sample(korsel2$CarID, 3)
korsel2 <- korsel2 %>%
  mutate(Pris = ifelse(CarID %in% aendret_id, Pris - 5000L, Pris))

# a) 2 nye biler: kopier af 2 eksisterende biler med nyt CarID
kilde_id <- sample(korsel2$CarID, 2)
nye_biler <- korsel2 %>%
  filter(CarID %in% kilde_id) %>%
  mutate(KildeID = CarID,
         CarID = c(9000001L, 9000002L))

korsel2 <- bind_rows(korsel2, nye_biler %>% select(-KildeID)) %>%
  mutate(ScrapeDato = scrapedato_ny)

saveRDS(korsel2, "korsel2.rds")    # gem den simulerede kørsel

nrow(korsel1)   # 790
nrow(korsel2)   # 790 - 5 + 2 = 787

# ---------------------------------------------------------------------
# 2) Hent forrige kørsel fra databasen
# ---------------------------------------------------------------------
con <- dbConnect(
  MySQL(),
  host = "localhost",
  user = "root",
  password = "Mjt56qem",     # ret til jeres eget
  dbname = "bilbasen"
)
dbSendQuery(con, "SET NAMES utf8mb4")

# Sikkerhed: stop hvis kørsel-II allerede er indlæst
if (dbGetQuery(con, "SELECT COUNT(*) AS n FROM Biler WHERE Sold = 1")$n > 0) {
  stop("Kørsel-II er allerede indlæst. Kør bilbasen_ddl.sql og bilbasen_inserts.sql for at starte forfra.")
}

# Seneste pris for hver bil, der ikke er solgt
forrige <- dbGetQuery(con, "
  SELECT b.CarID, p.Pris
  FROM Biler b
  JOIN Priser p ON b.SenestePrisID = p.PrisID
  WHERE b.Sold = 0
")

# ---------------------------------------------------------------------
# 3) Sammenlign forrige kørsel med kørsel-II
# ---------------------------------------------------------------------

# a) Nye: findes i kørsel-II, men ikke i databasen
nye <- anti_join(korsel2, forrige, by = "CarID")

# b) Ændrede: findes begge steder, men prisen er forskellig
aendrede <- inner_join(korsel2, forrige, by = "CarID", suffix = c("", "_forrige")) %>%
  filter(Pris != Pris_forrige)

# c) Solgte: findes i databasen, men ikke i kørsel-II
solgte <- anti_join(forrige, korsel2, by = "CarID")

nrow(nye)        # 2
nrow(aendrede)   # 3
nrow(solgte)     # 5

nye %>% select(CarID, Pris)
aendrede %>% select(CarID, Pris_forrige, Pris)
solgte

# ---------------------------------------------------------------------
# 4) Opdater databasen
# ---------------------------------------------------------------------

# De nye biler får de faste data (model, forhandler osv.) fra den bil de er kopieret fra
kilde <- dbGetQuery(con, paste0(
  "SELECT * FROM Biler WHERE CarID IN (", paste(nye_biler$KildeID, collapse = ", "), ")"
))

nye_Biler <- nye_biler %>%
  select(NyID = CarID, KildeID) %>%
  filter(NyID %in% nye$CarID) %>%
  inner_join(kilde, by = c("KildeID" = "CarID")) %>%
  transmute(
    CarID = NyID,
    ModelID,
    ForhandlerID,
    SaelgerType,
    FoersteRegistrering,
    Braendstof,
    RaekkeviddeKm,
    Lokation,
    Link = paste0("https://www.bilbasen.dk/simuleret/", NyID)
  )

# Nye priser: for de nye biler og for bilerne med ændret pris
nye_Priser <- bind_rows(nye, aendrede) %>%
  select(CarID, ScrapeDato, Pris, Km, Beskrivelse)

opdateringer <- c(
  "USE bilbasen;",
  # a) nye biler
  paste0(sqlAppendTable(con, "Biler", nye_Biler, row.names = FALSE), ";"),
  # a) + b) nye priser
  paste0(sqlAppendTable(con, "Priser", nye_Priser, row.names = FALSE), ";"),
  # c) solgte biler markeres, de slettes ikke
  paste0("UPDATE Biler SET Sold = 1, SoldDato = '", scrapedato_ny,
         "' WHERE CarID IN (", paste(solgte$CarID, collapse = ", "), ");"),
  # alle biler peger på deres seneste pris
  "UPDATE Biler b SET SenestePrisID = (SELECT MAX(p.PrisID) FROM Priser p WHERE p.CarID = b.CarID);"
)

# Gem SQL-statements som fil til GitHub
writeLines(enc2utf8(opdateringer), "korsel2_opdatering.sql", useBytes = TRUE)

# Kør dem i databasen
for (sql in opdateringer[-1]) {
  dbSendQuery(con, sql)
}

# ---------------------------------------------------------------------
# 5) Tjek resultatet
# ---------------------------------------------------------------------
dbGetQuery(con, "SELECT COUNT(*) AS biler FROM Biler")                  
dbGetQuery(con, "SELECT COUNT(*) AS priser FROM Priser")                  
dbGetQuery(con, "SELECT COUNT(*) AS solgte FROM Biler WHERE Sold = 1")    

# Prishistorik for de ændrede biler (to priser pr. bil)
dbGetQuery(con, paste0("
  SELECT b.CarID, p.ScrapeDato, p.Pris
  FROM Biler b
  JOIN Priser p ON p.CarID = b.CarID
  WHERE b.CarID IN (", paste(aendrede$CarID, collapse = ", "), ")
  ORDER BY b.CarID, p.ScrapeDato
"))

# De solgte biler ligger der stadig, nu med Sold = 1
dbGetQuery(con, "SELECT CarID, Sold, SoldDato FROM Biler WHERE Sold = 1")

# De nye biler med deres pris
dbGetQuery(con, "
  SELECT b.CarID, m.Model, f.ForhandlerNavn, p.ScrapeDato, p.Pris
  FROM Biler b
  JOIN Modeller m ON b.ModelID = m.ModelID
  LEFT JOIN Forhandlere f ON b.ForhandlerID = f.ForhandlerID
  JOIN Priser p ON b.SenestePrisID = p.PrisID
  WHERE b.CarID IN (9000001, 9000002)
")

dbDisconnect(con)

-- =====================================================================
-- Opgave 2.1 - Skema for bilbasen (MySQL)
--
-- Invariante entiteter (ændrer sig ikke):  Forhandlere, Modeller, Biler
-- Varierende data (tidsserie):             Priser (pris-tabel)
-- Biler.SenestePrisID peger på den sidst-forekommende pris-observation
-- Biler.Sold sættes til 1 (TRUE) når bilen forsvinder fra bilbasen
-- =====================================================================

-- Create / Delete Database
DROP DATABASE IF EXISTS bilbasen;
CREATE DATABASE bilbasen CHARACTER SET utf8mb4;
USE bilbasen;


-- ---------------------------------------------------------------------
-- Forhandlere (invariant)
-- Én række pr. forhandler-afdeling. CVR er IKKE unikt: kæder som
-- Ejner Hessel har mange afdelinger med samme CVR, men andet navn/adresse.
-- Private sælgere gemmes ikke her (persondata), de får NULL på bilen.
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS Forhandlere (
    ForhandlerID    INT AUTO_INCREMENT,
    CVR             VARCHAR(8) NOT NULL,
    ForhandlerNavn  VARCHAR(150) NOT NULL,
    Adresse         VARCHAR(200),
    LogoURL         VARCHAR(500),
    PRIMARY KEY (ForhandlerID)
);


-- ---------------------------------------------------------------------
-- Modeller (invariant): mærke, model og variant
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS Modeller (
    ModelID   INT AUTO_INCREMENT,
    Maerke    VARCHAR(50)  NOT NULL,        -- 'Tesla'
    Model     VARCHAR(100) NOT NULL,        -- 'Model 3'
    Variant   VARCHAR(150),                 -- 'Long Range AWD 4d'
    PRIMARY KEY (ModelID)
);


-- ---------------------------------------------------------------------
-- Biler (invariant): det der ikke ændrer sig for en annonce.
-- CarID er bilbasens eget id fra linket (fx 7041380).
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS Biler (
    CarID                  INT,
    ModelID                INT NOT NULL,
    ForhandlerID           INT,                              -- NULL = privat sælger
    SaelgerType            ENUM('forhandler', 'privat') NOT NULL,
    FoersteRegistrering    DATE,                             -- '2/2021' -> '2021-02-01'
    Braendstof             VARCHAR(30),
    RaekkeviddeKm          INT,                              -- '614 km rækkevidde' -> 614
    Lokation               VARCHAR(150),
    Link                   VARCHAR(500) NOT NULL,
    Sold                   TINYINT(1) NOT NULL DEFAULT 0,    -- 1 = solgt (TRUE)
    SoldDato               DATETIME,                         -- scrapedato hvor den forsvandt
    SenestePrisID          INT,                              -- FK tilføjes til sidst
    PRIMARY KEY (CarID),
    FOREIGN KEY (ModelID) REFERENCES Modeller (ModelID),
    FOREIGN KEY (ForhandlerID) REFERENCES Forhandlere (ForhandlerID)
        ON DELETE SET NULL
);


-- ---------------------------------------------------------------------
-- Priser (pris-tabel / tidsserie)
-- Ny række når en bil er ny, eller når prisen har ændret sig.
-- Joines med Biler på CarID.
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS Priser (
    PrisID          INT AUTO_INCREMENT,
    CarID           INT NOT NULL,
    ScrapeDato      DATETIME NOT NULL,
    Pris            INT,                    -- '179.900 kr.' -> 179900
    Km              INT,                    -- '150.000 km'  -> 150000
    Beskrivelse     TEXT,
    PRIMARY KEY (PrisID),
    FOREIGN KEY (CarID) REFERENCES Biler (CarID)
        ON DELETE CASCADE
);


-- ---------------------------------------------------------------------
-- Link fra bilen til den sidst-forekommende pris.
-- Kan først tilføjes nu, fordi Biler og Priser peger på hinanden.
-- ---------------------------------------------------------------------
ALTER TABLE Biler
    ADD FOREIGN KEY (SenestePrisID) REFERENCES Priser (PrisID)
        ON DELETE SET NULL;

-- Indexes (gør opslag på bil + dato hurtigere)
CREATE INDEX idx_pris_bil_dato ON Priser (CarID, ScrapeDato);

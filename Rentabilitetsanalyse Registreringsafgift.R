#Analyse af om det kan betale sig at importere en tesla fra Tyskland istedet for at købe den i Danmark

# Gennemsnitspris
mean(bilbasedf_clean$pris)
mean(autoscout$pris_DKK)

# Medianpris
median(bilbasedf_clean$pris)
median(autoscout$pris_DKK)
#ligner ikke der er de vilde outliers. 

#VI tjekker med et boxplot
boxplot(bilbasedf_clean$pris,
        autoscout$pris_DKK,
        names = c("Danmark", "Tyskland"),
        main = "Tesla-priser i Danmark og Tyskland",
        ylab = "Pris i DKK")
table(bilbasedf_clean$model)
table(autoscout$model)
#tyskland har langt flere model s og x modeller, som er meget dyrere biler. 
#vigtigt ift. hvis man bare tager et simpelt gennemsnit.
#Tyder på vi nok skal lave beregningerne på modelniveau

# Gennemsnitspris for hver model i Danmark
aggregate(pris ~ model,
          data = bilbasedf_clean,
          FUN = mean)

# Gennemsnitspris for hver model i Tyskland
aggregate(pris_DKK ~ model,
          data = autoscout,
          FUN = mean)

#model 3 er dyrere i Tyskland med ca.17k
#model s er 118k dyrere i Tyskland
#model x 88k dyrere i Tyskland
#model y 4k dyrere i tyskland
#model roadster har vi slet ikke i DK. dem tager vi ikke med.


#Er der tyske biler ældre, eller nyere?
# Gennemsnitlig årgang i Danmark
aggregate(aargang ~ model,
          data = bilbasedf_clean,
          FUN = mean)

# Gennemsnitlig årgang i Tyskland
aggregate(aargang ~ model,
          data = autoscout,
          FUN = mean)

#tyske model s og x er ca. 2-3 år nyere
#gennemsnitligt 0,5 år nyere på tværs af modellerne



#Så ift. kørte kilometer


# Gennemsnitlig kilometerstand i Danmark
aggregate(km ~ model,
          data = bilbasedf_clean,
          FUN = mean)

# Gennemsnitlig kilometerstand i Tyskland
aggregate(km ~ model,
          data = autoscout,
          FUN = mean)

#tyske teslaer har også kørt mindre
#model 3 -23k km
#model s -32k km
#model x -50k km
#model y -17k km


#det var lidt data exploration så har vi et overblik.

#samler de relevante data til vores importanalyse
# Vælg relevante kolonner fra Danmark
dk <- bilbasedf_clean[, c("model", "pris", "aargang", "km")]
dk$land <- "Danmark"

# Vælg relevante kolonner fra Tyskland
de <- autoscout[, c("model", "pris_DKK", "aargang", "km")]
names(de)[2] <- "pris"
de$land <- "Tyskland"

# Saml de to datasæt
tesla <- rbind(dk, de)

# Undersøg resultatet
str(tesla)
table(tesla$land)

tesla <- tesla[
  trimws(tesla$model) != "Tesla Roadster",
]
#vi fjerner de 7 roadsters fra autoscout da de ikke findes i DK

names(autoscout)

sort(unique(autoscout$variant))

#variantnavnene er alt for utydelige og rodede. Vi kan ikke sortere på dem, det er noget rod
#vi laver registreringsafgift-beregningen på model niveau med et groft estimat på tværs af varianterne af modellerne


#ifølge kildelisten i vores rapport er der en grænse ved 420.000kr. nuværende handelsværdi i DK før der kommer afgift på ca. 5% på en bil
#og over 550.000kr. afgift på 17,5% ca. (har taget medianen af intervallerne)

#vi tager altså nu og sammenligner de tyske biler med de danske og går ind og finder en bil der har kørt +- 30.000km og er +-1år indenfor den tyskebil
#dette gør vi for at de har nogenlunde samme værdi niveau, og så vi får den danske handelsværdi lige nu

tesla$handelsvaerdi_DK <- NA
tesla$antal_match <- 0


for (i in which(tesla$land == "Tyskland")) {
  
  match <- tesla[
    tesla$land == "Danmark" &
      tesla$model == tesla$model[i] &
      abs(tesla$aargang - tesla$aargang[i]) <= 1 &
      abs(tesla$km - tesla$km[i]) <= 30000,
  ]
  
  tesla$antal_match[i] <- nrow(match)
  
  if (nrow(match) > 0) {
    tesla$handelsvaerdi_DK[i] <- median(match$pris, na.rm = TRUE)
  }
}

table(tesla$land)

table(tesla$antal_match[tesla$land == "Tyskland"] > 0)

#vi fik faktisk 828 af dem til at matche en dansk handelsværdi, vi fjerner NA'erne.
# Fjern tyske biler uden dansk handelsværdi
tesla <- tesla[tesla$land == "Danmark" |
                 !is.na(tesla$handelsvaerdi_DK), ]

# Opret kolonne med registreringsafgift
tesla$registreringsafgift <- ifelse(
  tesla$handelsvaerdi_DK <= 420000, 0,
  ifelse(tesla$handelsvaerdi_DK <= 550000, 0.05, 0.175)
)

# Omregn procent til kroner
tesla$registreringsafgift <- tesla$registreringsafgift *
  tesla$handelsvaerdi_DK

#artiklen omtaler en fast ca. omkostning på 10.000kr. ved at indregistrere en bil bla. ift. nummerplader osv..
# Fast importomkostning på 10.000 kr.
tesla$importomkostninger <- 10000


# Total importpris for tyske biler
tesla$importpris_DKK <- NA_real_

tesla$importpris_DKK[tesla$land == "Tyskland"] <-
  tesla$pris[tesla$land == "Tyskland"] +
  tesla$registreringsafgift[tesla$land == "Tyskland"] +
  tesla$importomkostninger[tesla$land == "Tyskland"]

# Beregn besparelse for de tyske biler
tesla$besparelse <- tesla$handelsvaerdi_DK - tesla$importpris_DKK

# Antal biler hvor import kan betale sig
table(tesla$besparelse > 0)


# Fordeling på modeller
table(tesla$model[tesla$besparelse > 0])


# Kun biler hvor import er billigere
rentable <- tesla[tesla$besparelse > 0 & !is.na(tesla$besparelse), ]

# Sortér efter størst besparelse
rentable <- rentable[order(-rentable$besparelse), ]

# Vis top 10
head(rentable[, c("model", "aargang", "km", "pris",
                  "handelsvaerdi_DK", "importpris_DKK",
                  "besparelse")], 10)


# Procentdel rentable importer fordelt på model
round(prop.table(
  table(tesla$model[tesla$land == "Tyskland"],
        tesla$besparelse[tesla$land == "Tyskland"] > 0),
  margin = 1
) * 100, 1)


aggregate(besparelse ~ model,
          data = rentable,
          FUN = mean)

write.csv2(tesla,"tesladatasæt.csv")



# Multipel lineær regression

model1 <- lm(
  pris ~ land + model + aargang + km,
  data = tesla
)

# Se resultater
summary(model1)



#Vi prøver analyserne for rentabilitet på import for tyske teslaer efter regression

# Kun tyske biler
tyskland <- tesla[tesla$land == "Tyskland", ]

# Forestil os, at de tyske biler sælges i Danmark
tyskland$land <- "Danmark"

# Forudsagt dansk pris fra regressionen
tesla$handelsvaerdi_regression <- NA_real_

tesla$handelsvaerdi_regression[tesla$land == "Tyskland"] <-
  predict(model1, newdata = tyskland)


# Registreringsafgift baseret på regression
tesla$afgift_regression <- ifelse(
  tesla$handelsvaerdi_regression <= 420000, 0,
  ifelse(
    tesla$handelsvaerdi_regression <= 550000, 0.05, 0.175
  )
)

# Omregn til kroner
tesla$afgift_regression <-
  tesla$afgift_regression * tesla$handelsvaerdi_regression

# Samlet importpris
tesla$importpris_regression <-
  tesla$pris + tesla$afgift_regression + 10000

# Besparelse efter regression
tesla$besparelse_regression <-
  tesla$handelsvaerdi_regression - tesla$importpris_regression


# Hvor mange biler er rentable med hver metode?

table(tesla$besparelse > 0, useNA = "ifany")

table(tesla$besparelse_regression > 0, useNA = "ifany")


# Rentable importer efter regression
table(tesla$model[tesla$besparelse_regression > 0])




# ---- Figur 7: Matching vs. regression ----

# 1. Kun tyske biler
tyskland <- tesla[tesla$land == "Tyskland", ]

# 2. Andel rentable med matching
matching <- aggregate(
  besparelse ~ model,
  data = tyskland,
  FUN = function(x) mean(x > 0) * 100
)

# 3. Andel rentable med regression
regression <- aggregate(
  besparelse_regression ~ model,
  data = tyskland,
  FUN = function(x) mean(x > 0) * 100
)

# 4. Saml resultaterne
figur7 <- rbind(
  data.frame(model = matching$model,
             procent = matching$besparelse,
             metode = "Matching"),
  data.frame(model = regression$model,
             procent = regression$besparelse_regression,
             metode = "Regression")
)

# 5. Forkort modelnavne
figur7$model <- sub("Tesla ", "", figur7$model)

# 6. Figur
ggplot(figur7, aes(x = model, y = procent, fill = metode)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste0(dk(procent, 0.1), " %")),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Matching" = farve_blaa,
    "Regression" = farve_orange
  )) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, " %"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Regression ændrer markant hvilke Teslaer der er rentable at importere",
    subtitle = "Andel rentable importer efter to metoder til estimering af dansk handelsværdi",
    x = "Tesla-model",
    y = "Andel rentable importer",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Begge metoder inkluderer estimeret registreringsafgift ",
      "og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk()


table(
  Matching = tyskland$besparelse > 0,
  Regression = tyskland$besparelse_regression > 0
)




# ---- Figur 8: Enighed mellem metoderne ----

# 1. Kun tyske biler
tyskland <- tesla[tesla$land == "Tyskland", ]

# 2. Klassificer bilerne
tyskland$vurdering <- ifelse(
  tyskland$besparelse > 0 &
    tyskland$besparelse_regression > 0,
  "Begge rentable",
  ifelse(
    tyskland$besparelse <= 0 &
      tyskland$besparelse_regression <= 0,
    "Begge urentable",
    ifelse(
      tyskland$besparelse_regression > 0,
      "Kun regression rentabel",
      "Kun matching rentabel"
    )
  )
)

# 3. Beregn antal og procent
figur8 <- as.data.frame(table(tyskland$vurdering))
names(figur8) <- c("vurdering", "antal")

figur8$procent <- figur8$antal / sum(figur8$antal) * 100

figur8$vurdering <- factor(
  figur8$vurdering,
  levels = c(
    "Begge rentable",
    "Begge urentable",
    "Kun regression rentabel",
    "Kun matching rentabel"
  )
)

# 4. Visualisering
ggplot(figur8, aes(x = vurdering, y = procent,
                   fill = vurdering)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(
      antal, " biler\n",
      dk(procent, 0.1), " %"
    )),
    vjust = -0.35,
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(values = c(
    "Begge rentable" = farve_blaa,
    "Begge urentable" = "#B3B3B3",
    "Kun regression rentabel" = farve_orange,
    "Kun matching rentabel" = "#E5A17E"
  )) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10),
    labels = function(x) paste0(x, " %"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Hver tredje Tesla får en anden vurdering afhængigt af metoden",
    subtitle = "Sammenligning af matching og lineær regression for 828 tyske Teslaer",
    x = NULL,
    y = "Andel af tyske Teslaer",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Begge metoder anvender samme forenklede afgiftsmodel ",
      "og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10)
  )




# ---- Figur 8: Rentabilitet efter regression ----

# 1. Kun tyske biler med regressionsresultat
tyskland <- tesla[
  tesla$land == "Tyskland" &
    !is.na(tesla$besparelse_regression), ]

# 2. Beregn antal
antal <- c(
  "Kan betale sig" = sum(tyskland$besparelse_regression > 0),
  "Kan ikke betale sig" = sum(tyskland$besparelse_regression <= 0)
)

figur8 <- data.frame(
  resultat = names(antal),
  antal = as.numeric(antal)
)

figur8$procent <- figur8$antal / sum(figur8$antal) * 100

figur8$resultat <- factor(
  figur8$resultat,
  levels = c("Kan betale sig", "Kan ikke betale sig")
)

# 3. Tekst på søjlerne
figur8$tekst <- paste0(
  figur8$antal, " biler\n",
  dk(figur8$procent, 0.1), " %"
)

# 4. Visualisering
ggplot(figur8, aes(x = resultat, y = procent, fill = resultat)) +
  geom_col(width = 0.58) +
  geom_text(
    aes(label = tekst),
    vjust = -0.4,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Kan betale sig" = farve_blaa,
    "Kan ikke betale sig" = farve_orange
  )) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, " %"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "407 ud af 828 tyske Teslaer er rentable ifølge regressionen",
    subtitle = "49,2 % er rentable efter estimeret afgift og importomkostninger",
    x = NULL,
    y = "Andel af tyske Teslaer",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Dansk handelsværdi er estimeret med lineær regression ",
      "ud fra land, model, årgang og kilometerstand. ",
      "Importprisen inkluderer forenklet registreringsafgift ",
      "og 10.000 kr. i faste omkostninger."
    )
  ) +
  tema_dk() +
  theme(legend.position = "top")


# ---- Figur 9: Top 10 og bund 10 efter regression ----

# 1. Sortér de tyske biler efter besparelse
sorteret <- tyskland[
  order(tyskland$besparelse_regression), ]

# 2. Find top 10 og bund 10
bund10 <- head(sorteret, 10)
top10 <- tail(sorteret, 10)

# 3. Saml de 20 biler
figur9 <- rbind(top10, bund10)

# 4. Opret grupper
figur9$gruppe <- rep(
  c("Top 10 – største besparelser",
    "Bund 10 – største tab"),
  each = 10
)

# 5. Lav labels med model og årgang
figur9$bil <- paste0(
  sub("Tesla ", "", figur9$model),
  " (", figur9$aargang, ")"
)

# Tilføj nummer, så alle biler kan adskilles
figur9$bil <- paste0(
  seq_len(nrow(figur9)), ". ", figur9$bil
)

# 6. Beløb i tusinde kroner
figur9$beloeb <- abs(figur9$besparelse_regression) / 1000

figur9$bil <- factor(
  figur9$bil,
  levels = figur9$bil[order(figur9$beloeb)]
)

# 7. Visualisering
ggplot(figur9, aes(x = bil, y = beloeb, fill = gruppe)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(dk(beloeb, 0.1), " t.kr.")),
    hjust = -0.1,
    size = 3.2,
    fontface = "bold"
  ) +
  coord_flip() +
  facet_wrap(~ gruppe, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(
    "Top 10 – største besparelser" = farve_blaa,
    "Bund 10 – største tab" = farve_orange
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Regression afslører de største mulige importgevinster og tab",
    subtitle = "De 10 mest og mindst rentable Teslaer baseret på regressionsestimeret dansk pris",
    x = NULL,
    y = "Besparelse eller tab (1.000 kr.)",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Dansk handelsværdi er estimeret med lineær regression. ",
      "Importprisen inkluderer forenklet registreringsafgift ",
      "og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(
      colour = "#B3B3B3", fill = NA, linewidth = 0.8
    ),
    panel.spacing = grid::unit(1.5, "lines")
  )


# ---- Figur 2: Rentabel import fordelt på model og årgang ----

# 1. Kun tyske biler
tyskland <- tesla[tesla$land == "Tyskland" &
                    !is.na(tesla$besparelse), ]

# 2. Beregn procentdel rentable pr. model og årgang
figur2 <- aggregate(
  besparelse ~ model + aargang,
  data = tyskland,
  FUN = function(x) mean(x > 0) * 100
)

names(figur2)[3] <- "procent"

# 3. Forkort modelnavne
figur2$model <- sub("Tesla ", "", figur2$model)

# 4. Fast rækkefølge på modeller
figur2$model <- factor(
  figur2$model,
  levels = c("Model 3", "Model S", "Model X", "Model Y")
)

# 5. Lav søjlediagrammer
ggplot(figur2, aes(x = aargang, y = procent)) +
  geom_col(fill = farve_blaa, width = 0.75) +
  geom_text(
    aes(label = paste0(dk(procent, 0.1), " %")),
    vjust = -0.4,
    size = 3
  ) +
  facet_wrap(
    ~ model,
    ncol = 2,
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, " %")
  ) +
  scale_x_continuous(
    breaks = seq(min(figur2$aargang), max(figur2$aargang), 1)
  ) +
  labs(
    title = "Model X er oftest rentabel at importere – men årgangen gør en forskel",
    subtitle = "Andel rentable importer fordelt på Tesla-model og årgang",
    x = "Årgang",
    y = "Andel rentable importer",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Rentabilitet er beregnet efter estimeret registreringsafgift ",
      "og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk() +
  theme(
    # Ramme omkring hvert diagram
    panel.border = element_rect(
      colour = "#B3B3B3",
      fill = NA,
      linewidth = 1
    ),
    
    # Luft mellem diagrammerne
    panel.spacing = unit(1.5, "lines"),
    
    # Tydelige modeloverskrifter
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    
    # Årstal under hver søjle
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 9
    ),
    
    panel.grid.major.x = element_blank()
  )













# ---- Figur 3: Andel rentable importer pr. model ----

# 1. Beregn andelen rentable biler pr. model
figur3 <- aggregate(
  besparelse ~ model,
  data = tyskland,
  FUN = function(x) mean(x > 0) * 100
)

names(figur3)[2] <- "procent"

# 2. Forkort modelnavne
figur3$model <- sub("Tesla ", "", figur3$model)

# 3. Sortér fra højeste til laveste procent
figur3 <- figur3[order(-figur3$procent), ]

figur3$model <- factor(figur3$model, levels = figur3$model)

# 4. Overskrift med resultat
titel3 <- paste0(
  figur3$model[1], " er mest rentabel at importere – ",
  dk(figur3$procent[1], 0.1), " % kan betale sig"
)

# 5. Visualisering
ggplot(figur3, aes(x = model, y = procent)) +
  geom_col(fill = farve_blaa, width = 0.65) +
  geom_text(
    aes(label = paste0(dk(procent, 0.1), " %")),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, " %"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = titel3,
    subtitle = "Andel af tyske Teslaer, hvor import er billigere end køb i Danmark",
    x = "Tesla-model",
    y = "Andel rentable importer",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Rentabilitet er beregnet efter estimeret registreringsafgift ",
      "og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk()






# ---- Figur 4: Top 10 besparelser og tab ----

# 1. Kun tyske biler
tyskland <- tesla[tesla$land == "Tyskland" &
                    !is.na(tesla$besparelse), ]

# 2. Sortér efter besparelse
sorteret <- tyskland[order(tyskland$besparelse), ]

# 3. Find top 10 og bund 10
bund10 <- head(sorteret, 10)
top10 <- tail(sorteret, 10)

# 4. Saml de 20 biler
figur4 <- rbind(top10, bund10)

# 5. Opret kategorier
figur4$gruppe <- ifelse(
  figur4$besparelse > 0,
  "Top 10 – største besparelser",
  "Bund 10 – største tab"
)

# 6. Lav labels med model og årgang
figur4$bil <- paste0(
  sub("Tesla ", "", figur4$model),
  " (", figur4$aargang, ")"
)

# Tilføj nummer, så biler med samme model og årgang adskilles
figur4$bil <- paste0(
  seq_len(nrow(figur4)), ". ", figur4$bil
)

# 7. Omregn til tusinde kroner
figur4$beloeb <- abs(figur4$besparelse) / 1000

# 8. Sortér labels efter beløb
figur4$bil <- factor(
  figur4$bil,
  levels = figur4$bil[order(figur4$beloeb)]
)

# 9. Visualisering
ggplot(figur4, aes(x = bil, y = beloeb, fill = gruppe)) +
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
    title = "De største importbesparelser overstiger 100.000 kr.",
    subtitle = "De 10 mest og mindst rentable tyske Teslaer efter importomkostninger",
    x = NULL,
    y = "Besparelse eller tab (1.000 kr.)",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Beregnet som dansk handelsværdi minus samlet importpris, ",
      "inklusive estimeret registreringsafgift og 10.000 kr. i faste omkostninger."
    )
  ) +
  tema_dk() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(
      colour = "#B3B3B3", fill = NA, linewidth = 0.8
    ),
    panel.spacing = unit(1.5, "lines")
  )








# ---- Figur 5: Gennemsnitlig besparelse pr. model ----

# 1. Kun rentable tyske biler
rentable <- tesla[
  tesla$land == "Tyskland" &
    !is.na(tesla$besparelse) &
    tesla$besparelse > 0, ]

# 2. Beregn gennemsnitlig besparelse pr. model
figur5 <- aggregate(
  besparelse ~ model,
  data = rentable,
  FUN = mean
)

# 3. Forkort modelnavne
figur5$model <- sub("Tesla ", "", figur5$model)

# 4. Sortér efter besparelse
figur5 <- figur5[order(-figur5$besparelse), ]

figur5$model <- factor(
  figur5$model,
  levels = rev(figur5$model)
)

# 5. Lav labels
figur5$tekst <- paste0(
  dk(figur5$besparelse, 1), " kr."
)

# 6. Lav dynamisk overskrift
titel5 <- paste0(
  as.character(figur5$model[which.max(figur5$besparelse)]),
  " giver den største gennemsnitlige importbesparelse"
)

# 7. Visualisering
ggplot(figur5, aes(x = model, y = besparelse)) +
  geom_col(fill = farve_blaa, width = 0.65) +
  geom_text(
    aes(label = tekst),
    hjust = -0.1,
    size = 4.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(figur5$besparelse) * 1.35),
    labels = function(x) dk(x, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = titel5,
    subtitle = "Gennemsnitlig besparelse på tyske Teslaer, hvor import er rentabel",
    x = NULL,
    y = "Gennemsnitlig besparelse (DKK)",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Kun rentable importer indgår. Besparelsen er beregnet efter ",
      "estimeret registreringsafgift og 10.000 kr. i faste importomkostninger."
    )
  ) +
  tema_dk()








# ---- Figur 6: Prisforskel før og efter regression ----

# 1. Gennemsnitspriser på samme datasæt
priser <- aggregate(pris ~ land, data = tesla, FUN = mean)

# 2. Rå prisforskel (Tyskland minus Danmark)
raa_forskel <- priser$pris[priser$land == "Tyskland"] -
  priser$pris[priser$land == "Danmark"]

# 3. Justeret prisforskel fra regressionsmodellen
justeret_forskel <- coef(model1)["landTyskland"]

# 4. Saml resultater
figur6 <- data.frame(
  metode = c("Før korrektion", "Efter korrektion"),
  forskel = c(raa_forskel, justeret_forskel)
)

figur6$metode <- factor(
  figur6$metode,
  levels = c("Efter korrektion", "Før korrektion")
)

figur6$farve <- ifelse(
  figur6$forskel > 0,
  "Dyrere i Tyskland",
  "Billigere i Tyskland"
)

# 5. Labels i kroner
figur6$tekst <- paste0(
  ifelse(figur6$forskel > 0, "+", ""),
  dk(figur6$forskel, 1), " kr."
)

# 6. Dynamisk overskrift
titel6 <- paste0(
  "Prisforskellen vender fra +",
  dk(raa_forskel, 1),
  " kr. til ",
  dk(justeret_forskel, 1),
  " kr."
)

# 7. Figur
ggplot(figur6, aes(x = metode, y = forskel, fill = farve)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_text(
    aes(label = tekst),
    hjust = ifelse(figur6$forskel > 0, -0.1, 1.1),
    size = 5,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(
    "Dyrere i Tyskland" = farve_orange,
    "Billigere i Tyskland" = farve_blaa
  )) +
  scale_y_continuous(
    labels = function(x) paste0(dk(x, 1), " kr."),
    expand = expansion(mult = c(0.2, 0.2))
  ) +
  labs(
    title = titel6,
    subtitle = paste0(
      "Prisforskel mellem Tyskland og Danmark. ",
      "Efter korrektion er der taget højde for model, årgang og km."
    ),
    x = NULL,
    y = "Prisforskel (Tyskland minus Danmark)",
    caption = paste0(
      "Kilde: AutoScout24 og Bilbasen, egne beregninger. ",
      "Før: forskel i gennemsnitlige annoncepriser. ",
      "Efter: landekoefficient fra multipel lineær regression. ",
      "Begge beregninger bygger på samme datasæt."
    )
  ) +
  tema_dk() +
  theme(legend.position = "none")

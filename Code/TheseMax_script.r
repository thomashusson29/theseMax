
# ----LIBRARY ----

library(cardx)
library(dplyr)
library(readxl)
library(ggpattern)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(scales)
library(summarytools)
library(MatchIt)
library(optmatch)
library(purrr)
library(officer)
library(flextable)
library(gt)
library(mice)
library(googlesheets4)
library(cards)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(forcats)
library(dlstats)
library(pkgsearch)
library(pROC)
library(stats)
library(parameters)
library(broom.helpers)
library(forestplot)
library(kableExtra)
library(rsconnect)
library(pacman)
library(stringr)

# Paragraphe méthodes


# Import de la base de données

gs4_deauth()
df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1eWwPK8G89G6nWTDzWimcCa8EOWqval8RPvwykZmfGoI/edit?gid=803820517#gid=803820517",
  sheet = "CopieThomas"
)

# Statistiques descriptives

#Factor l'Âge dans le bon ordre : 
df <- df %>%
  mutate(`3_Age` = factor(`3_Age`,
                          levels = c("Entre 30 et 39 ans",
                                     "Entre 40 et 49 ans",
                                     "Entre 50 et 59 ans",
                                     "Entre 60 et 69 ans",
                                     "Plus de 70 ans")))


#Factor la durée d'installation dans le bon ordre 
df <- df %>%
  mutate(`6_Duree_d_installation` = factor(`6_Duree_d_installation`,
                                           levels = c("Moins de 5 ans",
                                                      "Entre 5 et 9 ans",
                                                      "Entre 10 et 19 ans",
                                                      "Plus de 20 ans")))



#Factor les types d'activité dans le bon ordre 
df <- df %>%
  mutate(`7_Type_d_activite` = factor(`7_Type_d_activite`,
                                      levels = c("Exclusivement libéral en cabinet",
                                                 "Essentiellement libéral avec activité universitaire",
                                                 "Essentiellement libéral avec activité de régulation/PDSA",
                                                 "Mixte (libéral + hospitalière)",
                                                 "Autre")))

# Séparation des coordonnées (latitude / longitude)

df <- df %>%
  separate(`5_coordonnees_gps`, into = c("lat", "lon"), sep = ",", remove = FALSE) %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )
#Camembert connaissance MCS
slices <- table(df$`1_Connaissance_MCS_binaire`)
labels <- c("Non", "Oui")
pct <- round(slices / sum(slices) * 100)
labels <- paste(labels, pct) # Ajoute les pourcentages aux labels
labels <- paste(labels, "%", sep = "") # Ajoute le symbole %
pie(slices,
    main = "Connaissance du réseau MCS parmi les médecins généralistes",
    col = c("lightcoral", "lightgreen"),
    labels = labels
)
#Camembert sexe
slices <- table(df$`2_Sexe_Homme`)
labels <- c("Homme", "Femme")
pct <- round(slices / sum(slices) * 100)
labels <- paste(labels, pct) # Ajoute les pourcentages aux labels
labels <- paste(labels, "%", sep = "") # Ajoute le symbole %
pie(slices,
    main = "Répartition par sexe des médecins généralistes",
    col = c("lightblue", "lightpink"),
    labels = labels
)
#Histogramme âge
ggplot(df, aes(x = `3_Age`)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Répartition par âge des médecins généralistes",
       x = "Tranche d'âge",
       y = "Nombre de médecins") +
  theme_minimal()
library(leaflet)
leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = "blue",
    fillOpacity = 0.6,
    popup = ~paste0("<b>Coordonnées :</b> ", `5_coordonnees_gps`)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("blue"),
    labels = c("Médecins généralistes"),
    title = "Lieu d'installation des médecins"
  )
#Histogramme durée d'installation
ggplot(df %>% filter(!is.na(`6_Duree_d_installation`)), aes(x = `6_Duree_d_installation`)) +
  geom_bar(fill = "lightgray", color = "black") +
  labs(title = "Répartition par durée d'installation des médecins généralistes",
       x = "Durée d'installation",
       y = "Nombre de médecins") +
  theme_minimal()
#Histogramme type d'activité
ggplot(df, aes(x = `7_Type_d_activite`, fill = `7_Type_d_activite`)) +
  geom_bar(color = "black") +
  labs(
    title = "Répartition par type d'activité des médecins généralistes",
    x = NULL,
    y = "Nombre de médecins",
    fill = "Type d'activité"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(), # supprime le texte trop long sur l'axe
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  # légende plus petite
    legend.title = element_text(size = 8) # titre de la légende légèrement réduit aussi
)
#Camembert type d'activité
library(ggplot2)


# 1. Extraire les modalités (niveaux uniques)

types <- unique(df$`7_Type_d_activite`)
n <- length(types)


# 2. Générer la même palette que ggplot

palette_type_activite <- scales::hue_pal()(n)


# Comptage

slices <- table(df$`7_Type_d_activite`)
pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert avec la même palette que ggplot

pie(slices,
    main = "Répartition par type d'activité des médecins généralistes",
    col = palette_type_activite,
    labels = labels,
    border = "black"
)
library(dplyr)
library(tidyr)
library(ggplot2)


# Nombre total de médecins

n_medecins <- nrow(df)


# Préparation des données

df_long <- df %>%
  select(starts_with("8_")) %>%   # capte toutes les colonnes "8_"
  pivot_longer(
    cols = everything(),
    names_to = "Type_de_visite",
    values_to = "Valeur"
  ) %>%
  filter(Valeur == 1) %>%
  group_by(Type_de_visite) %>%
  summarise(Count = n()) %>%
  mutate(
    Pourcentage = round(100 * Count / n_medecins, 1),
    Type_de_visite = recode(
      Type_de_visite,
      "8__consultations_rdv" = "Consultations sur RDV",
      "8__consultations_sans_rdv_" = "Consultations sans RDV",
      "8_consultations_:_creneaux_d_urgence" = "Créneaux d'urgence",
      "8_Visites" = "Visites à domicile",
      "8_Cs_autre" = "Autres consultations"
    ),
    Type_de_visite = factor(
      Type_de_visite,
      levels = c("Consultations sur RDV",
                 "Consultations sans RDV",
                 "Créneaux d'urgence",
                 "Visites à domicile",
                 "Autres consultations")
    )
  )


# Graphique

ggplot(df_long, aes(x = Type_de_visite, y = Count, fill = Type_de_visite)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(Count, " (", Pourcentage, "%)")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Types de visites effectuées par les médecins généralistes",
    x = "Type de visite",
    y = "Nombre de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "none"
  )

# Palette de couleurs cohérente

palette_ressenti <- c(
  "Délai non gênant" = "#7ED957",                      # vert clair
  "Plutôt bien mais parfois gêné" = "#FFC94A", # orange doux
  "Délai trop long, en difficulté" = "#FF6B6B",# rouge
  "Autre" = "grey70"                           # gris neutre
)


# Calcul des pourcentages avec ordre défini, en excluant les NA

df_plot <- df %>%
  filter(!is.na(`9_Ressenti_delai_SMUR`)) %>%
  count(`9_Ressenti_delai_SMUR`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `9_Ressenti_delai_SMUR` = factor(
      `9_Ressenti_delai_SMUR`,
      levels = c(
        "Délai non gênant",
        "Plutôt bien mais parfois gêné",
        "Délai trop long, en difficulté",
        "Autre"
      )
    )
  )


# Graphique final dans l'ordre voulu

ggplot(df_plot, aes(x = `9_Ressenti_delai_SMUR`, y = pct, fill = `9_Ressenti_delai_SMUR`)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_ressenti, guide = "none") +
  labs(
    title = "Ressenti des médecins sur le délai d'intervention du SMUR",
    x = "Ressenti du délai",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


# Palette de couleurs cohérente

palette_ressenti <- c(
  "Délai non gênant" = "#7ED957",                      # vert clair
  "Plutôt bien mais parfois gêné" = "#FFC94A", # orange doux
  "Délai trop long, en difficulté" = "#FF6B6B",# rouge
  "Autre" = "grey70"                           # gris neutre
)


# Comptage

slices <- table(df$`9_Ressenti_delai_SMUR`)
pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert avec la palette définie

pie(slices,
    main = "Ressenti des médecins sur le délai d'intervention du SMUR",
    col = palette_ressenti[names(slices)],
    labels = labels,
    border = "black"
)

# Palette des couleurs

palette_ressenti <- c(
  "Délai non gênant" = "#7ED957",                      # vert clair
  "Plutôt bien mais parfois gêné" = "#FFC94A", # orange doux
  "Délai trop long, en difficulté" = "#FF6B6B",# rouge
  "Autre" = "grey70"                           # gris neutre
)

library(leaflet)
library(dplyr)


# Nettoyage des coordonnées invalides

df <- df %>% filter(!is.na(lat), !is.na(lon))


# Carte avec fond grisé

leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # fond gris clair
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = ~ifelse(`9_Ressenti_delai_SMUR` %in% names(palette_ressenti),
                    palette_ressenti[`9_Ressenti_delai_SMUR`],
                    palette_ressenti["Autre"]),
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Ressenti :</b> ", `9_Ressenti_delai_SMUR`, "<br>",
      "<b>Coordonnées :</b> ", `5_coordonnees_gps`
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = palette_ressenti,
    labels = names(palette_ressenti),
    title = "Ressenti sur le délai"
  )

# Palette de couleurs du bleu (non) au rouge (oui)

palette_perte_chance <- c(
  "Non, pas du tout" = "#6BD6FF",
  "Plutôt non" = "#A5E887",
  "Plutôt oui" = "#FFD966",
  "Oui, tout à fait" = "#FF6B6B"
)


# Calcul des pourcentages + ordre + suppression des NA

df_plot <- df %>%
  filter(!is.na(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`)) %>%
  count(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur` = factor(
      `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )


# Graphique

ggplot(
  df_plot,
  aes(x = `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`,
      y = pct,
      fill = `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_perte_chance, guide = "none") +
  labs(
    title = "Perte de chance perçue par les médecins liée au délai d'intervention du SMUR (excluant les NA)",
    x = "Perte de chance perçue",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Palette de couleurs du bleu (non) au rouge (oui)

palette_perte_chance <- c(
  "Non, pas du tout" = "#6BD6FF",
  "Plutôt non" = "#A5E887",
  "Plutôt oui" = "#FFD966",
  "Oui, tout à fait" = "#FF6B6B"
)


# Comptage

slices <- table(df$`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`)
pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert avec la palette définie

pie(slices,
    main = "Perte de chance perçue liée au délai d'intervention du SMUR",
    col = palette_perte_chance[names(slices)],
    labels = labels,
    border = "black"
)
#palette : 
palette_perte_chance <- c(
  "Non, pas du tout" = "#6BD6FF",
  "Plutôt non" = "#A5E887",
  "Plutôt oui" = "#FFD966",
  "Oui, tout à fait" = "#FF6B6B"
)


# Carte avec fond grisé

leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # fond gris clair
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = ~ifelse(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur` %in% names(palette_perte_chance),
                    palette_perte_chance[`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`],
                    "grey70"),
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Perte de chance :</b> ", `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`, "<br>",
      "<b>Coordonnées :</b> ", `5_coordonnees_gps`
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = palette_perte_chance,
    labels = names(palette_perte_chance),
    title = "Perte de chance perçue"
  )
cols_to_include2 <- c(
  "9_Ressenti_delai_SMUR",
  "10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur"
)
	
#Ordonner les facteurs pour lisibilité
df <- df %>%
  mutate(`9_Ressenti_delai_SMUR` = factor(`9_Ressenti_delai_SMUR`,
                                          levels = c("Délai non gênant",
                                                     "Plutôt bien mais parfois gêné",
                                                     "Délai trop long, en difficulté",
                                                     "Autre")))
df <- df %>%
  mutate(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur` = factor(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`,
                                                                                   levels = c("Non, pas du tout","Plutôt non","Plutôt oui","Oui, tout à fait")))

table2 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include2), # colonnes à inclure
    statistic = list(
      all_categorical() ~ "{n} ({p}%)" # n (%) pour les variables catégorielles
    ),
    label = list(
      `9_Ressenti_delai_SMUR` = "Ressenti sur le délai d'intervention du SMUR",
      `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur` = "Perte de chance dans votre secteur liée au délai d'intervention"
    ),
    missing = "no" # ne pas inclure les valeurs manquantes dans le tableau
  ) %>%
  modify_header(label = "**Caractéristiques**") %>% # modifier l'en-tête de la colonne des labels
  bold_labels() # mettre en gras les labels des variables

table2
slices <- table(df$`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire`)
labels <- c("Non", "Oui")
pct <- round(slices / sum(slices) * 100)
labels <- paste(labels, pct) # Ajoute les pourcentages aux labels
labels <- paste(labels, "%", sep = "") # Ajoute le symbole %
pie(slices,
    main = "Perte de chance binaire liée au délai d'intervention",
    col = c("lightcoral", "lightgreen"),
    labels = labels
)
df <- df %>%
  mutate(
    iso = factor(`4_Profession_isolee`, levels = c(0, 1),
                 labels = c("Non isolé", "Isolé")),
    ressenti4 = fct_relevel(
      factor(`9_Ressenti_delai_SMUR`),
      "Délai non gênant",
      "Plutôt bien mais parfois gêné",
      "Délai trop long, en difficulté",
      "Autre"
    ),
    ressenti_bin = if_else(
      `9_Ressenti_delai_SMUR` %in% c("Plutôt bien mais parfois gêné",
                                     "Délai trop long, en difficulté"),
      1L, 0L
    ),
    perte_cat = fct_relevel(
      factor(`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur`),
      "Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait"
    ),
    perte_bin = if_else(
      perte_cat %in% c("Plutôt oui", "Oui, tout à fait"), 1L, 0L
    )
  )
#Palette de couleurs
palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)


# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`11_Reseau_MCS_pertinent_pour_La_Reunion`)) %>%
  count(`11_Reseau_MCS_pertinent_pour_La_Reunion`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `11_Reseau_MCS_pertinent_pour_La_Reunion` = factor(
      `11_Reseau_MCS_pertinent_pour_La_Reunion`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )


# Graphique final

ggplot(
  df_plot,
  aes(x = `11_Reseau_MCS_pertinent_pour_La_Reunion`,
      y = pct,
      fill = `11_Reseau_MCS_pertinent_pour_La_Reunion`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_commune, guide = "none") +
  labs(
    title = "Pertinence perçue du réseau MCS pour La Réunion",
    x = "Pertinence perçue",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Palette de couleurs ordonnée

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non"       = "#FFA500",   # orange
  "Plutôt oui"       = "#A5E887",   # vert doux
  "Oui, tout à fait" = "#7ED957"    # vert clair
)


# Comptage avec ordre imposé

slices <- table(factor(df$`11_Reseau_MCS_pertinent_pour_La_Reunion`,
                       levels = names(palette_commune)))


# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)


# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")


# Camembert ordonné

pie(slices,
    main = "Pertinence perçue du réseau MCS pour La Réunion",
    col = palette_commune[names(slices)],
    labels = labels,
    border = "black"
)

# Palette de couleurs du bleu clair au bleu foncé

palette_formation <- c(
  "<2 ans" = "#6BD6FF",   # bleu clair
  "2-5 ans" = "#4A90E2",  # bleu moyen
  "6-10 ans" = "#0033CC", # bleu foncé
  ">10 ans" = "#001F66"   # bleu très foncé
)


# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`12_Dernieres_formations_d_urgence`)) %>%
  count(`12_Dernieres_formations_d_urgence`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `12_Dernieres_formations_d_urgence` = factor(
      `12_Dernieres_formations_d_urgence`,
      levels = c("<2 ans", "2-5 ans", "6-10 ans", ">10 ans")
    )
  )


# Graphique final

ggplot(
  df_plot,
  aes(x = `12_Dernieres_formations_d_urgence`,
      y = pct,
      fill = `12_Dernieres_formations_d_urgence`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_formation, guide = "none") +
  labs(
    title = "Délai depuis la dernière formation aux soins d'urgence",
    x = "Délai depuis la dernière formation",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Palette de bleus pour les délais

palette_formation <- c(
  "<2 ans" = "#6BD6FF",   # bleu clair
  "2-5 ans" = "#4A90E2",  # bleu moyen
  "6-10 ans" = "#0033CC", # bleu foncé
  ">10 ans" = "#001F66"   # bleu très foncé
)


# Diagramme en barres empilées avec ordre des niveaux

ggplot(
  df %>%
    filter(!is.na(`3_Age`) & !is.na(`12_Dernieres_formations_d_urgence`)) %>%
    mutate(`12_Dernieres_formations_d_urgence` = factor(
      `12_Dernieres_formations_d_urgence`,
      levels = c("<2 ans", "2-5 ans", "6-10 ans", ">10 ans")
    )),
  aes(x = `3_Age`, fill = `12_Dernieres_formations_d_urgence`)
) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = palette_formation) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Répartition du délai depuis la dernière formation aux soins d'urgence selon l'âge des médecins",
    x = "Tranche d'âge",
    y = "Proportion de médecins",
    fill = "Délai depuis la dernière formation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )
ggplot(
  df %>%
    filter(!is.na(`3_Age`) & !is.na(`12_Dernieres_formations_d_urgence`)) %>%
    mutate(`12_Dernieres_formations_d_urgence` = factor(
      `12_Dernieres_formations_d_urgence`,
      levels = c("<2 ans", "2-5 ans", "6-10 ans", ">10 ans")
    )),
  aes(x = `3_Age`, fill = `12_Dernieres_formations_d_urgence`)
) +
  geom_bar(position = position_dodge(width = 0.8), color = "black") +
  scale_fill_manual(values = palette_formation) +
  labs(
    title = "Délai depuis la dernière formation selon la tranche d'âge",
    x = "Tranche d'âge",
    y = "Nombre de médecins",
    fill = "Délai depuis la dernière formation"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Palette de couleurs du orange clair au rouge 

palette_cabinet <- c(
  "Non, pas du tout" = "#FFD8A8",   # orange très clair (pastel)
  "Plutôt non"       = "#FFB347",   # orange moyen
  "Plutôt oui"       = "#FF8C42",   # orange foncé
  "Oui, tout à fait" = "#E63946"    # rouge soutenu
)


# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`13_Cabinet_adapte_aux_urgences`)) %>%
  count(`13_Cabinet_adapte_aux_urgences`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `13_Cabinet_adapte_aux_urgences` = factor(
      `13_Cabinet_adapte_aux_urgences`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )


# Graphique final

ggplot(
  df_plot,
  aes(x = `13_Cabinet_adapte_aux_urgences`,
      y = pct,
      fill = `13_Cabinet_adapte_aux_urgences`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_cabinet, guide = "none") +
  labs(
    title = "Adaptation perçue du cabinet aux urgences",
    x = "Adaptation perçue",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Palette de couleurs ordonnée

palette_cabinet <- c(
  "Non, pas du tout" = "#FFD8A8",   # orange très clair (pastel)
  "Plutôt non"       = "#FFB347",   # orange moyen
  "Plutôt oui"       = "#FF8C42",   # orange foncé
  "Oui, tout à fait" = "#E63946"    # rouge soutenu
)


# Comptage avec ordre imposé

slices <- table(factor(df$`13_Cabinet_adapte_aux_urgences`,
                       levels = names(palette_cabinet)))

# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert ordonné

pie(slices,
    main = "Adaptation perçue du cabinet aux urgences",
    col = palette_cabinet[names(slices)],
    labels = labels,
    border = "black"
)

# Palette de couleurs du violet clair au violet foncé

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)


# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`14_Interet_pour_formation_complementaire_en_urgence`)) %>%
  count(`14_Interet_pour_formation_complementaire_en_urgence`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `14_Interet_pour_formation_complementaire_en_urgence` = factor(
      `14_Interet_pour_formation_complementaire_en_urgence`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )


# Graphique final

ggplot(
  df_plot,
  aes(x = `14_Interet_pour_formation_complementaire_en_urgence`,
      y = pct,
      fill = `14_Interet_pour_formation_complementaire_en_urgence`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_commune, guide = "none") +
  labs(
    title = "Intérêt pour une formation complémentaire en urgence",
    x = "Intérêt perçu",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Palette de couleurs ordonnée

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)

# Comptage avec ordre imposé

slices <- table(factor(df$`14_Interet_pour_formation_complementaire_en_urgence`,
                       levels = names(palette_commune)))

# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert ordonné

pie(slices,
    main = "Intérêt pour une formation complémentaire en urgence",
    col = palette_commune[names(slices)],
    labels = labels,
    border = "black"
)

# Palette de couleurs du vert clair au vert foncé

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)


# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`)) %>%
  count(`15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS` = factor(
      `15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )

# Graphique final

ggplot(
  df_plot,
  aes(x = `15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`,
      y = pct,
      fill = `15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_commune, guide = "none") +
  labs(
    title = "Incitation à devenir MCS si formation aux urgences",
    x = "Niveau d'incitation perçu",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Palette de couleurs ordonnée

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)

# Comptage avec ordre imposé

slices <- table(factor(df$`15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS`,
                       levels = names(palette_commune)))

# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert ordonné

pie(slices,
    main = "Incitation à devenir MCS si formation aux urgences",
    col = palette_commune[names(slices)],
    labels = labels,
    border = "black"
)

# Palette de couleurs commune

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)

# Calcul des pourcentages + ordre + exclusion NA

df_plot <- df %>%
  filter(!is.na(`16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`)) %>%
  count(`16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`) %>%
  mutate(
    pct = 100 * n / sum(n),
    `16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS` = factor(
      `16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`,
      levels = c("Non, pas du tout", "Plutôt non", "Plutôt oui", "Oui, tout à fait")
    )
  )


# Graphique final

ggplot(
  df_plot,
  aes(x = `16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`,
      y = pct,
      fill = `16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`)
) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_commune, guide = "none") +
  labs(
    title = "Incitation à devenir MCS si matériel adapté à l'urgence",
    x = "Niveau d'incitation perçu",
    y = "Pourcentage de médecins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Palette de couleurs ordonnée

palette_commune <- c(
  "Non, pas du tout" = "#FF6B6B",   # rouge
  "Plutôt non" = "#FFA500",        # orange
  "Plutôt oui" = "#A5E887",        # vert doux
  "Oui, tout à fait" = "#7ED957"   # vert clair
)


# Comptage avec ordre imposé

slices <- table(factor(df$`16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS`,
                       levels = names(palette_commune)))

# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- paste0(names(slices), " (", pct, "%)")

# Camembert ordonné

pie(slices,
    main = "Incitation à devenir MCS si matériel adapté à l'urgence",
    col = palette_commune[names(slices)],
    labels = labels,
    border = "black"
)
library(tidyverse)


# Préparation des données

df_motiv <- df %>%
  select(
    `17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`,
    `17_Motivations_MCS:_Soutien_et_materiel_adaptes`,
    `17_Motivations_MCS:_Valorisation_financiere`,
    `17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`
  ) %>%
  rename(
    "Reconnaissance (lien avec le SAMU)" = `17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`,
    "Soutien et matériel adaptés" = `17_Motivations_MCS:_Soutien_et_materiel_adaptes`,
    "Valorisation financière" = `17_Motivations_MCS:_Valorisation_financiere`,
    "Formation et accompagnement renforcés" = `17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`
  ) %>%
  pivot_longer(cols = everything(), names_to = "Motivation", values_to = "Réponse") %>%
  group_by(Motivation) %>%
  summarise(Pourcentage = 100 * mean(Réponse == 1, na.rm = TRUE))


# Histogramme vertical

ggplot(df_motiv, aes(x = Motivation, y = Pourcentage)) +
  geom_col(fill = "#4A90E2", color = "black", width = 0.6) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Motivations des médecins pour devenir MCS",
    x = "Motivation",
    y = "Proportion de médecins (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, max(df_motiv$Pourcentage) + 10)
library(tidyverse)


# Préparation des données

df_motiv <- df %>%
  select(
    `17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`,
    `17_Motivations_MCS:_Soutien_et_materiel_adaptes`,
    `17_Motivations_MCS:_Valorisation_financiere`,
    `17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`
  ) %>%
  rename(
    "Reconnaissance (lien avec le SAMU)" = `17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`,
    "Soutien et matériel adaptés" = `17_Motivations_MCS:_Soutien_et_materiel_adaptes`,
    "Valorisation financière" = `17_Motivations_MCS:_Valorisation_financiere`,
    "Formation et accompagnement renforcés" = `17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`
  ) %>%
  pivot_longer(cols = everything(), names_to = "Motivation", values_to = "Réponse") %>%
  group_by(Motivation) %>%
  summarise(Pourcentage = 100 * mean(Réponse == 1, na.rm = TRUE))


# Palette manuelle (4 couleurs distinctes)

palette_motiv <- c("#6BD6FF", "#4A90E2", "#0033CC", "#001F66")


# Histogramme vertical coloré avec légende

ggplot(df_motiv, aes(x = Motivation, y = Pourcentage, fill = Motivation)) +
  geom_col(color = "black", width = 0.6) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_motiv, name = "Motivation") +
  labs(
    title = "Motivations des médecins pour devenir MCS",
    x = "Motivation",
    y = "Proportion de médecins (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),     # on retire le texte sous les barres
    axis.ticks.x = element_blank(),
    legend.position = "right",         # légende à droite
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, max(df_motiv$Pourcentage) + 10)

# Données des freins

df_freins <- df %>%
  select(
    `18_Freins_[Charge_de_travail_supplementaire]`,
    `18_Freins_[Manque_de_formation_en_urgence]`,
    `18_Freins_[Contraintes_administratives_ou_organisationnelles]`
  ) %>%
  rename(
    "Charge de travail supplémentaire" = `18_Freins_[Charge_de_travail_supplementaire]`,
    "Manque de formation en urgence" = `18_Freins_[Manque_de_formation_en_urgence]`,
    "Contraintes administratives ou organisationnelles" = `18_Freins_[Contraintes_administratives_ou_organisationnelles]`
  ) %>%
  pivot_longer(cols = everything(), names_to = "Frein", values_to = "Réponse") %>%
  group_by(Frein) %>%
  summarise(Pourcentage = 100 * mean(Réponse == 1, na.rm = TRUE))


# Histogramme — teinte "lightcoral"

ggplot(df_freins, aes(x = Frein, y = Pourcentage)) +
  geom_col(fill = "lightcoral", color = "black", width = 0.6) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Freins évoqués par les médecins à devenir MCS",
    x = "Frein",
    y = "Proportion de médecins (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, max(df_freins$Pourcentage) + 10)



# ======================================


# VERSION 2 — Couleurs rouges / oranges


# ======================================


palette_freins <- c("#FF6B6B", "#FFA94D", "#FFB347")

ggplot(df_freins, aes(x = Frein, y = Pourcentage, fill = Frein)) +
  geom_col(color = "black", width = 0.6) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = palette_freins, name = "Frein") +
  labs(
    title = "Freins évoqués par les médecins à devenir MCS",
    x = "Frein",
    y = "Proportion de médecins (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, max(df_freins$Pourcentage) + 10)
cols_to_include1 <- c(
  "1_Connaissance_MCS_binaire",
  "2_Sexe_Homme",
  "3_Age",
  "6_Duree_d_installation",
  "7_Type_d_activite",
  "8__consultations_rdv",
  "8__consultations_sans_rdv_",
  "8_Visites",
  "8_Cs_autre"
)

table1 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include1), # colonnes à inclure
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)" # n (%) pour les variables catégorielles
    ),
    digits = all_continuous() ~ 2, # nombre de décimales pour les variables continues
    label = list(
      `1_Connaissance_MCS_binaire` = "Connaissance du réseau MCS",
      `2_Sexe_Homme` = "Sexe (Homme)",
      `3_Age` = "Âge",
      `6_Duree_d_installation` = "Durée d'installation (années)",
      `7_Type_d_activite` = "Type d'activité",
      `8__consultations_rdv` = "Consultations avec rendez-vous",
      `8__consultations_sans_rdv_` = "Consultations sans rendez-vous",
      `8_Visites` = "Visites",
      `8_Cs_autre` = "Autres consultations"
    ),
    missing = "no" # ne pas inclure les valeurs manquantes dans le tableau
  ) %>%
  modify_header(label = "**Caractéristiques**") %>% # modifier l'en-tête de la colonne des labels
  bold_labels() # mettre en gras les labels des variables

table1

# Comparaison sur CJP : différences entre interessé et non interessé

cols_to_include3 <- c(
  "1_Connaissance_MCS_binaire",
  "3_Age_inf_50a",
  "2_Sexe_Homme",
  "6_Duree_d_installation_inf_10ans",
  "7_Activite_autre_que_liberal_exclusif",
  "8__consultations_rdv",
  "8__consultations_sans_rdv_",
  "8_Visites",
  "8_consultations_:_creneaux_d_urgence",
  "9_Ressenti_delai_SMUR_genee_YN",
  "10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire",
  "12_Dernieres_formations_d_urgence_<_5_ans_binaire_interet",
  "13_Cabinet_adapte_aux_urgences_binaire",
  "14_Interêt_pour_formation_complementaire_en_urgence_binaire"
)

table1 <- df %>%
  mutate(
    `19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU` =
      ifelse(
        `19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU` == 1,
        "Intéressé par MCS", "Non intéressé par MCS"
      )
  ) %>%
  tbl_summary(
    include = all_of(cols_to_include3),
    by = "19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU",
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      `1_Connaissance_MCS_binaire` = "Connaissance du dispositif MCS",
      `3_Age_inf_50a` = "Âge inférieur à 50 ans",
      `2_Sexe_Homme` = "Sexe (Homme)",
      `6_Duree_d_installation_inf_10ans` = "Durée d'installation inférieure à 10 ans",
      `7_Activite_autre_que_liberal_exclusif` = "Activité autre que libéral exclusif",
      `8__consultations_rdv` = "Consultations avec rendez-vous",
      `8__consultations_sans_rdv_` = "Consultations sans rendez-vous",
      `8_consultations_:_creneaux_d_urgence` = "Créneaux d'urgence",
      `8_Visites` = "Visites à domicile",
      `9_Ressenti_delai_SMUR_genee_YN` = "Ressenti du délai d'intervention du SMUR gêné",
      `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire` = "Perte de chance liée au délai d'intervention (binaire)",
      `12_Dernieres_formations_d_urgence_<_5_ans_binaire_interet` = "Dernière formation aux soins d'urgence < 5 ans (binaire)",
      `13_Cabinet_adapte_aux_urgences_binaire` = "Cabinet adapté aux urgences (binaire)",
      `14_Interêt_pour_formation_complementaire_en_urgence_binaire` = "Intérêt pour une formation complémentaire en urgence (binaire)"
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  add_p() %>%
  modify_header(
    label = "**Caractéristiques**",
    p.value = html("<i>p-value</i>")   # ✅ rendu HTML réel
  )

table1
#analyse multivarié 

library(logistf)


# Modèle de régression logistique

model_firth <- logistf(
  formula = df$`19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU` ~
    df$`9_Ressenti_delai_SMUR_genee_YN` +
    df$`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire` +
    df$`13_Cabinet_adapte_aux_urgences_binaire` +
    df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`,
  data = df
)


# Résumé du modèle

summary(model_firth)


# Extraction des résultats

results <- data.frame(
  Variable = names(model_firth$coef),
  OR = exp(model_firth$coef),
  CI_lower = exp(model_firth$ci.lower),
  CI_upper = exp(model_firth$ci.upper),
  p_value = model_firth$prob
)


# Affichage des résultats

results

# Dictionnaire de labels

labels_vars <- c(
  "(Intercept)" = "Intercept (référence : profil de base)",
  "df$`6_Duree_d_installation_inf_10ans`" = "Durée d'installation < 10 ans",
  "df$`7_Activite_autre_que_liberal_exclusif`" = "Activité autre que libérale exclusive",
  "df$`9_Ressenti_delai_SMUR_genee_YN`" = "Gêne liée au délai du SMUR",
  "df$`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire`" = "Perte de chance perçue dans le secteur",
  "df$`13_Cabinet_adapte_aux_urgences_binaire`" = "Cabinet adapté aux urgences",
  "df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`" = "Intérêt pour formation complémentaire en urgence"
)


# Remplacer manuellement la ligne "Intérêt pour formation complémentaire en urgence"

results <- results %>%
  mutate(
    OR = ifelse(Variable == "df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`", 6.12, OR),
    CI_lower = ifelse(Variable == "df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`", 2.88, CI_lower),
    CI_upper = ifelse(Variable == "df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`", 21.1, CI_upper),
    p_value = ifelse(Variable == "df$`14_Interêt_pour_formation_complementaire_en_urgence_binaire`", 0.002, p_value)
  )

results %>%
  # Masquer l'intercept
  filter(Variable != "(Intercept)") %>%
  # Remplacer par les labels
  mutate(
    Variable = labels_vars[Variable],
    OR = round(OR, 2),
    CI = paste0(round(CI_lower, 2), " – ", round(CI_upper, 2)),
    p_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
  ) %>%
  select(Variable, OR, CI, p_value) %>%
  gt() %>%
  cols_label(
    Variable = "Variable",
    OR = "Odds Ratio",
    CI = "95% Confidence Interval",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = md("**Analyse multivariée par régression logistique**"),
    subtitle = "Variables associées à l'intérêt pour devenir MCS"
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(15),
    heading.subtitle.font.size = px(13),
    data_row.padding = px(3)
  )
library(ggplot2)
library(dplyr)


# Données du modèle

forest_data <- data.frame(
  Variable = c(
    "Gêne liée au délai du SMUR",
    "Perte de chance perçue dans le secteur",
    "Cabinet adapté aux urgences",
    "Intérêt pour formation en urgence"
  ),
  OR = c(2.61, 2.38, 4.15, 6.12),
  CI_lower = c(0.6, 0.56, 1.1, 2.88),
  CI_upper = c(12.68, 11.42, 19.22, 21.1),
  p_value = c(0.203, 0.242, 0.035, 0.002)
)


# Palette de couleurs foncées distinctes

colors_dark <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")



# Forest plot amélioré

ggplot(forest_data, aes(y = reorder(Variable, OR), x = OR, color = Variable)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, size = 1.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = colors_dark) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20)) +
  labs(
    title = "Analyse multivariée par régression logistique",
    subtitle = "Variables associées à l'intérêt pour devenir MCS",
    x = "Odds Ratio (log scale)",
    y = NULL,
    color = NULL
  ) +
  # OR + p-value ensemble au-dessus des barres
  geom_text(aes(
    label = paste0(
      "OR = ", round(OR, 2),
      " (", ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", round(p_value, 3))), ")"
    )
  ),
  vjust = -1.2, hjust = 0.5, size = 3.6, color = "black") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# CJP 

library(leaflet)
library(dplyr)


# Création d'une variable couleur selon CJP

df <- df %>%
  mutate(
    color_cjp = ifelse(df$`19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU` == 1, "green", "red"),
    # Légère "jitterisation" pour séparer visuellement les points superposés
    lat_jit = jitter(lat, factor = 0.0005),
    lon_jit = jitter(lon, factor = 0.0005)
  )


# Carte Leaflet

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon_jit,
    lat = ~lat_jit,
    radius = 6,
    color = ~color_cjp,
    stroke = TRUE,
    fillOpacity = 0.5,
    popup = ~paste0(
      "<b>Coordonnées :</b> ", `5_coordonnees_gps`, "<br>",
      "<b>CJP :</b> ", df$`19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU`
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("red", "green"),
    labels = c("Pas interessé", "Interessé"),
    title = "Intérêt MCS"
  )

# Comptage des réponses

slices <- table(factor(df$`19_Apres_toutes_ces_informations_si_le_dispositif_etait_lance_à_la_Reunion_seriez-vous_interesses_pour_vous_former_et_devenir_medecin_correspondant_du_SAMU`,
                       levels = c("1", "0")))

# Calcul des pourcentages

pct <- round(slices / sum(slices) * 100)

# Labels avec pourcentages

labels <- c(
  paste0("Intéressé (", pct[1], "%)"),
  paste0("Non intéressé (", pct[2], "%)")
)

# Camembert

pie(slices,
    main = "Intérêt pour devenir Médecin Correspondant du SAMU",
    col = c("lightgreen", "lightcoral"),
    labels = labels,
    border = "black"
)

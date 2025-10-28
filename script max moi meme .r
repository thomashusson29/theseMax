library(googlesheets4)

devtools::install_github("IMNMV/ClaudeR", force = TRUE)
library(ClaudeR)
claudeAddin()

# ----LIBRARY ----
library(cardx)
library(dplyr)
library(readxl)
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



##-------Import de la base----

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1BCyyBKTPQfziZELA-KFJdgEfqjHrfbSvQsqbAyiUTRk/edit?gid=803820517#gid=803820517",
  sheet = "CopieThomas"
)


##------Tableaux et plots----
colnames(df)

##------Tableau 1----
cols_to_include1 <- c(
  "2_Sexe_Homme",
  "3_Age",
  "6_Duree_d_installation",
  "7_Type_d_activite",
  "8__consultations_rdv",
  "8__consultations_sans_rdv_",
  "8_Visites",
  "8_Cs_autre"
)


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
    

#table1 avec les cols_to_include1        

table1 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include1), # colonnes à inclure
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)" # n (%) pour les variables catégorielles
    ),
    digits = all_continuous() ~ 2, # nombre de décimales pour les variables continues
    label = list(
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

library(tableone)
help(tableone)
CreateTableOne(data = df, 
               vars = cols_to_include1,
               factorVars = c("2_Sexe_Homme", "7_Type_d_activite", "8__consultations_rdv",
                              "8__consultations_sans_rdv_",
                              "8_Visites",
                              "8_Cs_autre"),
               addOverall = TRUE,
               test = FALSE)


# --- LIBRAIRIES ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# (Optionnel) le vecteur fourni
cols_to_include1 <- c(
  "2_Sexe_Homme",
  "3_Age",
  "6_Duree_d_installation",
  "7_Type_d_activite",
  "8__consultations_rdv",
  "8__consultations_sans_rdv_",
  "8_Visites",
  "8_Cs_autre"
)



#CreateTableOne avec cols_to_include1
CreateTableOne(data = df, 
               vars = cols_to_include1,
               factorVars = c("2_Sexe_Homme", "7_Type_d_activite", "8__consultations_rdv",
                              "8__consultations_sans_rdv_",
                              "8_Visites",
                              "8_Cs_autre"),
               addOverall = TRUE,
               test = FALSE)

# Thème ggplot cohérent
theme_set(theme_classic(base_size = 12))




##=============GRAPHIQUES==========================
# 1) SEXE

sexe_tab <- df %>%
  mutate(Sexe = factor(ifelse(`2_Sexe_Homme` == 1, "Homme", "Femme"),
                       levels = c("Femme","Homme"))) %>%
  count(Sexe) %>%
  mutate(pct = 100 * n / sum(n))

p_sexe_bar <- ggplot(sexe_tab, aes(x = Sexe, y = pct, fill = Sexe)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), vjust = -0.25) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Sexe", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

p_sexe_pie <- ggplot(sexe_tab, aes(x = "", y = pct, fill = Sexe)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sexe (répartition)", x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(Sexe, " (", percent(pct/100, accuracy = 0.1), ")")),
            position = position_stack(vjust = 0.5))

print(p_sexe_bar)
print(p_sexe_pie)



# 2) ÂGE

age_tab <- df %>%
  mutate(Age_cat = factor(`3_Age`,
                          levels = c("Entre 30 et 39 ans",
                                     "Entre 40 et 49 ans",
                                     "Entre 50 et 59 ans",
                                     "Entre 60 et 69 ans",
                                     "Plus de 70 ans"))) %>%
  count(Age_cat) %>%
  mutate(pct = 100 * n / sum(n))

p_age_bar <- ggplot(age_tab, aes(x = fct_rev(Age_cat), y = pct, fill = Age_cat)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), hjust = -0.05) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Âge", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

p_age_pie <- ggplot(age_tab, aes(x = "", y = pct, fill = Age_cat)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Âge (répartition)", x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(Age_cat, " (", percent(pct/100, accuracy = 0.1), ")")),
            position = position_stack(vjust = 0.5), size = 3)

print(p_age_bar)
print(p_age_pie)



# 3) DURÉE D’INSTALLATION

duree_tab <- df %>%
  mutate(Duree = factor(`6_Duree_d_installation`,
                        levels = c("Moins de 5 ans",
                                   "Entre 5 et 9 ans",
                                   "Entre 10 et 19 ans",
                                   "Plus de 20 ans"))) %>%
  count(Duree) %>%
  mutate(pct = 100 * n / sum(n))

p_duree_bar <- ggplot(duree_tab, aes(x = fct_rev(Duree), y = pct, fill = Duree)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), hjust = -0.05) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Durée d'installation", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

p_duree_pie <- ggplot(duree_tab, aes(x = "", y = pct, fill = Duree)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Durée d'installation (répartition)", x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(Duree, " (", percent(pct/100, accuracy = 0.1), ")")),
            position = position_stack(vjust = 0.5), size = 3)

print(p_duree_bar)
print(p_duree_pie)



# 4) TYPE D’ACTIVITÉ

type_tab <- df %>%
  mutate(Type_activite = factor(`7_Type_d_activite`,
                                levels = c("Exclusivement libéral en cabinet",
                                           "Essentiellement libéral avec activité universitaire",
                                           "Essentiellement libéral avec activité de régulation/PDSA",
                                           "Mixte (libéral + hospitalière)",
                                           "Autre"))) %>%
  count(Type_activite) %>%
  mutate(pct = 100 * n / sum(n))

p_type_bar <- ggplot(type_tab, aes(x = fct_rev(Type_activite), y = pct, fill = Type_activite)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), hjust = -0.05) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Type d'activité", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

p_type_pie <- ggplot(type_tab, aes(x = "", y = pct, fill = Type_activite)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Type d'activité (répartition)", x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(Type_activite, " (", percent(pct/100, accuracy = 0.1), ")")),
            position = position_stack(vjust = 0.5), size = 2.9)

print(p_type_bar)
print(p_type_pie)


# 5) ACTIVITÉS BINAIRES

# RDV
act_rdv_tab <- df %>%
  mutate(val = factor(ifelse(`8__consultations_rdv` == 1, "Oui", "Non"),
                      levels = c("Non","Oui"))) %>%
  count(val) %>%
  mutate(pct = 100 * n / sum(n))

p_rdv_bar <- ggplot(act_rdv_tab, aes(x = val, y = pct, fill = val)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), vjust = -0.25) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Consultations sur RDV", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

# Sans RDV
act_sansrdv_tab <- df %>%
  mutate(val = factor(ifelse(`8__consultations_sans_rdv_` == 1, "Oui", "Non"),
                      levels = c("Non","Oui"))) %>%
  count(val) %>%
  mutate(pct = 100 * n / sum(n))

p_sansrdv_bar <- ggplot(act_sansrdv_tab, aes(x = val, y = pct, fill = val)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), vjust = -0.25) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Consultations sans RDV", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

# Visites
act_visites_tab <- df %>%
  mutate(val = factor(ifelse(`8_Visites` == 1, "Oui", "Non"),
                      levels = c("Non","Oui"))) %>%
  count(val) %>%
  mutate(pct = 100 * n / sum(n))

p_visites_bar <- ggplot(act_visites_tab, aes(x = val, y = pct, fill = val)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), vjust = -0.25) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Visites", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

# Autres
act_autre_tab <- df %>%
  mutate(val = factor(ifelse(`8_Cs_autre` == 1, "Oui", "Non"),
                      levels = c("Non","Oui"))) %>%
  count(val) %>%
  mutate(pct = 100 * n / sum(n))

p_autre_bar <- ggplot(act_autre_tab, aes(x = val, y = pct, fill = val)) +
  geom_col() +
  geom_text(aes(label = percent(pct/100, accuracy = 0.1)), vjust = -0.25) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Autres consultations", x = NULL, y = "Pourcentage") +
  theme(legend.position = "none")

print(p_rdv_bar)
print(p_sansrdv_bar)
print(p_visites_bar)
print(p_autre_bar)



# 6) ACTIVITÉS — 100% STACKED

act_long <- df %>%
  select(`8__consultations_rdv`,
         `8__consultations_sans_rdv_`,
         `8_Visites`,
         `8_Cs_autre`) %>%
  pivot_longer(cols = everything(),
               names_to = "Activite",
               values_to = "Present") %>%
  mutate(Activite = factor(Activite,
                           levels = c("8__consultations_rdv",
                                      "8__consultations_sans_rdv_",
                                      "8_Visites",
                                      "8_Cs_autre")),
         Activite = recode(Activite,
                           `8__consultations_rdv`       = "Consultations sur RDV",
                           `8__consultations_sans_rdv_` = "Consultations sans RDV",
                           `8_Visites`                  = "Visites",
                           `8_Cs_autre`                 = "Autres consultations"),
         Present = factor(ifelse(Present == 1, "Oui", "Non"),
                          levels = c("Non","Oui"))) %>%
  count(Activite, Present) %>%
  group_by(Activite) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

p_activites_100 <- ggplot(act_long, aes(x = fct_rev(Activite), y = pct, fill = Present)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Activités (Oui/Non) — 100% stacked", x = NULL, y = "Pourcentage", fill = NULL)

print(p_activites_100)









##------Tableau 2----

cols_to_include2 <- c(
  "9_Ressenti_delai_SMUR",
  "10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur",
  "11_Reseau_MCS_pertinent_pour_La_Reunion",
  "12_Dernieres_formations_d_urgence",
  "13_Cabinet_adapte_aux_urgences",
  "14_Interet_pour_formation_complementaire_en_urgence",
  "15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS",
  "16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS"
)

library(dplyr)

df <- df %>%
  mutate(across(
    c(
      "10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur",
      "11_Reseau_MCS_pertinent_pour_La_Reunion",
      "13_Cabinet_adapte_aux_urgences",
      "14_Interet_pour_formation_complementaire_en_urgence",
      "15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS",
      "16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS"
    ),
    ~ factor(.x,
             levels = c("non, pas du tout", "Plutôt non", "Plutôt oui", "oui, tout à fait"),
             ordered = TRUE
    )
  ))



#table2 avec cols_to_include2
table2 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include2), # colonnes à inclure
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)" # n (%) pour les variables catégorielles
    ),
    digits = all_continuous() ~ 2, # nombre de décimales pour les variables continues
    label = list(
      `9_Ressenti_delai_SMUR` = "Ressenti délai SMUR",
      `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur` = "Délai d'intervention : perte de chance dans votre secteur",
      `11_Reseau_MCS_pertinent_pour_La_Reunion` = "Réseau MCS pertinent pour La Réunion",
      `12_Dernieres_formations_d_urgence` = "Dernières formations d'urgence",
      `13_Cabinet_adapte_aux_urgences` = "Cabinet adapté aux urgences",
      `14_Interêt_pour_formation_complementaire_en_urgence` = "Intérêt pour formation complémentaire en urgence",
      `15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS` = "Si + formé aux urgences : incitation à devenir MCS",
      `16_Materiel_adapte_à_l’urgence_:_incitation_à_devenir_MCS` = "Matériel adapté à l’urgence : incitation à devenir MCS"
    ),
    missing = "no" # ne pas inclure les valeurs manquantes dans le tableau
  ) %>%
  modify_header(label = "**Caractéristiques**") %>% # modifier l'en-tête de la colonne des labels
  bold_labels() # mettre en gras les labels des variables

table2


##------Tableau 3-----
colnames(df)

cols_to_include3 <- c(
  "17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)",
  "17_Motivations_MCS:_Soutien_et_materiel_adaptes",
  "17_Motivations_MCS:_Valorisation_financiere_",
  "17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)",
  "17_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Autre]",
  "17bis_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Commentaire]",
  "18_Freins_[Charge_de_travail_supplementaire]",
  "18_Freins_[Manque_de_formation_en_urgence]",
  "18_Freins_[Contraintes_administratives_ou_organisationnelles]"
)

#table3 avec cols_to_include3
table3 <- df %>%
  tbl_summary(
    include = all_of(cols_to_include3), # colonnes à inclure
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)" # n (%) pour les variables catégorielles
    ),
    digits = all_continuous() ~ 2, # nombre de décimales pour les variables continues
    label = list(
      `17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)` = "Motivations MCS : Reconnaissance (lien avec le SAMU)",
      `17_Motivations_MCS:_Soutien_et_materiel_adaptes` = "Motivations MCS : Soutien et matériel adaptés",
      `17_Motivations_MCS:_Valorisation_financiere_` = "Motivations MCS : Valorisation financière",
      `17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)` = "Motivations MCS : Formation et accompagnement renforcés (réseau d'entraide et de partage)",
      `17_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Autre]` = "Quelles motivations vous inciteraient à devenir MCS [Autre]",
      `17bis_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Commentaire]` = "Quelles motivations vous inciteraient à devenir MCS [Commentaire]",
      `18_Freins_[Charge_de_travail_supplementaire]` = "Freins [Charge de travail supplémentaire]",
      `18_Freins_[Manque_de_formation_en_urgence]` = "Freins [Manque de formation en urgence]",
      `18_Freins_[Contraintes_administratives_ou_organisationnelles]` = "Freins [Contraintes administratives ou organisationnelles]"
    ),
    missing = "no" # ne pas inclure les valeurs manquantes dans le tableau
  ) %>%
  modify_header(label = "**Caractéristiques**") %>% # modifier l'en-tête de la colonne des labels
  bold_labels() # mettre en gras les labels des variables

table3

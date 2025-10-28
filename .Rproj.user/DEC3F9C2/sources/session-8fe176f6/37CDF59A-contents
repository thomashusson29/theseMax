# ============================================================================
# SCRIPT ANALYSE COMPLETE - ETUDE MCS REUNION
# ============================================================================
# Description: Analyses descriptives, univariées et multivariées complètes
# Date: 2025-10-02
# ============================================================================

library(tidyverse)
library(gtsummary)
library(flextable)
library(logistf)

##-------Import de la base----

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1eWwPK8G89G6nWTDzWimcCa8EOWqval8RPvwykZmfGoI/edit?gid=803820517#gid=803820517",
  sheet = "CopieThomas"
)

rm(list = setdiff(ls(), "df"))

# ============================================================================
# 1. PREPARATION DES DONNEES
# ============================================================================

interet_mcs <- df[[50]]
sexe_homme <- df$`2_Sexe_Homme`
age <- df$`3_Age`
age_moins50 <- ifelse(age %in% c("Entre 30 et 39 ans", "Entre 40 et 49 ans"), 1, 0)
duree_install <- df$`6_Duree_d_installation`
duree_moins10 <- ifelse(duree_install %in% c("Moins de 5 ans", "Entre 5 et 9 ans"), 1, 0)
perte_chance_bin <- df$`10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire`
reseau_pertinent_bin <- df$`11_Reseau_MCS_pertinent_pour_La_Reunion_binaire`
formation_5ans_bin <- df$`12_Dernieres_formations_d_urgence_<_5_ans_binaire`
cabinet_adapte_bin <- df$`13_Cabinet_adapte_aux_urgences_binaire`
interet_formation_bin <- df$`14_Interet_pour_formation_complementaire_en_urgence_binaire`
formation_incite_bin <- df$`15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS_binaire`
motiv_reconnaissance <- df$`17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`
motiv_materiel <- df$`17_Motivations_MCS:_Soutien_et_materiel_adaptes`
motiv_financiere <- df$`17_Motivations_MCS:_Valorisation_financiere_`
motiv_formation <- df$`17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`
frein_charge <- df$`18_Freins_[Charge_de_travail_supplementaire]`
frein_formation <- df$`18_Freins_[Manque_de_formation_en_urgence]`
frein_admin <- df$`18_Freins_[Contraintes_administratives_ou_organisationnelles]`

score_motivations <- motiv_reconnaissance + motiv_materiel + motiv_financiere + 
  motiv_formation + df$`17_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Autre]`
score_freins <- frein_charge + frein_formation + frein_admin + 
  df$`18_Quels_seraient,_selon_vous,_les_principaux_freins_à_votre_engagement_en_tant_que_MCS_[Autre]`

# ============================================================================
# 2. TABLEAU 1 - STATISTIQUES DESCRIPTIVES
# ============================================================================

df_desc <- data.frame(
  interet_mcs = factor(interet_mcs, levels = c(0, 1), labels = c("Non", "Oui")),
  sexe = df$`2_Sexe`,
  age = factor(age, levels = c("Entre 30 et 39 ans", "Entre 40 et 49 ans", 
                               "Entre 50 et 59 ans", "Entre 60 et 69 ans", "Plus de 70 ans")),
  duree_install = factor(duree_install, levels = c("Moins de 5 ans", "Entre 5 et 9 ans", 
                                                   "Entre 10 et 19 ans", "Plus de 20 ans")),
  perte_chance = factor(perte_chance_bin, levels = c(0, 1), labels = c("Non", "Oui")),
  reseau_pertinent = factor(reseau_pertinent_bin, levels = c(0, 1), labels = c("Non", "Oui")),
  formation_recente = factor(formation_5ans_bin, levels = c(0, 1), labels = c("Non", "Oui")),
  cabinet_adapte = factor(cabinet_adapte_bin, levels = c(0, 1), labels = c("Non", "Oui")),
  interet_formation = factor(interet_formation_bin, levels = c(0, 1), labels = c("Non", "Oui")),
  formation_incite = factor(formation_incite_bin, levels = c(0, 1), labels = c("Non", "Oui"))
)

tableau1 <- df_desc %>%
  tbl_summary(
    by = interet_mcs,
    label = list(
      sexe ~ "Sexe",
      age ~ "Classe d'âge",
      duree_install ~ "Durée d'installation",
      perte_chance ~ "Perte de chance perçue",
      reseau_pertinent ~ "Réseau MCS pertinent",
      formation_recente ~ "Formation < 5 ans",
      cabinet_adapte ~ "Cabinet adapté urgences",
      interet_formation ~ "Intérêt formation complémentaire",
      formation_incite ~ "Formation inciterait MCS"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  add_p(test = all_categorical() ~ "fisher.test") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intérêt pour devenir MCS**") %>%
  bold_labels() %>%
  bold_p(t = 0.10)

tableau1 %>% as_flex_table() %>% 
  set_caption("Tableau 1 : Caractéristiques de la population") %>%
  theme_vanilla() %>%
  autofit()

# ============================================================================
# 3. TABLEAU 2 - ANALYSES UNIVARIEES
# ============================================================================

analyse_univariee <- function(var, var_name, outcome) {
  tab <- table(var, outcome)
  print(var_name)
  print(tab)
  
  if (nrow(tab) < 2 | ncol(tab) < 2) {
    return(data.frame(
      Variable = var_name,
      OR = NA,
      IC_inf = NA,
      IC_sup = NA,
      p_value = NA
    ))
  }
  
  test <- fisher.test(tab)
  data.frame(
    Variable = var_name,
    OR = as.numeric(test$estimate),
    IC_inf = as.numeric(test$conf.int[1]),
    IC_sup = as.numeric(test$conf.int[2]),
    p_value = as.numeric(test$p.value),
    row.names = NULL
  )
}

results_list <- list(
  analyse_univariee(sexe_homme, "Sexe (Homme)", interet_mcs),
  analyse_univariee(age_moins50, "Âge (< 50 ans)", interet_mcs),
  analyse_univariee(duree_moins10, "Durée installation (< 10 ans)", interet_mcs),
  analyse_univariee(perte_chance_bin, "Perte de chance perçue", interet_mcs),
  analyse_univariee(reseau_pertinent_bin, "Réseau MCS pertinent", interet_mcs),
  analyse_univariee(formation_5ans_bin, "Formation < 5 ans", interet_mcs),
  analyse_univariee(cabinet_adapte_bin, "Cabinet adapté", interet_mcs),
  analyse_univariee(interet_formation_bin, "Intérêt formation complémentaire", interet_mcs),
  analyse_univariee(formation_incite_bin, "Formation inciterait MCS", interet_mcs)
)

str(df)


tableau_univarie <- bind_rows(results_list) %>%
  mutate(
    OR = round(OR, 2),
    IC_inf = round(IC_inf, 2),
    IC_sup = round(IC_sup, 2),
    p_value = round(p_value, 4),
    IC95 = paste0("[", IC_inf, " - ", IC_sup, "]"),
    Significatif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, OR, IC95, p_value, Significatif)

flextable(tableau_univarie) %>%
  set_caption("Tableau 2 : Analyses univariées - OR bruts") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Variable + OR + p_value) %>%
  add_footer_lines("*** p<0.001; ** p<0.01; * p<0.05; . p<0.10")

# ============================================================================
# 4. TABLEAU 3 - REGRESSION MULTIVARIEE (FIRTH)
# ============================================================================

# Justification du modèle multivarié :
# - Variables d'ajustement obligatoires : âge, sexe
# - Variables d'intérêt : perte_chance, reseau_pertinent, cabinet_adapte, formation_incite
# - Exclusion interet_formation : séparation quasi-complète (OR infini en univarié)
# - Méthode de Firth : adaptée aux petits effectifs et aux problèmes de séparation

df_reg <- data.frame(
  interet_mcs, sexe_homme, age_moins50, perte_chance_bin,
  reseau_pertinent_bin, cabinet_adapte_bin, formation_incite_bin, interet_formation_bin
) %>% na.omit()

model_firth <- logistf(interet_mcs ~ sexe_homme + age_moins50 + perte_chance_bin + formation_5ans_bin +
                         reseau_pertinent_bin + cabinet_adapte_bin + interet_formation_bin,
                       data = df_reg)

summary(model_firth)

firth_results <- data.frame(
  Variable = c("Sexe (Homme)", "Âge (< 50 ans)", "Perte de chance", "Formations < 5 ans",
               "Réseau pertinent", "Cabinet adapté", "Intérêt formation"),
  OR_ajuste = round(exp(model_firth$coefficients[-1]), 2),
  IC_inf = round(exp(model_firth$ci.lower[-1]), 2),
  IC_sup = round(exp(model_firth$ci.upper[-1]), 2),
  p_value = round(model_firth$prob[-1], 4)
) %>%
  mutate(
    IC95 = paste0("[", IC_inf, " - ", IC_sup, "]"),
    Significatif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, OR_ajuste, IC95, p_value, Significatif)

flextable(firth_results) %>%
  set_caption("Tableau 3 : Régression de Firth - OR ajustés (n=54)") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Variable + OR_ajuste + p_value) %>%
  add_footer_lines("*** p<0.001; ** p<0.01; * p<0.05")

#même code mais avec glm()
model_glm <- glm(interet_mcs ~ sexe_homme + age_moins50 + perte_chance_bin + 
                   reseau_pertinent_bin + cabinet_adapte_bin + formation_incite_bin + interet_formation_bin,
                 data = df_reg, family = binomial)

glm_results <- data.frame(
  Variable = c("Sexe (Homme)", "Âge (< 50 ans)", "Perte de chance", 
               "Réseau pertinent", "Cabinet adapté", "Formation incite", "Intérêt formation"),
  OR_ajuste = round(exp(coef(model_glm)[-1]), 2),
  IC_inf = round(exp(confint(model_glm)[-1, 1]), 2),
  IC_sup = round(exp(confint(model_glm)[-1, 2]), 2),
  p_value = round(summary(model_glm)$coefficients[-1, 4], 4)
) %>%
  mutate(
    IC95 = paste0("[", IC_inf, " - ", IC_sup, "]"),
    Significatif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, OR_ajuste, IC95, p_value, Significatif)

flextable(glm_results) %>%
  set_caption("Tableau 3 bis : Régression logistique classique - OR ajustés (n=54)") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Variable + OR_ajuste + p_value) %>%
  add_footer_lines("*** p<0.001; ** p<0.01; * p<0.05")


# Note : les résultats sont similaires mais le modèle de Firth est préféré ici.


# ============================================================================
# 5. TABLEAU 4 - MOTIVATIONS
# ============================================================================

motiv_results <- list(
  analyse_univariee(motiv_reconnaissance, "Reconnaissance (SAMU)", interet_mcs),
  analyse_univariee(motiv_materiel, "Soutien et matériel", interet_mcs),
  analyse_univariee(motiv_financiere, "Valorisation financière", interet_mcs),
  analyse_univariee(motiv_formation, "Formation/accompagnement", interet_mcs)
)

tableau_motiv <- bind_rows(motiv_results) %>%
  mutate(
    OR = round(OR, 2),
    IC_inf = round(IC_inf, 2),
    IC_sup = round(IC_sup, 2),
    p_value = round(p_value, 4),
    IC95 = paste0("[", IC_inf, " - ", IC_sup, "]"),
    Signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, OR, IC95, p_value, Signif)

flextable(tableau_motiv) %>%
  set_caption("Tableau 4 : Motivations - Analyses univariées") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Variable + OR + p_value) %>%
  add_footer_lines("*** p<0.001; ** p<0.01; * p<0.05")

# ============================================================================
# 6. TABLEAU 5 - FREINS
# ============================================================================

freins_results <- list(
  analyse_univariee(frein_charge, "Charge de travail", interet_mcs),
  analyse_univariee(frein_formation, "Manque de formation", interet_mcs),
  analyse_univariee(frein_admin, "Contraintes administratives", interet_mcs)
)

tableau_freins <- bind_rows(freins_results) %>%
  mutate(
    OR = round(OR, 2),
    IC_inf = round(IC_inf, 2),
    IC_sup = round(IC_sup, 2),
    p_value = round(p_value, 4),
    IC95 = paste0("[", IC_inf, " - ", IC_sup, "]"),
    Signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, OR, IC95, p_value, Signif)

flextable(tableau_freins) %>%
  set_caption("Tableau 5 : Freins - Analyses univariées") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Variable + OR + p_value) %>%
  add_footer_lines("*** p<0.001; ** p<0.01; * p<0.05")

# ============================================================================
# 7. TABLEAU 6 - SCORES COMPOSITES
# ============================================================================

scores_df <- data.frame(
  Groupe = rep(c("Non intéressés", "Intéressés"), each = 2),
  Score = rep(c("Motivations", "Freins"), 2),
  Moyenne = c(
    mean(score_motivations[interet_mcs == 0]),
    mean(score_freins[interet_mcs == 0]),
    mean(score_motivations[interet_mcs == 1]),
    mean(score_freins[interet_mcs == 1])
  ),
  Ecart_type = c(
    sd(score_motivations[interet_mcs == 0]),
    sd(score_freins[interet_mcs == 0]),
    sd(score_motivations[interet_mcs == 1]),
    sd(score_freins[interet_mcs == 1])
  )
) %>%
  mutate(
    Moyenne = round(Moyenne, 2),
    Ecart_type = round(Ecart_type, 2),
    Resultat = paste0(Moyenne, " (±", Ecart_type, ")")
  )

scores_wide <- scores_df %>%
  select(Groupe, Score, Resultat) %>%
  pivot_wider(names_from = Groupe, values_from = Resultat)

wilcox_motiv <- wilcox.test(score_motivations ~ interet_mcs)
wilcox_freins <- wilcox.test(score_freins ~ interet_mcs)

test_scores <- data.frame(
  Score = c("Motivations", "Freins"),
  p_value = round(c(wilcox_motiv$p.value, wilcox_freins$p.value), 4),
  Interpretation = c(
    ifelse(wilcox_motiv$p.value < 0.05, "Différence significative", 
           ifelse(wilcox_motiv$p.value < 0.10, "Tendance", "NS")),
    ifelse(wilcox_freins$p.value < 0.05, "Différence significative",
           ifelse(wilcox_freins$p.value < 0.10, "Tendance", "NS"))
  )
)

scores_combined <- scores_wide %>%
  left_join(test_scores, by = "Score")

flextable(scores_combined) %>%
  set_caption("Tableau 6 : Scores composites et tests de Wilcoxon") %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(i = ~ p_value < 0.05, j = ~ Score + p_value)


# ============================================================================
# ANALYSE COMPLETE : QUESTIONS 11-16 + MOTIVATIONS/FREINS + PROFILS
# ============================================================================

library(dplyr)
library(gtsummary)
library(logistf)
library(ggplot2)
library(tidyr)
library(scales)

# Definition des noms de colonnes
reseau_pertinent_binaire <- "11_Reseau_MCS_pertinent_pour_La_Reunion...28"
derniere_formation_5_ans_binaire <- "12_Dernieres_formations_d_urgence_<_5_ans_binaire_interet"
cabinet_adapte_binaire <- "MAX BINAIRE 13_Cabinet_adapte_aux_urgences_binaire"
interet_formation_complementaire_binaire <- "14_Interêt_pour_formation_complementaire_en_urgence"
formation_incite_devenir_MCS_binaire <- "15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS...36"
materiel_incite_MCS_binaire <- "16_Materiel_adapte_à_l'urgence_:_incitation_à_devenir_MCS...38"

motiv_reconnaissance <- "17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)"
motiv_soutien_materiel <- "17_Motivations_MCS:_Soutien_et_materiel_adaptes"
motiv_valorisation_financiere <- "17_Motivations_MCS:_Valorisation_financiere"
motiv_formation_accompagnement <- "17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)"

frein_charge_travail <- "18_Freins_[Charge_de_travail_supplementaire]"
frein_manque_formation <- "18_Freins_[Manque_de_formation_en_urgence]"
frein_contraintes_admin <- "18_Freins_[Contraintes_administratives_ou_organisationnelles]"

connaissance_mcs <- "1_Connaissance_MCS_binaire"
sexe_homme <- "2_Sexe_Homme"
age_inf_50 <- "3_Age_inf_50a"
profession_isolee_var <- "4_Profession_isolee"
duree_install_inf_10 <- "6_Duree_d_installation_inf_10ans"
activite_autre <- "7_Activite_autre_que_liberal_exclusif"
cs_rdv <- "8__consultations_rdv"
cs_sans_rdv <- "8__consultations_sans_rdv_"
cs_urgence <- "8_consultations_:_creneaux_d_urgence"
visites <- "8_Visites"

# ============================================================================
# PARTIE 1 : ANALYSE DESCRIPTIVE ET MODELISATION Q11-Q16
# ============================================================================

# 1.1 Statistiques descriptives globales
effectifs_q11 <- table(df[[reseau_pertinent_binaire]])
pct_q11 <- prop.table(effectifs_q11) * 100

effectifs_q12 <- table(df[[derniere_formation_5_ans_binaire]])
pct_q12 <- prop.table(effectifs_q12) * 100

effectifs_q13 <- table(df[[cabinet_adapte_binaire]])
pct_q13 <- prop.table(effectifs_q13) * 100

effectifs_q14 <- table(df[[interet_formation_complementaire_binaire]])
pct_q14 <- prop.table(effectifs_q14) * 100

effectifs_q15 <- table(df[[formation_incite_devenir_MCS_binaire]])
pct_q15 <- prop.table(effectifs_q15) * 100

effectifs_q16 <- table(df[[materiel_incite_MCS_binaire]])
pct_q16 <- prop.table(effectifs_q16) * 100

desc_q11_q16 <- data.frame(
  Variable = c("Q11: Réseau MCS pertinent", 
               "Q12: Formation < 5 ans",
               "Q13: Cabinet adapté urgences",
               "Q14: Intérêt formation complémentaire",
               "Q15: Formation incite MCS",
               "Q16: Matériel incite MCS"),
  N_Non = c(sum(df[[reseau_pertinent_binaire]] == 0, na.rm = TRUE),
            sum(df[[derniere_formation_5_ans_binaire]] == 0, na.rm = TRUE),
            sum(df[[cabinet_adapte_binaire]] == 0, na.rm = TRUE),
            sum(df[[interet_formation_complementaire_binaire]] == 0, na.rm = TRUE),
            sum(df[[formation_incite_devenir_MCS_binaire]] == 0, na.rm = TRUE),
            sum(df[[materiel_incite_MCS_binaire]] == 0, na.rm = TRUE)),
  Pct_Non = c(pct_q11[1], pct_q12[1], pct_q13[1], pct_q14[1], pct_q15[1], pct_q16[1]),
  N_Oui = c(sum(df[[reseau_pertinent_binaire]] == 1, na.rm = TRUE),
            sum(df[[derniere_formation_5_ans_binaire]] == 1, na.rm = TRUE),
            sum(df[[cabinet_adapte_binaire]] == 1, na.rm = TRUE),
            sum(df[[interet_formation_complementaire_binaire]] == 1, na.rm = TRUE),
            sum(df[[formation_incite_devenir_MCS_binaire]] == 1, na.rm = TRUE),
            sum(df[[materiel_incite_MCS_binaire]] == 1, na.rm = TRUE)),
  Pct_Oui = c(pct_q11[2], pct_q12[2], pct_q13[2], pct_q14[2], pct_q15[2], pct_q16[2])
)

print(desc_q11_q16)

# 1.2 Analyse univariee avec CJP

# Q11 vs CJP
table_q11_cjp <- table(df[[reseau_pertinent_binaire]], df$CJP)
table_q11_cjp_prop <- prop.table(table_q11_cjp, margin = 1) * 100
test_q11 <- if(any(table_q11_cjp < 5)) fisher.test(table_q11_cjp) else chisq.test(table_q11_cjp)
or_q11 <- (table_q11_cjp[2,2] * table_q11_cjp[1,1]) / (table_q11_cjp[2,1] * table_q11_cjp[1,2])
log_or_q11 <- log(or_q11)
se_log_or_q11 <- sqrt(sum(1/table_q11_cjp))
ic_inf_q11 <- exp(log_or_q11 - 1.96 * se_log_or_q11)
ic_sup_q11 <- exp(log_or_q11 + 1.96 * se_log_or_q11)

# Q12 vs CJP
table_q12_cjp <- table(df[[derniere_formation_5_ans_binaire]], df$CJP)
table_q12_cjp_prop <- prop.table(table_q12_cjp, margin = 1) * 100
test_q12 <- if(any(table_q12_cjp < 5)) fisher.test(table_q12_cjp) else chisq.test(table_q12_cjp)
or_q12 <- (table_q12_cjp[2,2] * table_q12_cjp[1,1]) / (table_q12_cjp[2,1] * table_q12_cjp[1,2])
log_or_q12 <- log(or_q12)
se_log_or_q12 <- sqrt(sum(1/table_q12_cjp))
ic_inf_q12 <- exp(log_or_q12 - 1.96 * se_log_or_q12)
ic_sup_q12 <- exp(log_or_q12 + 1.96 * se_log_or_q12)

# Q13 vs CJP
table_q13_cjp <- table(df[[cabinet_adapte_binaire]], df$CJP)
table_q13_cjp_prop <- prop.table(table_q13_cjp, margin = 1) * 100
test_q13 <- if(any(table_q13_cjp < 5)) fisher.test(table_q13_cjp) else chisq.test(table_q13_cjp)
or_q13 <- (table_q13_cjp[2,2] * table_q13_cjp[1,1]) / (table_q13_cjp[2,1] * table_q13_cjp[1,2])
log_or_q13 <- log(or_q13)
se_log_or_q13 <- sqrt(sum(1/table_q13_cjp))
ic_inf_q13 <- exp(log_or_q13 - 1.96 * se_log_or_q13)
ic_sup_q13 <- exp(log_or_q13 + 1.96 * se_log_or_q13)

# Q14 vs CJP
table_q14_cjp <- table(df[[interet_formation_complementaire_binaire]], df$CJP)
table_q14_cjp_prop <- prop.table(table_q14_cjp, margin = 1) * 100
test_q14 <- if(any(table_q14_cjp < 5)) fisher.test(table_q14_cjp) else chisq.test(table_q14_cjp)

# Q15 vs CJP
table_q15_cjp <- table(df[[formation_incite_devenir_MCS_binaire]], df$CJP)
table_q15_cjp_prop <- prop.table(table_q15_cjp, margin = 1) * 100
test_q15 <- if(any(table_q15_cjp < 5)) fisher.test(table_q15_cjp) else chisq.test(table_q15_cjp)
or_q15 <- (table_q15_cjp[2,2] * table_q15_cjp[1,1]) / (table_q15_cjp[2,1] * table_q15_cjp[1,2])
log_or_q15 <- log(or_q15)
se_log_or_q15 <- sqrt(sum(1/table_q15_cjp))
ic_inf_q15 <- exp(log_or_q15 - 1.96 * se_log_or_q15)
ic_sup_q15 <- exp(log_or_q15 + 1.96 * se_log_or_q15)

# Q16 vs CJP
table_q16_cjp <- table(df[[materiel_incite_MCS_binaire]], df$CJP)
table_q16_cjp_prop <- prop.table(table_q16_cjp, margin = 1) * 100
test_q16 <- if(any(table_q16_cjp < 5)) fisher.test(table_q16_cjp) else chisq.test(table_q16_cjp)
or_q16 <- (table_q16_cjp[2,2] * table_q16_cjp[1,1]) / (table_q16_cjp[2,1] * table_q16_cjp[1,2])
log_or_q16 <- log(or_q16)
se_log_or_q16 <- sqrt(sum(1/table_q16_cjp))
ic_inf_q16 <- exp(log_or_q16 - 1.96 * se_log_or_q16)
ic_sup_q16 <- exp(log_or_q16 + 1.96 * se_log_or_q16)

resultats_univaries <- data.frame(
  Variable = c("Q11: Réseau MCS pertinent",
               "Q12: Formation < 5 ans",
               "Q13: Cabinet adapté urgences",
               "Q14: Intérêt formation complémentaire",
               "Q15: Formation incite MCS",
               "Q16: Matériel incite MCS"),
  OR_brut = c(or_q11, or_q12, or_q13, NA, or_q15, or_q16),
  IC_inf = c(ic_inf_q11, ic_inf_q12, ic_inf_q13, NA, ic_inf_q15, ic_inf_q16),
  IC_sup = c(ic_sup_q11, ic_sup_q12, ic_sup_q13, NA, ic_sup_q15, ic_sup_q16),
  p_value = c(test_q11$p.value, test_q12$p.value, test_q13$p.value, 
              test_q14$p.value, test_q15$p.value, test_q16$p.value)
)

print(resultats_univaries)

# Tableaux gtsummary pour analyse univariee
df_q11_q16 <- df %>%
  select(all_of(c(reseau_pertinent_binaire, derniere_formation_5_ans_binaire,
                  cabinet_adapte_binaire, interet_formation_complementaire_binaire,
                  formation_incite_devenir_MCS_binaire, materiel_incite_MCS_binaire,
                  "CJP")))

df_q11_q16 <- df_q11_q16 %>%
  mutate(across(where(is.numeric), as.factor))

df_q11_q16 <- df_q11_q16 %>%
  rename(
    "Réseau MCS pertinent" = all_of(reseau_pertinent_binaire),
    "Formation < 5 ans" = all_of(derniere_formation_5_ans_binaire),
    "Cabinet adapté urgences" = all_of(cabinet_adapte_binaire),
    "Intérêt formation complémentaire" = all_of(interet_formation_complementaire_binaire),
    "Formation incite MCS" = all_of(formation_incite_devenir_MCS_binaire),
    "Matériel incite MCS" = all_of(materiel_incite_MCS_binaire),
    "Intéressé pour MCS" = CJP
  )

table_univariee_gts <- df_q11_q16 %>%
  tbl_summary(by = "Intéressé pour MCS",
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1))) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels()

print(table_univariee_gts)

# 1.3 Analyse multivariee : regression logistique de Firth

df_complet_q11_q16 <- df %>%
  select(all_of(c(reseau_pertinent_binaire, derniere_formation_5_ans_binaire,
                  cabinet_adapte_binaire, interet_formation_complementaire_binaire,
                  formation_incite_devenir_MCS_binaire, materiel_incite_MCS_binaire,
                  "CJP"))) %>%
  na.omit()

formule_firth <- as.formula(paste("CJP ~", 
                                  paste(c(reseau_pertinent_binaire, 
                                          derniere_formation_5_ans_binaire,
                                          cabinet_adapte_binaire, 
                                          interet_formation_complementaire_binaire,
                                          formation_incite_devenir_MCS_binaire, 
                                          materiel_incite_MCS_binaire), 
                                        collapse = " + ")))

modele_firth_q11_q16 <- logistf(formule_firth, data = df_complet_q11_q16)

summary(modele_firth_q11_q16)

coef_firth <- modele_firth_q11_q16$coefficients[-1]
or_ajustes <- exp(coef_firth)
ic_inf_ajustes <- exp(modele_firth_q11_q16$ci.lower[-1])
ic_sup_ajustes <- exp(modele_firth_q11_q16$ci.upper[-1])
p_values_ajustes <- modele_firth_q11_q16$prob[-1]

resultats_multivaries <- data.frame(
  Variable = c("Q11: Réseau MCS pertinent",
               "Q12: Formation < 5 ans",
               "Q13: Cabinet adapté urgences",
               "Q14: Intérêt formation complémentaire",
               "Q15: Formation incite MCS",
               "Q16: Matériel incite MCS"),
  OR_ajuste = or_ajustes,
  IC_inf_ajuste = ic_inf_ajustes,
  IC_sup_ajuste = ic_sup_ajustes,
  p_value_ajuste = p_values_ajustes
)

print(resultats_multivaries)

# ============================================================================
# PARTIE 2 : ANALYSE MOTIVATIONS ET FREINS
# ============================================================================

# 2.1 Analyse descriptive separee selon CJP

df_interesses <- df %>% filter(CJP == 1)
df_non_interesses <- df %>% filter(CJP == 0)

# MOTIVATIONS chez les interesses
n_interesses <- nrow(df_interesses)

n_motiv_reconnaissance <- sum(df_interesses[[motiv_reconnaissance]] == 1, na.rm = TRUE)
pct_motiv_reconnaissance <- (n_motiv_reconnaissance / n_interesses) * 100

n_motiv_soutien <- sum(df_interesses[[motiv_soutien_materiel]] == 1, na.rm = TRUE)
pct_motiv_soutien <- (n_motiv_soutien / n_interesses) * 100

n_motiv_valorisation <- sum(df_interesses[[motiv_valorisation_financiere]] == 1, na.rm = TRUE)
pct_motiv_valorisation <- (n_motiv_valorisation / n_interesses) * 100

n_motiv_formation <- sum(df_interesses[[motiv_formation_accompagnement]] == 1, na.rm = TRUE)
pct_motiv_formation <- (n_motiv_formation / n_interesses) * 100

tableau_motivations <- data.frame(
  Motivation = c("Reconnaissance/lien SAMU", 
                 "Soutien et matériel adaptés",
                 "Valorisation financière",
                 "Formation et accompagnement renforcés"),
  N = c(n_motiv_reconnaissance, n_motiv_soutien, n_motiv_valorisation, n_motiv_formation),
  Pourcentage = c(pct_motiv_reconnaissance, pct_motiv_soutien, 
                  pct_motiv_valorisation, pct_motiv_formation)
)

print(tableau_motivations)

# Graphique motivations
ggplot_motivations <- ggplot(tableau_motivations, 
                             aes(x = reorder(Motivation, Pourcentage), 
                                 y = Pourcentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Motivations à devenir MCS chez les médecins intéressés",
       x = "",
       y = "Pourcentage (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), 
            hjust = -0.2, size = 4)

print(ggplot_motivations)

# FREINS chez les non interesses
n_non_interesses <- nrow(df_non_interesses)

n_frein_charge <- sum(df_non_interesses[[frein_charge_travail]] == 1, na.rm = TRUE)
pct_frein_charge <- (n_frein_charge / n_non_interesses) * 100

n_frein_formation <- sum(df_non_interesses[[frein_manque_formation]] == 1, na.rm = TRUE)
pct_frein_formation <- (n_frein_formation / n_non_interesses) * 100

n_frein_admin <- sum(df_non_interesses[[frein_contraintes_admin]] == 1, na.rm = TRUE)
pct_frein_admin <- (n_frein_admin / n_non_interesses) * 100

tableau_freins <- data.frame(
  Frein = c("Charge de travail supplémentaire",
            "Manque de formation en urgence",
            "Contraintes administratives/organisationnelles"),
  N = c(n_frein_charge, n_frein_formation, n_frein_admin),
  Pourcentage = c(pct_frein_charge, pct_frein_formation, pct_frein_admin)
)

print(tableau_freins)

# Graphique freins
ggplot_freins <- ggplot(tableau_freins, 
                        aes(x = reorder(Frein, Pourcentage), 
                            y = Pourcentage)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Freins à devenir MCS chez les médecins non intéressés",
       x = "",
       y = "Pourcentage (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), 
            hjust = -0.2, size = 4)

print(ggplot_freins)

# 2.2 Scores composites

# Score motivations
df_interesses$score_motivations <- rowSums(df_interesses[, c(motiv_reconnaissance, 
                                                             motiv_soutien_materiel,
                                                             motiv_valorisation_financiere,
                                                             motiv_formation_accompagnement)], 
                                           na.rm = TRUE)

summary_score_motivations <- summary(df_interesses$score_motivations)
print(summary_score_motivations)

quartiles_motiv <- quantile(df_interesses$score_motivations, probs = c(0.25, 0.5, 0.75))
print(quartiles_motiv)

df_interesses$profil_motivation <- cut(df_interesses$score_motivations,
                                       breaks = c(-Inf, 1, 2, Inf),
                                       labels = c("Faiblement motivé", 
                                                  "Moyennement motivé", 
                                                  "Fortement motivé"))

table_profil_motiv <- table(df_interesses$profil_motivation)
prop_profil_motiv <- prop.table(table_profil_motiv) * 100
print(table_profil_motiv)
print(prop_profil_motiv)

# Score freins
df_non_interesses$score_freins <- rowSums(df_non_interesses[, c(frein_charge_travail,
                                                                frein_manque_formation,
                                                                frein_contraintes_admin)], 
                                          na.rm = TRUE)

summary_score_freins <- summary(df_non_interesses$score_freins)
print(summary_score_freins)

quartiles_freins <- quantile(df_non_interesses$score_freins, probs = c(0.25, 0.5, 0.75))
print(quartiles_freins)

df_non_interesses$profil_freins <- cut(df_non_interesses$score_freins,
                                       breaks = c(-Inf, 1, 2, Inf),
                                       labels = c("Peu freiné", 
                                                  "Moyennement freiné", 
                                                  "Très freiné"))

table_profil_freins <- table(df_non_interesses$profil_freins)
prop_profil_freins <- prop.table(table_profil_freins) * 100
print(table_profil_freins)
print(prop_profil_freins)

# 2.3 Analyse individuelle : associations entre motivations

table_reconnaissance_soutien <- table(df_interesses[[motiv_reconnaissance]], 
                                      df_interesses[[motiv_soutien_materiel]])
print(table_reconnaissance_soutien)

table_reconnaissance_valorisation <- table(df_interesses[[motiv_reconnaissance]], 
                                           df_interesses[[motiv_valorisation_financiere]])
print(table_reconnaissance_valorisation)

table_reconnaissance_formation <- table(df_interesses[[motiv_reconnaissance]], 
                                        df_interesses[[motiv_formation_accompagnement]])
print(table_reconnaissance_formation)

table_soutien_valorisation <- table(df_interesses[[motiv_soutien_materiel]], 
                                    df_interesses[[motiv_valorisation_financiere]])
print(table_soutien_valorisation)

table_soutien_formation <- table(df_interesses[[motiv_soutien_materiel]], 
                                 df_interesses[[motiv_formation_accompagnement]])
print(table_soutien_formation)

table_valorisation_formation <- table(df_interesses[[motiv_valorisation_financiere]], 
                                      df_interesses[[motiv_formation_accompagnement]])
print(table_valorisation_formation)

# Analyse individuelle : associations entre freins

table_charge_formation <- table(df_non_interesses[[frein_charge_travail]], 
                                df_non_interesses[[frein_manque_formation]])
print(table_charge_formation)

table_charge_admin <- table(df_non_interesses[[frein_charge_travail]], 
                            df_non_interesses[[frein_contraintes_admin]])
print(table_charge_admin)

table_formation_admin <- table(df_non_interesses[[frein_manque_formation]], 
                               df_non_interesses[[frein_contraintes_admin]])
print(table_formation_admin)

# ============================================================================
# PARTIE 3 : PROFILS DE MEDECINS
# ============================================================================

# 3.1 et 3.2 : Classification en 2 profils selon CJP

df_profils <- df %>%
  select(all_of(c(connaissance_mcs, sexe_homme, age_inf_50, profession_isolee_var,
                  duree_install_inf_10, activite_autre, cs_rdv, cs_sans_rdv,
                  cs_urgence, visites, "CJP"))) %>%
  na.omit()

df_profils$profil <- ifelse(df_profils$CJP == 1, "Intéressé", "Non intéressé")

# Description des profils

profil_interesse <- df_profils %>% filter(profil == "Intéressé")
profil_non_interesse <- df_profils %>% filter(profil == "Non intéressé")

# Statistiques descriptives par profil
prop_connaissance_interesse <- mean(profil_interesse[[connaissance_mcs]], na.rm = TRUE) * 100
prop_connaissance_non_interesse <- mean(profil_non_interesse[[connaissance_mcs]], na.rm = TRUE) * 100

prop_homme_interesse <- mean(profil_interesse[[sexe_homme]], na.rm = TRUE) * 100
prop_homme_non_interesse <- mean(profil_non_interesse[[sexe_homme]], na.rm = TRUE) * 100

prop_age50_interesse <- mean(profil_interesse[[age_inf_50]], na.rm = TRUE) * 100
prop_age50_non_interesse <- mean(profil_non_interesse[[age_inf_50]], na.rm = TRUE) * 100

prop_isole_interesse <- mean(profil_interesse[[profession_isolee_var]], na.rm = TRUE) * 100
prop_isole_non_interesse <- mean(profil_non_interesse[[profession_isolee_var]], na.rm = TRUE) * 100

prop_duree10_interesse <- mean(profil_interesse[[duree_install_inf_10]], na.rm = TRUE) * 100
prop_duree10_non_interesse <- mean(profil_non_interesse[[duree_install_inf_10]], na.rm = TRUE) * 100

prop_autre_interesse <- mean(profil_interesse[[activite_autre]], na.rm = TRUE) * 100
prop_autre_non_interesse <- mean(profil_non_interesse[[activite_autre]], na.rm = TRUE) * 100

prop_rdv_interesse <- mean(profil_interesse[[cs_rdv]], na.rm = TRUE) * 100
prop_rdv_non_interesse <- mean(profil_non_interesse[[cs_rdv]], na.rm = TRUE) * 100

prop_sans_rdv_interesse <- mean(profil_interesse[[cs_sans_rdv]], na.rm = TRUE) * 100
prop_sans_rdv_non_interesse <- mean(profil_non_interesse[[cs_sans_rdv]], na.rm = TRUE) * 100

prop_urgence_interesse <- mean(profil_interesse[[cs_urgence]], na.rm = TRUE) * 100
prop_urgence_non_interesse <- mean(profil_non_interesse[[cs_urgence]], na.rm = TRUE) * 100

prop_visites_interesse <- mean(profil_interesse[[visites]], na.rm = TRUE) * 100
prop_visites_non_interesse <- mean(profil_non_interesse[[visites]], na.rm = TRUE) * 100

tableau_profils <- data.frame(
  Variable = c("Connaissance MCS", "Homme", "Âge < 50 ans", "Profession isolée",
               "Durée installation < 10 ans", "Activité autre que libéral exclusif",
               "Consultations RDV", "Consultations sans RDV", 
               "Créneaux urgence", "Visites"),
  Interesses_pct = c(prop_connaissance_interesse, prop_homme_interesse, 
                     prop_age50_interesse, prop_isole_interesse,
                     prop_duree10_interesse, prop_autre_interesse,
                     prop_rdv_interesse, prop_sans_rdv_interesse,
                     prop_urgence_interesse, prop_visites_interesse),
  Non_interesses_pct = c(prop_connaissance_non_interesse, prop_homme_non_interesse,
                         prop_age50_non_interesse, prop_isole_non_interesse,
                         prop_duree10_non_interesse, prop_autre_non_interesse,
                         prop_rdv_non_interesse, prop_sans_rdv_non_interesse,
                         prop_urgence_non_interesse, prop_visites_non_interesse)
)

print(tableau_profils)

# 3.3 Tests statistiques pour chaque caracteristique selon profil

test_connaissance <- fisher.test(table(df_profils[[connaissance_mcs]], df_profils$profil))
test_sexe <- fisher.test(table(df_profils[[sexe_homme]], df_profils$profil))
test_age <- fisher.test(table(df_profils[[age_inf_50]], df_profils$profil))
test_isole <- fisher.test(table(df_profils[[profession_isolee_var]], df_profils$profil))
test_duree <- fisher.test(table(df_profils[[duree_install_inf_10]], df_profils$profil))
test_autre <- fisher.test(table(df_profils[[activite_autre]], df_profils$profil))
test_rdv <- fisher.test(table(df_profils[[cs_rdv]], df_profils$profil))
test_sans_rdv <- fisher.test(table(df_profils[[cs_sans_rdv]], df_profils$profil))
test_urgence <- fisher.test(table(df_profils[[cs_urgence]], df_profils$profil))
test_visites <- fisher.test(table(df_profils[[visites]], df_profils$profil))

tests_profils <- data.frame(
  Variable = c("Connaissance MCS", "Homme", "Âge < 50 ans", "Profession isolée",
               "Durée installation < 10 ans", "Activité autre que libéral exclusif",
               "Consultations RDV", "Consultations sans RDV", 
               "Créneaux urgence", "Visites"),
  p_value = c(test_connaissance$p.value, test_sexe$p.value, test_age$p.value,
              test_isole$p.value, test_duree$p.value, test_autre$p.value,
              test_rdv$p.value, test_sans_rdv$p.value, test_urgence$p.value,
              test_visites$p.value)
)

print(tests_profils)

# ============================================================================
# PARTIE 4 : TABLEAUX RECAPITULATIFS FINAUX
# ============================================================================

# Tableau gtsummary pour profils
df_profils_gts <- df_profils %>%
  mutate(across(where(is.numeric), as.factor))

df_profils_gts <- df_profils_gts %>%
  select(-CJP) %>%
  rename(
    "Connaissance MCS" = all_of(connaissance_mcs),
    "Homme" = all_of(sexe_homme),
    "Âge < 50 ans" = all_of(age_inf_50),
    "Profession isolée" = all_of(profession_isolee_var),
    "Durée installation < 10 ans" = all_of(duree_install_inf_10),
    "Activité autre" = all_of(activite_autre),
    "Consultations RDV" = all_of(cs_rdv),
    "Consultations sans RDV" = all_of(cs_sans_rdv),
    "Créneaux urgence" = all_of(cs_urgence),
    "Visites" = all_of(visites),
    "Profil" = profil
  )

table_profils_gts <- df_profils_gts %>%
  tbl_summary(by = "Profil",
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1))) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels()

print(table_profils_gts)

# Tableau final combinant univarie et multivarie Q11-Q16
tableau_final_q11_q16 <- data.frame(
  Variable = resultats_univaries$Variable,
  OR_brut = round(resultats_univaries$OR_brut, 2),
  IC95_brut = paste0("[", round(resultats_univaries$IC_inf, 2), " - ", 
                     round(resultats_univaries$IC_sup, 2), "]"),
  p_brut = round(resultats_univaries$p_value, 3),
  OR_ajuste = round(resultats_multivaries$OR_ajuste, 2),
  IC95_ajuste = paste0("[", round(resultats_multivaries$IC_inf_ajuste, 2), " - ",
                       round(resultats_multivaries$IC_sup_ajuste, 2), "]"),
  p_ajuste = round(resultats_multivaries$p_value_ajuste, 3)
)

print(tableau_final_q11_q16)

# Code pour forest plot
or_data_forest <- data.frame(
  Variable = c("Q11: Réseau MCS pertinent",
               "Q12: Formation < 5 ans",
               "Q13: Cabinet adapté urgences",
               "Q14: Intérêt formation complémentaire",
               "Q15: Formation incite MCS",
               "Q16: Matériel incite MCS"),
  OR = resultats_multivaries$OR_ajuste,
  IC_inf = resultats_multivaries$IC_inf_ajuste,
  IC_sup = resultats_multivaries$IC_sup_ajuste
)

forest_plot <- ggplot(or_data_forest, aes(x = OR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(title = "Forest plot : OR ajustés (Régression de Firth)",
       x = "Odds Ratio (échelle log)",
       y = "") +
  theme_minimal()

print(forest_plot)
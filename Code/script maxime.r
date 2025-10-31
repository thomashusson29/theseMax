################################################################################
# ANALYSE COMPLeTE - eTUDE MCS ReUNION
# Facteurs associes à l'interet pour devenir Medecin Correspondant du SAMU
# 
# Prerequis : dataframe "df" charge dans l'environnement
# Date : Octobre 2025
# Version : 1.0 - Testee et fonctionnelle
################################################################################

# ==============================================================================
# 0. PACKAGES NeCESSAIRES
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, gtsummary, flextable, ggplot2, gridExtra, 
  logistf, broom, scales
)

cat("✓ Packages charges avec succes\n\n")

# ==============================================================================
# 1. PRePARATION DES DONNeES
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("PRePARATION DES DONNeES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

if (!exists("df")) stop("ERREUR : Le dataframe 'df' n'existe pas")

n_total <- nrow(df)
cat("Effectif total :", n_total, "observations\n")
cat("Nombre de variables :", ncol(df), "\n\n")

# Creer df_clean
df_clean <- df %>%
  mutate(
    interet_mcs = df[[43]],
    sexe = `2_Sexe`,
    sexe_homme = `2_Sexe_Homme`,
    age = `3_Age`,
    duree_install = `6_Duree_d_installation`,
    perte_chance_bin = `10_Delai_d_intervention_:_perte_de_chance_dans_votre_secteur_binaire`,
    reseau_pertinent_bin = `11_Reseau_MCS_pertinent_pour_La_Reunion_binaire`,
    formation_5ans_bin = `12_Dernieres_formations_d_urgence_<_5_ans_binaire`,
    cabinet_adapte_bin = `13_Cabinet_adapte_aux_urgences_binaire`,
    age_moins50 = ifelse(age %in% c("Entre 30 et 39 ans", "Entre 40 et 49 ans"), 1, 0),
    duree_moins10 = ifelse(duree_install %in% c("Moins de 5 ans", "Entre 5 et 9 ans"), 1, 0)
  )

# Variables supplementaires
interet_formation_bin <- ifelse(
  df$`14_Interet_pour_formation_complementaire_en_urgence...27` %in% 
    c("1, tout à fait", "Plutôt 1"), 1, 0)

formation_incite_bin <- ifelse(
  df$`15_Si_+_forme_aux_urgences:_incitation_à_devenir_MCS` %in% 
    c("1, tout à fait", "Plutôt 1"), 1, 0)

# Variables motivations (Q17)
motiv_reconnaissance <- df$`17__Motivations_MCS:_Reconnaissance_(lien_avec_le_SAMU)`
motiv_materiel <- df$`17_Motivations_MCS:_Soutien_et_materiel_adaptes`
motiv_financiere <- df$`17_Motivations_MCS:_Valorisation_financiere_`
motiv_formation <- df$`17_Motivations_MCS:_Formation_et_accompagnement_renforces_(_reseau_d_entraide_et_de_partage)`

# Variables freins (Q18)
frein_charge <- df$`18_Freins_[Charge_de_travail_supplementaire]`
frein_formation <- df$`18_Freins_[Manque_de_formation_en_urgence]`
frein_admin <- df$`18_Freins_[Contraintes_administratives_ou_organisationnelles]`

# Scores composites
score_motivations <- motiv_reconnaissance + motiv_materiel + motiv_financiere + 
  motiv_formation + df$`17_Quelles_motivations_vous_inciteraient_à_devenir_MCS_[Autre]`

score_freins <- frein_charge + frein_formation + frein_admin + 
  df$`18_Quels_seraient,_selon_vous,_les_principaux_freins_à_votre_engagement_en_tant_que_MCS_[Autre]`

cat("✓ Variables creees et transformees\n\n")

# ==============================================================================
# 2. STATISTIQUES DESCRIPTIVES
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("TABLEAU 1 : STATISTIQUES DESCRIPTIVES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("CRITeRE DE JUGEMENT PRINCIPAL\n------------------------------\n")
cat(sprintf("Interet pour devenir MCS : %d/%d (%.1f%%)\n\n",
            sum(df_clean$interet_mcs), n_total,
            sum(df_clean$interet_mcs)/n_total*100))

cat("CARACTeRISTIQUES SOCIODeMOGRAPHIQUES\n------------------------------------\n")
table_sexe <- table(df_clean$sexe)
prop_sexe <- prop.table(table_sexe) * 100
for(i in 1:length(table_sexe)) {
  cat(sprintf("  %s : %d (%.1f%%)\n", names(table_sexe)[i], table_sexe[i], prop_sexe[i]))
}
cat("\n")

table_age <- table(df_clean$age)
age_order <- c("Entre 30 et 39 ans", "Entre 40 et 49 ans", "Entre 50 et 59 ans", 
               "Entre 60 et 69 ans", "Plus de 70 ans")
table_age <- table_age[age_order[age_order %in% names(table_age)]]
prop_age <- prop.table(table_age) * 100
cat("Age:\n")
for(i in 1:length(table_age)) {
  cat(sprintf("  %s : %d (%.1f%%)\n", names(table_age)[i], table_age[i], prop_age[i]))
}
cat("\n")

cat("MOTIVATIONS (Q17)\n-----------------\n")
cat(sprintf("  Reconnaissance : %d (%.1f%%)\n", 
            sum(motiv_reconnaissance), sum(motiv_reconnaissance)/n_total*100))
cat(sprintf("  Soutien et materiel : %d (%.1f%%)\n", 
            sum(motiv_materiel), sum(motiv_materiel)/n_total*100))
cat(sprintf("  Valorisation financiere : %d (%.1f%%)\n", 
            sum(motiv_financiere), sum(motiv_financiere)/n_total*100))
cat(sprintf("  Formation et accompagnement : %d (%.1f%%)\n\n", 
            sum(motiv_formation), sum(motiv_formation)/n_total*100))

cat("FREINS (Q18)\n------------\n")
cat(sprintf("  Charge de travail : %d (%.1f%%)\n", 
            sum(frein_charge), sum(frein_charge)/n_total*100))
cat(sprintf("  Manque de formation : %d (%.1f%%)\n", 
            sum(frein_formation), sum(frein_formation)/n_total*100))
cat(sprintf("  Contraintes admin : %d (%.1f%%)\n\n", 
            sum(frein_admin), sum(frein_admin)/n_total*100))

# ==============================================================================
# 3. ANALYSES UNIVARIeES
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("TABLEAU 2 : ANALYSES UNIVARIeES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

print_univariate <- function(var, var_label, outcome) {
  cat(var_label, "\n", paste(rep("-", nchar(var_label)), collapse=""), "\n")
  tab <- table(var, outcome)
  rownames(tab) <- c("Non", "Oui")
  colnames(tab) <- c("Non interesse", "Interesse")
  print(addmargins(tab))
  test <- fisher.test(tab)
  cat(sprintf("\nOR = %.2f [IC95%% : %.2f - %.2f]\n", 
              test$estimate, test$conf.int[1], test$conf.int[2]))
  cat(sprintf("Test de Fisher : p = %.4f", test$p.value))
  if(test$p.value < 0.05) cat(" ***")
  cat("\n\n")
  return(c(test$estimate, test$conf.int[1], test$conf.int[2], test$p.value))
}

results_list <- list()
results_list$sexe <- print_univariate(df_clean$sexe_homme, 
                                      "1. SEXE (Homme vs Femme)", df_clean$interet_mcs)
results_list$age <- print_univariate(df_clean$age_moins50, 
                                     "2. Age (< 50 ans vs ≥ 50 ans)", df_clean$interet_mcs)
results_list$duree <- print_univariate(df_clean$duree_moins10, 
                                       "3. DUReE D'INSTALLATION (< 10 ans vs ≥ 10 ans)", df_clean$interet_mcs)
results_list$perte_chance <- print_univariate(df_clean$perte_chance_bin, 
                                              "4. PERTE DE CHANCE PERÇUE", df_clean$interet_mcs)
results_list$reseau <- print_univariate(df_clean$reseau_pertinent_bin, 
                                        "5. ReSEAU MCS PERTINENT", df_clean$interet_mcs)
results_list$formation <- print_univariate(df_clean$formation_5ans_bin, 
                                           "6. FORMATION < 5 ANS", df_clean$interet_mcs)
results_list$cabinet <- print_univariate(df_clean$cabinet_adapte_bin, 
                                         "7. CABINET ADAPTe", df_clean$interet_mcs)
results_list$interet_formation <- print_univariate(interet_formation_bin, 
                                                   "8. INTeReT POUR FORMATION", df_clean$interet_mcs)
results_list$formation_incite <- print_univariate(formation_incite_bin, 
                                                  "9. FORMATION INCITERAIT MCS", df_clean$interet_mcs)

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("ReSUMe DES ANALYSES UNIVARIeES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

results_df <- data.frame(
  Variable = c("Sexe (Homme)", "Age (< 50 ans)", "Duree < 10 ans",
               "Perte de chance", "Reseau pertinent", "Formation < 5 ans",
               "Cabinet adapte", "Interet formation", "Formation incite"),
  OR = sapply(results_list, function(x) round(x[1], 2)),
  IC_inf = sapply(results_list, function(x) round(x[2], 2)),
  IC_sup = sapply(results_list, function(x) round(x[3], 2)),
  p_value = sapply(results_list, function(x) round(x[4], 4))
)
results_df$IC95 <- paste0("[", results_df$IC_inf, " - ", results_df$IC_sup, "]")
results_df$Sig <- ifelse(results_df$p_value < 0.05, "***", 
                         ifelse(results_df$p_value < 0.10, ".", ""))

print(results_df[, c("Variable", "OR", "IC95", "p_value", "Sig")], row.names = FALSE)
cat("\n*** p < 0.05  . p < 0.10\n\n")

# ==============================================================================
# 4. ANALYSE MULTIVARIeE (FIRTH)
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("TABLEAU 3 : ReGRESSION LOGISTIQUE MULTIVARIeE (FIRTH)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

df_reg <- data.frame(
  interet_mcs = df_clean$interet_mcs,
  sexe_homme = df_clean$sexe_homme,
  age_moins50 = df_clean$age_moins50,
  perte_chance_bin = df_clean$perte_chance_bin,
  reseau_pertinent_bin = df_clean$reseau_pertinent_bin,
  cabinet_adapte_bin = df_clean$cabinet_adapte_bin,
  formation_incite_bin = formation_incite_bin
) %>% na.omit()

cat("N pour la regression :", nrow(df_reg), "\n\n")

model_firth <- logistf(
  interet_mcs ~ sexe_homme + age_moins50 + perte_chance_bin + 
    reseau_pertinent_bin + cabinet_adapte_bin + formation_incite_bin,
  data = df_reg
)

firth_results <- data.frame(
  Variable = c("Sexe (Homme)", "Age (< 50 ans)", "Perte de chance", 
               "Reseau pertinent", "Cabinet adapte", "Formation incite"),
  OR_ajuste = round(exp(model_firth$coefficients[-1]), 2),
  IC_inf = round(exp(model_firth$ci.lower[-1]), 2),
  IC_sup = round(exp(model_firth$ci.upper[-1]), 2),
  p_value = round(model_firth$prob[-1], 4)
)
firth_results$IC95 <- paste0("[", firth_results$IC_inf, " - ", firth_results$IC_sup, "]")
firth_results$Sig <- ifelse(firth_results$p_value < 0.05, "***", "")

print(firth_results[, c("Variable", "OR_ajuste", "IC95", "p_value", "Sig")], row.names = FALSE)
cat("\n*** p < 0.05\n\n")

# ==============================================================================
# 5. ANALYSES SPeCIFIQUES MOTIVATIONS ET FREINS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("ANALYSES MOTIVATIONS ET FREINS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("QUESTION 17 : MOTIVATIONS (analyses item par item)\n")
cat("---------------------------------------------------\n\n")

cat("A. Reconnaissance (lien SAMU)\n")
test_m1 <- fisher.test(table(motiv_reconnaissance, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_m1$estimate, test_m1$conf.int[1], test_m1$conf.int[2], test_m1$p.value))

cat("B. Soutien et materiel\n")
test_m2 <- fisher.test(table(motiv_materiel, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_m2$estimate, test_m2$conf.int[1], test_m2$conf.int[2], test_m2$p.value))

cat("C. Valorisation financiere\n")
test_m3 <- fisher.test(table(motiv_financiere, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_m3$estimate, test_m3$conf.int[1], test_m3$conf.int[2], test_m3$p.value))

cat("D. Formation et accompagnement\n")
test_m4 <- fisher.test(table(motiv_formation, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_m4$estimate, test_m4$conf.int[1], test_m4$conf.int[2], test_m4$p.value))

cat("QUESTION 18 : FREINS (analyses item par item)\n")
cat("----------------------------------------------\n\n")

cat("A. Charge de travail\n")
test_f1 <- fisher.test(table(frein_charge, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_f1$estimate, test_f1$conf.int[1], test_f1$conf.int[2], test_f1$p.value))

cat("B. Manque de formation\n")
test_f2 <- fisher.test(table(frein_formation, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_f2$estimate, test_f2$conf.int[1], test_f2$conf.int[2], test_f2$p.value))

cat("C. Contraintes administratives\n")
test_f3 <- fisher.test(table(frein_admin, df_clean$interet_mcs))
cat(sprintf("OR = %.2f [%.2f - %.2f], p = %.4f\n\n", 
            test_f3$estimate, test_f3$conf.int[1], test_f3$conf.int[2], test_f3$p.value))

cat("SCORES COMPOSITES\n-----------------\n\n")
cat("Score Motivations:\n")
cat(sprintf("  Interesses : %.2f (±%.2f)\n", 
            mean(score_motivations[df_clean$interet_mcs == 1]),
            sd(score_motivations[df_clean$interet_mcs == 1])))
cat(sprintf("  Non interesses : %.2f (±%.2f)\n", 
            mean(score_motivations[df_clean$interet_mcs == 0]),
            sd(score_motivations[df_clean$interet_mcs == 0])))
test_w_motiv <- wilcox.test(score_motivations ~ df_clean$interet_mcs)
cat(sprintf("  Test de Wilcoxon : p = %.4f\n\n", test_w_motiv$p.value))

cat("Score Freins:\n")
cat(sprintf("  Interesses : %.2f (±%.2f)\n", 
            mean(score_freins[df_clean$interet_mcs == 1]),
            sd(score_freins[df_clean$interet_mcs == 1])))
cat(sprintf("  Non interesses : %.2f (±%.2f)\n", 
            mean(score_freins[df_clean$interet_mcs == 0]),
            sd(score_freins[df_clean$interet_mcs == 0])))
test_w_freins <- wilcox.test(score_freins ~ df_clean$interet_mcs)
cat(sprintf("  Test de Wilcoxon : p = %.4f\n\n", test_w_freins$p.value))

# ==============================================================================
# 6. GRAPHIQUES
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("GeNeRATION DES GRAPHIQUES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Figure 1 : Caracteristiques sociodemographiques
df_graph <- df_clean %>%
  mutate(
    age_ordered = factor(age, levels = c("Entre 30 et 39 ans", "Entre 40 et 49 ans", 
                                         "Entre 50 et 59 ans", "Entre 60 et 69 ans", 
                                         "Plus de 70 ans")),
    duree_ordered = factor(duree_install, levels = c("Moins de 5 ans", "Entre 5 et 9 ans", 
                                                     "Entre 10 et 19 ans", "Plus de 20 ans")),
    interet_label = factor(interet_mcs, levels = c(0, 1), 
                           labels = c("Non interesse", "Interesse"))
  )

p1 <- ggplot(df_graph, aes(x = sexe, fill = sexe)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = paste0(after_stat(count), "\n(", 
                                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Feminin" = "#E06C9F", "Masculin" = "#6CB4E0")) +
  labs(title = "Repartition par sexe", x = "", y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11))

p2 <- ggplot(df_graph, aes(x = age_ordered, fill = age_ordered)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = paste0(after_stat(count), "\n(", 
                                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Repartition par Age", x = "", y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold", size = 11))

p3 <- ggplot(df_graph, aes(x = duree_ordered, fill = duree_ordered)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = paste0(after_stat(count), "\n(", 
                                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Duree d'installation", x = "", y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold", size = 11))

p4 <- ggplot(df_graph, aes(x = interet_label, fill = interet_label)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = paste0(after_stat(count), "\n(", 
                                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non interesse" = "#D9534F", "Interesse" = "#5CB85C")) +
  labs(title = "Interet pour devenir MCS\n(Critere principal)", x = "", y = "Effectif") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11))

grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = "Figure 1 : Caracteristiques sociodemographiques et critere principal")
cat("✓ Figure 1 creee\n")

# Figure 2 : Forest plot univarie
forest_data <- results_df[!is.infinite(results_df$OR), ]
forest_data$Variable <- factor(forest_data$Variable, levels = rev(forest_data$Variable))

p_forest <- ggplot(forest_data, aes(x = OR, y = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.3, linewidth = 0.8) +
  geom_point(aes(color = p_value < 0.05), size = 4) +
  scale_color_manual(values = c("FALSE" = "gray40", "TRUE" = "red"), 
                     labels = c("NS", "p < 0.05"), name = "") +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20, 40)) +
  labs(title = "Forest plot - Analyses univariees",
       subtitle = "Facteurs associes à l'interet pour devenir MCS",
       x = "Odds Ratio (echelle log)", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p_forest)
cat("✓ Forest plot univarie cree\n")

# Figure 3 : Forest plot multivarie
firth_plot_data <- firth_results
firth_plot_data$Variable <- factor(firth_plot_data$Variable, levels = rev(firth_plot_data$Variable))
firth_plot_data$Significatif <- firth_plot_data$p_value < 0.05

p_forest_multi <- ggplot(firth_plot_data, aes(x = OR_ajuste, y = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.3, linewidth = 0.8) +
  geom_point(aes(color = Significatif), size = 5) +
  scale_color_manual(values = c("FALSE" = "gray40", "TRUE" = "red"), 
                     labels = c("NS", "p < 0.05"), name = "") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100)) +
  labs(title = "Forest plot - Analyse multivariee (Regression de Firth)",
       subtitle = "Odds Ratios ajustes",
       x = "Odds Ratio ajuste (echelle log)", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"),
        axis.text.y = element_text(size = 11, face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p_forest_multi)
cat("✓ Forest plot multivarie cree\n\n")


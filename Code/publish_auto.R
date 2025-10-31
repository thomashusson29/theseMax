#push site max
# ============================================
# 🚀 Publication automatique sur GitHub Pages
# Projet : Thèse Maxime
# Auteur : Thomas Husson
# ============================================

# Charger les librairies nécessaires
if (!require(rmarkdown)) install.packages("rmarkdown")

# 1️⃣ Knit du rapport Rmd vers HTML
cat("⏳ Génération du rapport HTML...\n")
rmarkdown::render("Rapport-Thèse-Max.Rmd",
                  output_format = "html_document",
                  encoding = "UTF-8")

rmarkdown::render(
  input = "Rapport-Thèse-Max.Rmd",
  output_format = "word_document",
  encoding = "UTF-8"
)

# 2️⃣ Copie du rapport en index.html
cat("📁 Copie du rapport en index.html...\n")
file.copy("Rapport-Thèse-Max.html", "index.html", overwrite = TRUE)

# 3️⃣ Création d’un message de commit automatique daté
commit_msg <- sprintf("Mise à jour automatique du %s à %s",
                      format(Sys.Date(), "%d/%m/%Y"),
                      format(Sys.time(), "%Hh%M"))

# 4️⃣ Push Git automatique
cat("⬆️  Commit & Push sur GitHub...\n")
system("git add Rapport-Thèse-Max.Rmd Rapport-Thèse-Max.html index.html", ignore.stdout = TRUE)
system(paste0("git commit -m '", commit_msg, "'"), ignore.stdout = TRUE)
system("git push origin main", ignore.stdout = TRUE)

# 5️⃣ Message final
cat("\n✅ Rapport publié avec succès sur GitHub Pages !\n")
cat("🌐 Lien direct : https://thomashusson29.github.io/theseMax/\n")
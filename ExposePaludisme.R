# ============== Éxposé Paludisme =================

# 😍 0. Les Librairies ----------------------------

# Charger 'tidyverse' 
# Pour Importer 'dplyr' et 'ggplot2'

library(readxl)       # Data reader
library(tidyverse)    # data wrangling
library(plotly)       # data Viz
library(cowplot)      # data Viz
library(GGally)       # data Viz
library(corrplot)     # data Viz
library(labelled)     # data Labelled
library(explore)      # EDA
library(naniar)       # Missing Data
library(janitor)      # Data Cleanning






# 😍 1. Importation des données (BD) ----------------------

data <- readRDS("data/Donnees_Paludisme_2008_2022.rds")

# Affichage Structure 
data |> glimpse()



# 👉 Data Feature Engineering -----------------------

# Duplication de la BD
dpalu <- data

# Supprimer le nom des colonnes
for (i in 1:length(colnames(dpalu))) {
  colnames(dpalu)[i] <- paste("V", i, sep = "")
}

# Labellisation des colonnes
dpalu <- set_variable_labels(dpalu, .labels = names(data))

dpalu |> glimpse() # structure


# Viz des Données manquantes
dpalu |>
  gg_miss_var()

# Nombre de doublons 
table(duplicated(dpalu))
dpalu |> duplicated() |> table()


# Inspection des colonnes 
dpalu |> describe_all() |> 
  print(n = 22)

dpalu |> describe_cat(V2)

unique(dpalu$V2)

# Recodage de la colonne V2
dpalu <- dpalu |> 
  mutate(V2 = str_to_upper(V2))

unique(dpalu$V2)

# Création de la variables "trimestre" 

mois_fr <- c(
  "Janvier" = "01", "Février" = "02", "Mars" = "03", "Avril" = "04",
  "Mai" = "05", "Juin" = "06", "Juillet" = "07", "Août" = "08",
  "Septembre" = "09", "Octobre" = "10", "Novembre" = "11", "Décembre" = "12"
)

dpalu <- dpalu |>
  mutate(
    trimestre = mois_fr[V4],
    trimestre = ymd(paste(V3, trimestre, "01", sep = "-")),
    trimestre = quarter(trimestre),
    trimestre = paste("Trimestre", trimestre, sep = " ") |>
      factor(levels = c("Trimestre 4", "Trimestre 1", "Trimestre 2", "Trimestre 3"))
  )

# 🔍 Aalyse univarié -----------------------------------

  # Qualitative .....................................

    # Fréquence des régions dans les données 
dpalu |>
  mutate_if(is.character, as.factor) |>
  count(V2) |>
  ggplot(
    aes(x = fct_reorder(V2, n, .desc = F),
        y = n)
  ) +
  geom_bar(
    stat = "identity",
    fill = "#a2d9ce",
  ) +
  labs(
    x = "Régions",
    y = "Fréquence",
    title = "Fréquence des régions dans les données",
    subtitle = "avec "
  ) +
  geom_text(aes(label = n), size = 3, hjust = 1.5, col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

  # Quantitatif .....................................

# 📊 Population -------------------------------------

    # Population total par an 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V5) |>
  unique() |>
  group_by(V3) |>
  summarise(total_pop = sum(V5)) |>
  ggplot(
    aes(x = V3,
        y = total_pop)
  ) +
  labs(
    x = "Années",
    y = "Total population",
    title = "Population total par an",
    subtitle = "(2008 - 2022)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#e59866",
  ) +
  geom_text(
    aes(label = total_pop),
    size = 3,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Population total par région chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V2, V5) |>
  unique() |>
  group_by(V3, V2) |>
  summarise(total_pop = sum(V5)) |>
  ggplot(
    aes(
      x = V2,
      y = total_pop)
  ) +
  geom_bar(stat = "identity",
           fill = "#e59866"
  )+
  labs(
    x = "Région",
    y = "Population total",
    title = "Population total par région chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()
  
    # Population total par mois chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V4, V5) |>
  unique() |>
  group_by(V3, V4) |>
  summarise(total_pop = sum(V5)) |>
  ggplot(
    aes(
      x = V4,
      y = total_pop)
  ) +
  geom_bar(stat = "identity",
           fill = "#e59866"
  )+
  labs(
    x = "Mois",
    y = "Population total",
    title = "Population total par mois chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

# 📊 Consultation -----------------------------------

    # Nombre de consultation par an 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V6) |>
  unique() |>
  group_by(V3) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(x = fct_reorder(V3, total_consult),
        y = total_consult)
  ) +
  labs(
    x = "Années",
    y = "Nombre de consultation",
    title = "Nombre de consultation par an",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#5dade2",
  ) +
  geom_text(
    aes(label = total_consult),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de consultation par mois 
dpalu |>
  mutate(V4 = as.factor(V4)) |>
  select(V4, V6) |>
  unique() |>
  group_by(V4) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(x = fct_reorder(V4, total_consult),
        y = total_consult)
  ) +
  labs(
    x = "Mois",
    y = "Nombre de consultation",
    title = "Nombre de consultation par mois",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#5dade2",
  ) +
  geom_text(
    aes(label = total_consult),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de consultation par trimestre
dpalu |>
  mutate(trimestre = as.factor(trimestre)) |>
  select(trimestre, V6) |>
  unique() |>
  group_by(trimestre) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(x = fct_reorder(trimestre, total_consult),
        y = total_consult)
  ) +
  labs(
    x = "Trimestre",
    y = "Nombre de consultation",
    title = "Nombre de consultation par trimestre",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#5dade2",
  ) +
  geom_text(
    aes(label = total_consult),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de consultation par région 
dpalu |>
  select(V2, V6) |>
  unique() |>
  group_by(V2) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(x = fct_reorder(V2, total_consult),
        y = total_consult)
  ) +
  labs(
    x = "Région",
    y = "Nombre de consultation",
    title = "Nombre de consultation par région",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#5dade2",
  ) +
  geom_text(
    aes(label = total_consult),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de consultation par région chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V2, V6) |>
  unique() |>
  group_by(V3, V2) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(
      x = fct_reorder(V2, total_consult),
      y = total_consult)
  ) +
  geom_bar(stat = "identity",
           fill = "#5dade2"
           )+
  labs(
    x = "Région",
    y = "Nombre de consultation",
    title = "Nombre de consultation par région chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de consultation par mois chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V4, V6) |>
  unique() |>
  group_by(V3, V4) |>
  summarise(total_consult = sum(V6)) |>
  ggplot(
    aes(
      x = fct_reorder(V4, total_consult),
      y = total_consult)
  ) +
  geom_bar(stat = "identity",
           fill = "#5dade2"
  )+
  labs(
    x = "Mois",
    y = "Nombre de consultation",
    title = "Nombre de consultation par mois chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

# 📊 Test TDR positif -------------------------------

    # Nombre de test positif par an 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V9) |>
  unique() |>
  group_by(V3) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(x = fct_reorder(V3, total_positif),
        y = total_positif)
  ) +
  labs(
    x = "Années",
    y = "Nombre de test positif",
    title = "Nombre de test positif par an",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#c0392b",
  ) +
  geom_text(
    aes(label = total_positif),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par mois
dpalu |>
  select(V4, V9) |>
  unique() |>
  group_by(V4) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(x = fct_reorder(V4, total_positif),
        y = total_positif)
  ) +
  labs(
    x = "Mois",
    y = "Nombre de test positif",
    title = "Nombre de test positif par mois",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#c0392b",
  ) +
  geom_text(
    aes(label = total_positif),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par trimestre
dpalu |>
  mutate(trimestre = as.factor(trimestre)) |>
  select(trimestre, V9) |>
  unique() |>
  group_by(trimestre) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(x = fct_reorder(trimestre, total_positif),
        y = total_positif)
  ) +
  labs(
    x = "Trimestre",
    y = "Nombre de test positif",
    title = "Nombre de test positif par trimestre",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#c0392b",
  ) +
  geom_text(
    aes(label = total_positif),
    size = 3,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par région
dpalu |>
  select(V2, V9) |>
  unique() |>
  group_by(V2) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(x = fct_reorder(V2, total_positif),
        y = total_positif)
  ) +
  labs(
    x = "Région",
    y = "Nombre de test positif",
    title = "Nombre de test positif par Région",
    subtitle = "(Depuis 2008)"
  ) +
  geom_bar(
    stat = "identity",
    fill = "#c0392b",
  ) +
  geom_text(
    aes(label = total_positif),
    size = 5,
    hjust = 1,
    col = "white"
  ) +
  theme(
    panel.background = element_rect(fill = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par mois chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V4, V9) |>
  unique() |>
  group_by(V3, V4) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(
      x = fct_reorder(V4, total_positif),
      y = total_positif)
  ) +
  geom_bar(stat = "identity",
           fill = "#c0392b"
  )+
  labs(
    x = "Mois",
    y = "Nombre de test positif",
    title = "Nombre de test positif par mois chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par région chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, V2, V9) |>
  unique() |>
  group_by(V3, V2) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(
      x = fct_reorder(V2, total_positif),
      y = total_positif)
  ) +
  geom_bar(stat = "identity",
           fill = "#c0392b"
  )+
  labs(
    x = "Région",
    y = "Nombre de test positif",
    title = "Nombre de test positif par région chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()

    # Nombre de test positif par trimestre chaque année 
dpalu |>
  mutate(V3 = as.factor(V3)) |>
  select(V3, trimestre, V9) |>
  unique() |>
  group_by(V3, trimestre) |>
  summarise(total_positif = sum(V9)) |>
  ggplot(
    aes(
      x = fct_reorder(trimestre, total_positif),
      y = total_positif)
  ) +
  geom_bar(stat = "identity",
           fill = "#c0392b"
  )+
  labs(
    x = "Trimestre",
    y = "Nombre de test positif",
    title = "Nombre de test positif par trimestre chaque année",
    subtitle = "(2008 - 2022)"
  ) +
  facet_wrap(
    ~V3
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#f2f4f4"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  coord_flip()
# 🔍 Aalyse bivarié ------------------------------------

  # Quantitatif ~ Quantitatif

dpalu |>
  select(V3, V5:V9, V12:V14, V17) |>
  ggpairs() #ggpairs

dpalu |> 
  select(V3, V5:V9) |>
  cor() |> 
  corrplot::corrplot(type = "upper", 
                     order = "hclust", 
                     tl.col = "black", 
                     tl.srt = 45) #corrplot


# Régréssion lineaire multivarié --------------------

  # Manipulation ....................................

dpalu <- dpalu |>
  mutate_if(is.character, as.factor) # Convertion des 'chr' en 'fct' 

dpalu |>
  glimpse()

dpalu_model <- dpalu |>
  mutate(
    axe = ifelse(V2 %in% c("KOLDA", "TAMBACOUNDA", "KEDOUGOU", "SEDHIOU", "ZIGUINCHOR"), "SUD",
                 ifelse(V2 %in% c("DAKAR", "THIES"), "OUEST",
                        ifelse(V2 %in% c("LOUGA", "SAINT-LOUIS", "MATAM"), "NORD", "CENTRE"))) |>
      factor(levels = c("SUD", "OUEST", "CENTRE", "NORD"))
    ) |>
  select(axe, trimestre, V3, V6, V13, V8, V9)

dpalu_model <- dpalu_model |>
  mutate_if(is.character, as.factor)

dpalu_model |>
  glimpse()

# 🤖 Modèle lineaire multivarié ---------------------

modele_palu <- lm(V9 ~ ., data = dpalu_model)

summary(modele_palu)

# ⚙️ Evaluation du modèle ---------------------------

  # Multicolinearité ................................
multco <- modele_palu |> performance::check_collinearity()
modele_palu |> vif()

plot(multco)

  # Hypothèses pour les résidus du modèl ............

    # Hypothèses de Normalité des Résidus ...........
summary(modele_palu$residuals)

ggdensity(modele_palu$residuals, fill = "orange") # Visualisation avec ggdensity de ggpubr

ggqqplot(modele_palu$residuals) # Visualisation

shapiro_test(modele_palu$residuals) # Test de Shapiro avec shapiro_test de rstatix

check_normality(modele_palu) # Normalité avec check_normality de performance

    # Hypothèses d'Homoscédasticité des Résidus .....
plot(modele_palu, which = 3, pch = 20) # Visualisation

bptest(modele_palu) # Test de Breush-Pagan avec bptest de lmtest

    # Hypothèses Indépendance des Résidus ...........
acf(residuals(modele_palu), 
    main = "ACF : Auto-corrélation des Résidus") # Graphique ACF

dwtest(modele_palu) # Test de Durbin-Watson avec dwtest de lmtest

# 📋 Résultats du modèle lineaire -------------------
modele_palu |> tbl_regression(
  intercept = F, 
  label = list(groupe_region = "Régions", trimestre = "Trimestres", Annees = "Années")) |>
  bold_labels()



dpalu |> 
  select(V9) |>
  summarise(sum(V9))

dpalu |> 
  select(V16) |>
  summarise(sum(V16))

summary(dpalu)

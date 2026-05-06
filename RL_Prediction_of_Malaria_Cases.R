# ========================== Regression lineaire =============================



# 😍 0. Les Librairies ----------------------------

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
df <- data

# Supprimer le nom des colonnes
for (i in 1:length(colnames(df))) {
  colnames(df)[i] <- paste("V", i, sep = "")
}

# Labellisation des colonnes
df <- set_variable_labels(df, .labels = names(data))

df |> glimpse() # structure


# Viz des Données manquantes
gg_miss_var(df)

# Nombre de doublons 
table(duplicated(df))


# Inspection des colonnes 
df |> describe_all() |> 
  print(n = 22)

df |> describe_cat(V2)

unique(df$V2)

# Recodage de la colonne V2
df <- df |> 
  mutate(V2 = str_to_upper(V2))

unique(df$V2)




# 😍 2. EDA - Analyse Exploiratoire des Données ----------------------


# 👉 Analyse univariée ------------------------

# options(scipen = 999) # pas de notation scientifique




# Histogram : Nombre de Tests TDR Positifs  .....
ggplot(
  data    = df,
  mapping = aes(x = V9)
) + 
  geom_histogram(
    col   = "white",                                          # Couleur du contours des rectangles
    fill  = "cornflowerblue",                                 # Couleur de remplissage des rectangles
    bins  = 20
  ) +                 
  labs(
    x     = 'Tests TDR Positifs',                             # Etiquettes des Axes X et Y
    y     = 'Frequence',                          
    title = "Distribution du Nombre de Tests TDR Positifs"    # Le Titre
  ) +                        
  theme_minimal()                                             # Changement du thème de fond 



# Frequence des Régions dans les données
df |> 
  mutate_if(is.character, as.factor) |>
  count(V2) |> 
  ggplot(aes(
    x = fct_rev(fct_reorder(V2, n, .desc = T)), 
    y = n)
  ) +
  geom_bar(stat = "identity", fill = "#2c3e50") +
  coord_flip() +
  labs(
    y = "", x = "", 
    title = "Frequence des Régions"
  ) +
  theme_minimal()


# Evolution de la Population de 2008 à 2022
df |> 
  select(V3, V5) |> 
  unique() |> 
  mutate(V3 = as.factor(V3)) |>
  group_by(V3) |> 
  summarise(total = sum(V5)) |> 
  
  ggplot(aes(
    x = V3,
    y = total)
  ) +
  geom_bar(stat = "identity", fill = "#2c3e50") +
  coord_flip() +
  labs(
    y = "", x = "Années", 
    title = "Evolution de la Population du Sénégal de 2008 à 2022", 
    subtitle = "avec 17 738 748 d'habitants en 2022"
  ) +
  theme_minimal()

df_prep |>
  select(trimestre, V9) |>
  group_by(trimestre) |>
  summarise(total_trim = sum(V9)) |>
  ggplot(aes(x = trimestre,
             y = total_trim)
         ) +
  geom_bar(stat = "identity")


# 👉 Analyse Bivariée -----------------------------------

# Graphe des Corrélation
# Avec la fonction 'ggpairs()' de la librairie 'GGally'
df |> 
  select(V3, V5:V9) |> 
  ggpairs() +
  theme_minimal()



# Corrélogramme Avec la fonction 'corrplot()' de la librairie 'corrplot'
df |> 
  select(V3, V5:V9) |>
  cor() |> 
  corrplot::corrplot(type = "upper", 
                     order = "hclust", 
                     tl.col = "black", 
                     tl.srt = 45)




# 😍 3. Initiation aux Modèles de Régression --------------------


# 🚀 Cas d'une Régression Linéaire Multivariée -----------

"
- Problématique : 

  On cherche par exemple l'Effet des variables comme : 
  V3, V5, V7 etc.. sur V9. soit :

  Y = b0 + b1*X1 + b2*X2 + b3*X3 + ... + e

  V9 = b0 + b1*V3 + b2*V5 + b3*V7 + ... + e

- ❤️ RMQ : 
  La variable aléatoire e (terme d'erreur) doit être
  indépendante des variables explicatives Xi. 
  De plus e ~ N(0, s) (suit une loi Norale Centrée).

"



# 🚀 Modele Linéaire Initial (sans transforamtion) -------------------


# Pretraitement de la donnée avant modelisation ................

mois_fr <- c(
  "Janvier" = "01", "Février" = "02", "Mars" = "03", "Avril" = "04",
  "Mai" = "05", "Juin" = "06", "Juillet" = "07", "Août" = "08",
  "Septembre" = "09", "Octobre" = "10", "Novembre" = "11", "Décembre" = "12"
)

df_prep <- df |>
  mutate(mois_num = mois_fr[V4],
         date = paste(V3, mois_num, "01", sep = "-"),
         date = ymd(paste(V3, mois_num, "01", sep = "-")),
         trimestre = quarters(date) |>
           factor(levels = c('Q4', 'Q1', 'Q2', 'Q3')),
         localite = ifelse(
           V2 %in% c("KOLDA", "TAMBACOUNDA", "KEDOUGOU"), V2, 'Others Regions') |>
           factor(levels = c('Others Regions', "KOLDA", "TAMBACOUNDA", "KEDOUGOU"))
         ) |>
  select(localite, trimestre, V3, V6, V7, V9)

df_prep |>
  glimpse()

# df_prep <- df |>
#   mutate(
# 
#     date = ymd(paste(V3, V4, "01", sep = "-")),
# 
#     trimestre = quarters(date) |>
#       factor(levels = c("Q4", "Q1", "Q2", "Q3")),
# 
#     localite = ifelse(
#       V2 %in% c("KOLDA", "TAMBACOUNDA", "KEDOUGOU"), V2, "Others Regions") |>
#       factor(levels = c("Others Regions", "KEDOUGOU", "TAMBACOUNDA", "KOLDA"))
#   ) |>
# 
#   #select(localite, trimestre, V3, V5:V9, V14, V17)         # 1er choix des predicteurs
#   select(localite, trimestre, V3, V6, V7, V9)               # 2e choix des predicteurs



# Modele 1 : Estimation ...................
model_1 <- lm(V9 ~., data = df_prep)

# Affichage du Modele ...............
summary(model_1)



# 👉 Multicolinéarité dans la régression --------------------------


"  ❤️ Multicolinéarité 

- Dans une régression, la multicolinéarité est un 
  problème qui survient lorsque certaines variables 
  de prévision du modèle mesurent le même phénomène.
   
- Les conséquences de coefficients instables peuvent 
  être les suivantes :
   
👉 les coefficients peuvent sembler non significatifs, 
   même lorsqu’une relation significative existe entre 
   le prédicteur et la réponse ;
    
👉 les coefficients de prédicteurs fortement corrélés 
   varieront considérablement d’un échantillon à un autre.
    
👉 La multicolinéarité n’a aucune incidence sur l’adéquation
   de l’ajustement (R2), ni sur la qualité de la prévision.
   
   
- En effet avec le Facteur d'Inflation de la Variance (FIV)
   
👉 Si tous les FIV sont égaux à 1, il n’existe pas 
   de multicolinéarité.
   
👉 si les FIV sont entre 1 et 5, il existe une multi -
   colinéarité moyenne
   
👉 Sinon, il existe une multicolinéarité forte 


  En résumé, les FIV estiment à combien la variance d’un 
  coefficient peut augmenter en raison d’une relation 
  linéaire avec d’autres prédicteurs.
   
"



# Mesure de la colinéarité ..............................
mc <- model_1 |> performance::check_collinearity()
print(mc)

# VIZ 
plot(mc)



"
❤️ Le choix des predicteurs repose sur 2 principes :

- La multicolinéarité cad des variables qui fournissent 
  la même information au modèle.

- Ainsi, les meilleurs predicteurs sont les variables qui
  statistiquement sont les plus liées (corrélées) à la 
  variable cible ou dépendante et qui ne présentent pas 
  de multicolinéarité.
  
"

# Nullité de l'esperance des erreurs

model_1 |>
  residuals() |>
  mean()

# 🚀 Evaluation du Modele Linéaire Initial -----------------------------


"
❤️ Hypothèses pour les résidus du modèle 
   (e_i : terme d'erreur du mdele)

Notre modèle final est associé à trois autres 
hypothèses, en particulier dans les résidus. 

Il s’agit de :


1. Normalité                👉 H0 : e_i ∼ Normal(μ,σ)


2. Homoscédasticité         👉 H0 : σ2(e_i) = σ2


3. Pas d’auto-corrélation   👉 H0 : cor(e_i, e_i+k) = 0

"


# 👉 Hypothèses de Normalité des Résidus (Modele Initial) -------------


# Distribution des Résidus
summary(model_1$residuals)


# Histogramme : Viz de la Distribution
df_prep |> 
  mutate(
    residuals = model_1$residuals
  ) |> 
  
  ggplot(
    mapping = aes(x = residuals)
  ) + 
  geom_histogram(
    aes(y = stat(density)),
    col   = "white",                   
    fill  = "cornflowerblue"
  ) +                                 
  geom_density(
    na.rm    = T,                      
    col      = "orange",                
    size     = 1.1,                     
    linetype = 2                       
  ) +   
  labs(
    x     = 'Résidus',              
    y     = 'Densité',                          
    title = "Distribution des Résidus du Modèle Linéaire (Initial)"  
  ) +                        
  theme_minimal()

df_prep |>
  mutate(residus = model_1$residuals) |>
  ggplot(aes(x = residus)
         ) +
  geom_histogram(aes(y = stat(density))
                 ) +
  geom_density()

# Autre Méthode avce la librairie 'ggpubr' :

# Charger la librairie 'ggpubr'
library(ggpubr)

# Diagramme de densité
ggdensity(model_1$residuals, fill = "orange")

# QQ plot
ggqqplot(model_1$residuals)


" 👉 Commentaire : 

Plus les points se situent approximativement 
le long de cette ligne de référence, nous pouvons 
supposer une normalité.

"



# 👉 Test Shapiro de Noramalité (Cas des Résidus) ---------------

" ❤️ 

- Afin de faire des inférences sur les résultats 
  de ce processus de modélisation, il est nécessaire 
  d’établir hypothèses de distribution, 
  et pour faciliter les choses, nous allons supposer 
  que la variable est normalement distribuée (H0).
  
- Autrement dit, l’hypothèse nulle (H0) de ces tests 
  est que “la distribution de l’échantillon est normale”. 
  Si le test est significatif, la distribution est 
  non-normale.

- H0 : Y ~ N(u, s)

"


# Test Shapiro wilk avec la librairie 'rstatix' ........

# Charger la librairie 'rstatix'
library(rstatix)

# Test
shapiro_test(model_1$residuals)


" 👉 Commentaire : 

- D'après résultat ci-dessus, la p-value << 0,05 
  indiquant que la distribution des résidus 
  est significativement différente de la 
  distribution normale. 
  
  En d’autres termes, nous ne pouvons supposer 
  la normalité.

"

check_normality(model_1)



# 👉 Hypothèses d'Homoscédasticité des Résidus (Modele Initial) -------------

"
❤️ Vérifier que la variance des résidus est constante 
   (distribution homogène)


- Pour cela on trace le graphique suivant qui met en 
  relation les racines carrées des résidus (résidus 
  standardisés) en fonction des valeurs théoriques 
  (fitted-values) de Y prédites par l'équation de 
  la régression.



- On cherche ici une courbe rouge plane. L'homogénéité 
  est à rejeter si celle-ci n'est pas horizontale.

- De plus, ce graphique est l'occasion de vérifier si 
  certains points se regroupent, ce qui indiquerait un 
  défaut d'indépendance.
"


# Visualisation de l'Homoscédasticité des Résidus .........
plot(model_1, which = 3, pch = 20)


# Vérification par le test de Breush-Pagan .................
# (homogénéité : p-value > 0,05)

" ❤️ Principe du test 

Le test de  Breush-Pagan permet de vérifier objectivement 
l'homogénéité des résidus (et donc l'hétéroscédasticité 
vs homoscédasticité).

👉 L'homogénéité est rejeté si la p-value est inférieure à 0,05.

"

# Test de  Breush-Pagan
library("lmtest")
 
bptest(model_1)





# 👉 Hypothèses Indépendance des Résidus -----------------
#    Non d’Auto-Corrélation

# visualiser l'indépendance avec ce graphique

" ❤️ Critére basé sur un  Graphique 

- L'interprétation de ce graphique se fait de 
  la manière suivante :

- Le premier bâtonnet est très élevé, c'est 
  l'auto-corrélation des résidus avec eux-même !

- Le deuxième bâtonnet indique l'auto-corrélation 
  entre les résidus et les résidus n+1 : 
  il y a auto-corrélation dès que le bâtonnet (lag) 
  dépasse les pointillés.

- Le troisième bâtonnet entre les résidus n et les 
  résidus n+2... etc.


"

# Graphique ACF ...........................................
acf(residuals(model_1), 
    main = "ACF : Auto-corrélation des Résidus")

" 👉 Commentaire : 

- La majorité des coefficients de corrélation sont 
  statistiquement significatifs (dépassent les lignes7
  horizontales en bleue). 

"

# Vérification par le test de Durbin-Watson ...............
# (indépendance : p-value > 0,05)

"
❤️ Principe du test 

   Avec le test de Durbin-Watson : il faut une 
   p-value supérieure à 0,05 pour avoir indépendance.
"
# Test indépendance.
dwtest(model_1) # nécessite la librairie lmtest



# 🚀  Interprétation des Résultas du Modele Lineaire Initial --------------
#     Avec la Librarie 'gtsummary'

# Affichage du Modele
library(gtsummary)

model_1 |> tbl_regression(
  intercept = TRUE, 
  label = list(localite = "Régions", trimestre = "Trimestres")) |> 
  bold_labels()


" ❤️ Commentaire : 

- Donc on peut dire qu'en moyenne 
  une Région comme KOLDA présente
  1 357 cas de paludismes de plus 
  par rapport autres Régions 
  (statistique significatif)


- Donc on peut dire qu'en moyenne 
  au 3e Trimestre on a diminution des 
  cas de paludismes de 810 par rapport
  au 4e Trimestre (statistique 
  significatif)
  
- Pour une Année donnée, quand celle ci
  augment d'une unité, alors le Nbre de 
  cas de Paludisme diminue de 235 
  (statistique significatif)

"






# Chargement des données
cardio_vascular <- read.csv2("data/cardio_vascular_diseases.csv", 
                             header = TRUE,   
                             sep = ';',       
                             dec = '.')  

head(cardio_vascular)   
names(cardio_vascular)
str(cardio_vascular)    


# Prétraitement des données --------------------

# factorisation des varibles catégorielles 
cardio_vascular$gender <- factor(cardio_vascular$gender, 
                                     labels = c('Female', 'Male'))


cardio_vascular$cholesterol <- factor(cardio_vascular$cholesterol, 
                                          labels = c('Normal', 'Above Normal', 
                                                     'Well Above Normal'))


cardio_vascular$gluc <- factor(cardio_vascular$gluc, 
                                   labels = c('Normal', 'Above Normal', 
                                              'Well Above Normal'))

cardio_vascular$smoke <- factor(cardio_vascular$smoke, 
                                    labels = c('No', 'Yes'))

cardio_vascular$alco <- factor(cardio_vascular$alco, 
                                   labels = c('No', 'Yes'))

cardio_vascular$active <- factor(cardio_vascular$active, 
                                     labels = c('No', 'Yes'))

cardio_vascular$cardio <- factor(cardio_vascular$cardio, 
                                     labels = c('No', 'Yes'))


# Convertir la variable âge en nbr d'année (arrondi à l'unité)
cardio_vascular$age <- round(cardio_vascular$age / 365) 


# Affichage de la structure des variables
str(cardio_vascular_tbl)


# Sauvegarde des Données Labellisées 

saveRDS(object = cardio_vascular_tbl, 
        file = "data/cardio_vascular_labelled.rds")


cardio_vascular_labelled_rds <- readRDS("data/cardio_vascular_labelled.rds")


write.csv2(x = cardio_vascular_tbl, 
           file = "data/cardio_vascular_labelled.csv", 
           row.names = F)  


cardio_vascular_labelled <- read.csv2("data/cardio_vascular_labelled.csv", 
                                      header = TRUE,   # Première ligne comme en tête
                                      sep = ';',       # Séparateur des colonnes
                                      dec = '.')       # Séparateur des décimale

str(cardio_vascular_labelled)




# MODULE 4 : ANALYSE STATISTIQUE DES DONNÉES -------------------------------



# * Analyse Univariée -----------------------------------------------

# Moyenne (Indice de position)
mean(cardio_vascular_tbl$age)

# Médiane (Indice de position)
median(cardio_vascular_tbl$age)

# Ecart type (Indice de dispersion)
sd(cardio_vascular_tbl$age)

# Les quantiles (Indice de position)
# avec min et max aux extrêmes
quantile(cardio_vascular_tbl$age)


"Interprétation des valeurs : Q1, Q2 et Q3

Le premier quartile Q1 est la plus petite valeur 
de la série telle qu'au moins 25% des valeurs sont 
inférieures ou égales à Q1.

Ainsi, les données montrent qu'au moins 25% des 
individus sont agés d'au plus 49 ans.

Le troisième quartile Q3 est la plus petite valeur 
de la série telle qu'au moins 75% des valeurs sont 
inférieures ou égales à Q3.

Ainsi, les données montrent qu'au moins 75% des 
individus sont agés d'au plus 58 ans.


Q2 = Me (Médiane)

Ainsi, les données montrent qu'au moins 50% des 
individus sont agés d'au plus (ou d'au moins) 54 ans.

"

# Etude de Répartition (sur les Var. Quali.) .......


# Répartition de la maladie cardio chez les individus (effectifs)
table(cardio_vascular_tbl$cardio)

# Répartition de la maladie cardio en % chez les individus (fréquences)
tab <- table(cardio_vascular_tbl$cardio)
percent <- prop.table(tab) * 100
# Affichage des %
percent


# Nombre de Fumeurs dans la base de données
table(cardio_vascular_tbl$smoke == "Yes")

# Nombre d'individus atteints de maladie cardio
table(cardio_vascular_tbl$cardio == "Yes")


# La statistiques Descriptive globale
summary(cardio_vascular_tbl[, -1]) 

# Tableau de Rapport Statistique avec la librairie 'gtsummary' ..............

# installation du package 'gtsummary'
# install.packages("gtsummary", dependencies = T)
# Chargement du package 'gtsummary'
library(gtsummary)

# Help 
?gtsummary

# Help 
?tbl_summary



# Recueil du Nom l'ensemble des Variables
variable_names <- names(cardio_vascular_tbl[, -1]) # Sans les identifiants  

# Personnalisation du tableau
# Affichage du thème en français
theme_gtsummary_language(language = "fr", 
                         decimal.mark = ",", 
                         big.mark = " ")

# Tableau 'gtsummary' 
tab <- tbl_summary(
  data = cardio_vascular_tbl,  # les données
  include = variable_names,  # nom des variables
  statistic = list(all_continuous() ~ "{mean} ({sd})"), # Choix des Stats à afficher
  type = all_dichotomous() ~ "categorical", # affiche toutes les variables qualitatives
  digits = list(~ 1)) # Nbre de chiffres après la virgule

# Affichage et Modification de l'entête 
modify_header(tab, label ~ "**Variables**")








# * Analyse Bivariée (Etude de Dépendance) ----------------------------


# 1. Cas de deux varibles continues (Etude de Corrélation) ..........
# Association entre le poids 'weight'  et la taille 'height'
cor(cardio_vascular_tbl$height, cardio_vascular_tbl$weight)

# Test classique de nullité d’un coefficient de corrélation de 'Pearson'
# sous condition que l'une des variables ~ une loi normale
cor.test(cardio_vascular_tbl$height, cardio_vascular_tbl$weight)


# 2. Cas de deux varibles qualitatives (Etude de dépendance) ..........

# Dépendance entre 'cholesterol' et 'cardio' ....................
tableau <- table(cardio_vascular_tbl$cardio,
                 cardio_vascular_tbl$cholesterol,
                 deparse.level = 2)  # Affichage des labelles 


# Tableau des % colonne (tableau direct)
tableau_prop <- round(prop.table(tableau, margin = 2) * 100, 
                      digits = 1) # digits = 1 : Nbr de chiffre après la virgule

# Test de comparaison de deux pourcentages 'Test de chi-2'
# Test de chi-2'
chisq.test(tableau, correct = FALSE)




# Reprise des mêmes Tableaux avec la librairie 'questionr' .............
# qui est plus complète
library(questionr)

# Dépendance entre 'cholesterol' et 'cardio' ....................
# Tableau initial 
tableau <- table(cardio_vascular_tbl$cardio,
                 cardio_vascular_tbl$cholesterol,
                 deparse.level = 2)


#***** Tableau marginal avec % colonne (tableau direct)
tab_prop_col <- cprop(tableau)  # fonction de la librairie 'questionr'
# Affichage
tab_prop_col


#***** # Tableau marginal avec % ligne (tableau indirect)
tab_prop_row <- rprop(tableau)  # fonction de la librairie 'questionr'
# Affichage
tab_prop_row


# Test de chi-2'
chisq.test(tableau, correct = FALSE)




"
------------- Interprétation des tableaux ------------------

------------------ Résultat ----------------

                            cardio_vascular_tbl$cholesterol
cardio_vascular_tbl$cardio  Normal Above Normal Well Above Normal Ensemble
                     No     55.1   41.3         23.4              49.6   
                     Yes    44.9   58.7         76.6              50.4   
                     Total  100.0  100.0        100.0             100.0  


Les données montrent que seulement 45 % des personnes ayant 
un Niveau normal de cholestérol sont atteintes de maladie 
cardiovasculaire contre 59 % et 77 % pour ceux qui sont au 
dessus et fortement au dessus de la normale respectivement.
De plus, les données montrent que l'association entre ces 
deux variables est statistiquement significative. 
Donc, on peut affirmer avec une haute certitude que la 
maladie cardiovasculaire est liée au Niveau de cholestérol.

(D'après une lecture du tableau direct)





------------------ Résultat ----------------

                            cardio_vascular_tbl$cholesterol
cardio_vascular_tbl$cardio  Normal Above Normal Well Above Normal Total
                  No        83.1   11.6          5.3             100.0
                  Yes       66.6   16.3         17.1             100.0
                  Ensemble  74.8   14.0         11.2             100.0


Entre autres, les personnes ayant un Niveau trop élevé 
de cholestérol représentent 11 % des individus, 
mais seulement 5 % de ces individus ne sont atteints 
de maladie cardiovasculaire contre 17 % d'atteints.

Pour ce qui est des personnes ayant un Niveau normal
de cholestérol, ils représentent 75 % des individus,
mais seulement 67 % de ces individus sont atteints de 
maladie cardiovasculaire contre 83 % de non atteints.


(D'après une lecture du tableau indirect)

"







# 3. Cas de variables quantitatives et qualitatives ..............


# Comparaison des moyennes (Distribution de la taille selon le sexe)
# Ordre des variables : var. quantitative ensuite var. qualitative
by(cardio_vascular_tbl$height, 
   cardio_vascular_tbl$gender, 
   mean, # statistique de comparaison
   na.rm = T) # Pour calculer sans les valeurs manquantes 

# Test de comparaison des moyennes (t-test de 'Student')
# Attention !!! fonction tilde '~' à mettre entre les deux variables
t.test(cardio_vascular_tbl$height ~ cardio_vascular_tbl$gender, var.equal = T)


"........ Interprétation ..............


------------ Résultat  --------------

Two Sample t-test

data:  cardio_vascular_tbl$height by cardio_vascular_tbl$gender
t = -56.9, df = 9998, p-value < 2.2e-16
alternative hypothesis: true difference in means between 
group Female and group Male is not equal to 0
95 percent confidence interval:
 -8.860343 -8.270201
sample estimates:
mean in group Female   mean in group Male 
            161.3705             169.9358
            

-------- Commentaire --------


Les données montrent une association statistiquemnt 
significative entre la taille des individus et leur
sexe.
En effet, les données montrent qu'en moyenne les
hommes sont plus grands en taille que les femmes
(170 contre 161).

"






# * Visualisation de Données ----------------------------------------


"
La visualisation des données (ou data visualization ou 
dataviz en anglais) désigne le fait de représenter 
visuellement ses données pour pouvoir déceler et 
comprendre des informations, les données brutes étant 
difficilement interprétables et exploitables. 

"





# Etude des Distributions (Densités et Histogrammes) ......................



# Histogramme de Distribution des Ages avec la fonction 'hist()'

# Première affichage
hist(x = cardio_vascular_tbl$age)


"
----------- Liste de Modifications à faire -----------

Après une première affichage, nous allons apporter
plusieurs modifications :

- Changer l'échelle d'affichage (graduation) de 
  l'axe des Y avec l'argument 'ylim = c(0, 8000)' 
  pour une graduation de 0 à 8000.
  
- Changer la couleur de remplissage des barres avec
  l'argument 'col = 'cornfloWerblue' (couleur bleue)
  
- Chager la couleur de la circonférence des barres avec
  l'argument 'border = white' (couleur blanche)  

- Changer le titre du graph avec l'argument 
  main = 'titre Graph'
  
- Changer la légende de l'axe des X  avec l'argument 
  'xlab = axe des X'  

- Changer la légende de l'axe des Y  avec l'argument 
  'ylab = axe des Y'  

"


# Histogramme de 'Age' : Optimisé au Top
hist(x = cardio_vascular_tbl$age, 
     ylim = c(0, 8000),       # Les limites de l'axe verticale
     xlim = c(35, 70),
     col = 'cornfloWerblue',  # Couleur de remplissage
     border = 'white',        # Couleur de bordure 
     main = "Distribution des Âges", # Titre
     xlab = "Ages",                  # Titre axe des X
     ylab = "Fréquences")            # Titre axe des Y

# summary(cardio_vascular_tbl$age)




# Densité de Distribution des Ages avec la fonction 'plot(density())'

# Première affichage
plot(x = density(cardio_vascular_tbl$age, na.rm = T))
# Bandwidth = degré de lissage 



# Densité de distribution des Ages : Optimisé au Top
# Bandwidth = degré de lissage (bw = 1)
plot(x = density(cardio_vascular_tbl$age, na.rm = T, bw = 1), 
     col = 'orange', 
     main = "Densité de distribution des Âges", 
     xlab = "Âges",
     ylab = "Densité", 
     lwd = 2) # lwd = 2 : Pour gérer l'épaisseur de la courbe






# Histogramme + Densité sur un même Graphique

"
Pour combiner la fonction densité et l'histograme,
il est primordial que dabord l'histogramme ait une
distribution en densité de probabilté. 

Pour cela, nous faisons appel à l'argument 'probability = TRUE'
sans lequel, il sera impossible d'associer ces deux graphes.

En fin, la fonction 'lines()' va nous permettre de joindre
la Densité en même temps que l'histogramme.

"

# Histogramme + Densité 
hist(x = cardio_vascular_tbl$age, 
     probability = T)
lines(x = density(cardio_vascular_tbl$age, na.rm = T, bw = 1)) # ajout de courbe 



# Histogramme + Densité : Optimisé au Top
# Bandwidth = degré de lissage (bw = 1.2)
hist(x = cardio_vascular_tbl$age, 
     probability = T,
     ylim = c(0, 0.06), 
     col = 'cornfloWerblue',
     border = 'white',
     main = "Densité de distribution des Âges", 
     xlab = "Âges",
     ylab = "Densité")
lines(x = density(cardio_vascular_tbl$age, na.rm = T, bw = 1.2), 
      col = 'orange',
      lwd = 2)








# Comparaison des Densités de distribution du poids 'weight'
# selon la présence ou pas de la maladie cardiovasculaire
# Analyse Bivariée

plot(x = density(cardio_vascular_tbl$weight[cardio_vascular_tbl$cardio == "Yes"], 
                 na.rm = T, bw = 2),
     ylim = c(0, 0.04),
     col = 'brown', 
     main = "Densités de distribution du Poids selon la présence ou pas
     de la maladie cardiovasculaire",
     xlab = "Poids",
     ylab = "Densité", 
     lwd = 2)
lines(x = density(cardio_vascular_tbl$weight[cardio_vascular_tbl$cardio == "No"], 
                  na.rm = T, bw = 2), 
      col = 'orange',
      lwd = 2)
abline(v = 73, col = "red")       # Droite Verticale 
# Add Legend
legend(title = "Cardio Disease",  # titre de la légende
       x = "topright",            # Position
       cex = 0.6,                 # Dimension cadre légende
       legend = c("Yes", "No"),   # Labelles 
       fill = c("brown", "orange")) # Couleur des classes


"-------- Commentaire --------

Les données montrent qu'en dessous d'environ 73 kg, 
il y'a moins d'individus atteints de la maladie cardio.
Et au dessus de 75 kg, les individus atteints de la 
maladie cardio sont plus nombreux.

"







# Nuages de points ..............................................

"
Ils permettent d'étudier la relation (corrélation) linéaire
entre deux variables continues.
"


# Représenter la Taille en fonction du Poids (Y = f(X))
plot(x = cardio_vascular_tbl$weight, 
     y = cardio_vascular_tbl$height)

# Autre méthode de faire : plot(y~x)
plot(cardio_vascular_tbl$height ~ cardio_vascular_tbl$weight)




# Nuages de points : Optimisé au Top

"
- Nous allons utiliser la fonction 'jitter' pour 
  décaler les points et éviter leur superposition.

- pch = 20 : Pour changer la forme des points

- la fonction 'abline()' combinée avec 'plot()' pour 
  tracer la droite de régression.
"

# Représenter la Taille en fonction du Poids
plot(x = jitter(cardio_vascular_tbl$weight),   # jitter : Améliorer le rendu
     y = jitter(cardio_vascular_tbl$height),
     pch = 20, # Forme des points
     main = "Variation de la Taille en fonction de Poids",
     sub = "Source : Propre à l'auteur",
     xlab = "Poids",
     ylab = "Taille")
abline(lm(cardio_vascular_tbl$height ~ cardio_vascular_tbl$weight), # droite de régression
       col = "red", 
       lwd = 2) # epaissuer







# Visualisation des Variables qualitatives cholesterol ................................


# Analyse Univariée
# Diagrammes en Secteur ou Diagramme circulaire ---
# 'pie plot'


# Tableau des fréquences du taux de 'cholesterol'
tab <- table(cardio_vascular_tbl$cholesterol)
# Affichage
tab


# Pie plot : Diagramme circulaire
# Première affichage
pie(tab, 
    main = "Répartition du Niveau de Cholestérol")



# Pie plot : Diagramme circulaire (Optimisé au Top)
pie(tab, 
    labels = c('Normal : 75%', 'Above Normal : 14%', 'Well Above Normal : 11%'),
    col = c("#009E73", "#CC79A7", "#D55E00"), 
    main = "Répartition du Niveau de Cholestérol")


# prettyR::describe(cardio_vascular_tbl$cholesterol)





# Analyse Bivariée
# Diagramme en barres (Y = f(X)) ---

# Tableau de Contingence (des % colonne direct)
# Tableau initial 
tableau <-  round(
  prop.table(
    table(cardio_vascular_tbl$cardio,
          cardio_vascular_tbl$cholesterol), 
    margin = 2) * 100, 
  digits = 2)
# Affichage
tableau


# Diagramme en barres de la répartition de 

barplot(tableau, legend.text = T) # barre emppilée 








# Digramme en barres : Optimisé au Top

"
- Nous allons utiliser l'argument 'beside = T' pour 
  modifier la position des barres en côte à côte 
  (empilées par défaut).

- l'argument 'legend.text = T', nous permet d'afficher
  la légende.

- l'argument 'col = ' pour choisir une variété de couleurs
"


# Propres choix de couleurs (couleurs par le code hexadécimal)
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

# Digramme en barres
barplot(tableau, 
        beside = T, 
        col = my_colors[c(1, 2)], # par défaut 3 et 5
        ylim = c(0, 80), 
        main = "Répartition de la maladie cardiovasculaire 
        selon le Niveau de cholesterol", 
        xlab = "Niveau de cholesterol", 
        ylab = "Proportion")
# Add Legend
legend(title = "Cardiovascular Disease",  # titre de la légende
       x = "top",                 # Position
       cex = 0.6,                 # Dimension cadre légende
       legend = c("No", "Yes"),   # Labelles 
       fill = my_colors[c(1, 2)]) # Couleur des classes







# Analyse de deux varaibles de classes différentes .................

"
Dans le cadre d'une analyse bivariée, lorsque l'on a 
deux varaibles de classes différentes nous pouvons 
utiliser un 'BOXPLOT' ou diagramme en boîtes (boîtes à
moustaches). 

Ainsi, ce diagramme nous permettra de comparer la distribution
de la variable continue par rapport à la variable qualitative.

"


# Comparaison des moyennes (le poids selon le sexe : y = f(x))
by(cardio_vascular_tbl$weight,
   cardio_vascular_tbl$gender,
   median,
   na.rm = T)






# Diagramme en Boîtes 
# Attention !!! fonction tilde '~' à mettre entre les deux variables
boxplot(cardio_vascular_tbl$weight ~ cardio_vascular_tbl$gender)



# Diagramme en Boîtes : Optimisé au Top
# Avec plusieurs choix de couleurs
boxplot(cardio_vascular_tbl$weight ~ cardio_vascular_tbl$gender, 
        col = my_colors[c(4, 2)], 
        main = "Distribution du Poids selon le sexe", 
        xlab = "Sexe", 
        ylab = "Poids")







# * Tableau de Rapport avec la librairie 'gtsummary' ------------------------------

# Analyse Bivariée ...........

# Chargement des packages
library(gtsummary) # tableau reporting 
library(dplyr)    # package dédié à la data science (manipulation)




# Help 
??gtsummary
??tbl_summary



# Recueil du Nom l'ensemble des Variables
variable_names <- names(cardio_vascular_tbl[, -1]) # Sans les identifiants  

# Personnalisation du tableau
# Affichage du thème en français
theme_gtsummary_language(language = "fr", 
                         decimal.mark = ",", 
                         big.mark = " ")

# Ajoutons des etiquettes aux variables colonnes
# Avec la librairie 'labelled'
# Chargement de la librairie 'labelled'
library(labelled)

# Labellisation des variables colonnes 
var_label(cardio_vascular_tbl$age) <- "Age"
var_label(cardio_vascular_tbl$gender) <- "Gender"
var_label(cardio_vascular_tbl$height) <- "Height"
var_label(cardio_vascular_tbl$weight) <- "Weight"
var_label(cardio_vascular_tbl$ap_hi) <- "Systolic Blood Pressure"
var_label(cardio_vascular_tbl$ap_lo) <- "Diastolic Blood Pressure"
var_label(cardio_vascular_tbl$cholesterol) <- "Cholesterol"
var_label(cardio_vascular_tbl$gluc) <- "Glucose"
var_label(cardio_vascular_tbl$smoke) <- "Smoking"
var_label(cardio_vascular_tbl$alco) <- "Alcohol Intake"
var_label(cardio_vascular_tbl$active) <- "Physical Activity"
var_label(cardio_vascular_tbl$cardio) <- "Presence or Absence of Cardiovascular Disease"






# Table de l'analyse bivariée des variables avec 'gtsummary' ..................
# *1 : Choix de la variable dépendante ou cible "cardio"
# *2 : Tableau de pourcentage ligne
# *3 : Modification de la statistique en 'moyenne(écart-type)'
# *4 : Affichage des Totaux
# *5 : Affichage de la p-value
# *6 : Modification du non de l'entête
# *7 : Modification du non de l'entête
# %>% : fonction raccordement de la librairie 'dplyr'

library(gtsummary)

tab <- tbl_summary(
  data = cardio_vascular_tbl,  # les données
  include = variable_names,    # [6:11] nom des variables
  by = cardio,   # *1
  percent = "row", # *2
  statistic = list(all_continuous()~"{mean} ({sd})"), # *3
  type = all_dichotomous() ~ "categorical", # affiche toutes les variables qualitatives
  digits = list(~ 1) # Nbre de chiffres après la virgule
) %>%  
  # Fonctions Complémentaires
  add_overall(last = T, col_label = "**Total**, N = {N}") %>% # *4
  add_p() %>%  # *5
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Presence or Absence of Cardiovascular Disease**") %>% # *6
  modify_header(label ~ "**Variables**") # *7

# Affichage
tab

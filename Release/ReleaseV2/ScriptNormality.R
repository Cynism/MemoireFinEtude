setwd("chemin du dossier de téléchargement")

# installation des packages R
'install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggplot2")
install.packages("UsingR")'

# Initialisation des packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(UsingR)


# Import des données .csv situées dans le Working Directory
data1205LRdG <- read.table("Relevés12-05-2021_LRdG.csv", 
                           header=TRUE, sep=";")
data0306LRdG <- read.table("Relevés03-06-2021_LRdG.csv", 
                           header=TRUE, sep=";")
data2306Manu <- read.table("Relevés23-06-2021_Manu.csv", 
                           header=TRUE, sep=";")
dataPartenaire <- read.table("RelevesEvalPartenaire.csv", 
                           header=TRUE, sep=";")

# 12/05 ----

## Septo

# Incrémentation dans a d'un diagramme de densité
a <- ggdensity(data1205LRdG$SeptoF3,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
# Retour du diagramme de répartition des points et courbe de tendande
ggqqplot(data1205LRdG$SeptoF3)

b <- ggdensity(data1205LRdG$SeptoF2,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data1205LRdG$SeptoF2)

c <- ggdensity(data1205LRdG$SeptoF1,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data1205LRdG$SeptoF1)


## Rouille jaune
d <- ggdensity(data1205LRdG$RouilF3,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data1205LRdG$RouilF3)


e <- ggdensity(data1205LRdG$RouilF2,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data1205LRdG$RouilF2)


f <- ggdensity(data1205LRdG$RouilF1,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data1205LRdG$RouilF1)

# Création de la grille de résultats
fig <- ggarrange(a, b, c, d, e, f,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, 
          nrow = 2)
annotate_figure(fig,
                top=text_grob("Diagrammes de densité des résultats: % de surface infestée à la date du 12/05 - LRdG", 
                              color = "red", 
                              face = "bold", 
                              size = 12))


# 03/06 ----

## Septo
a <- ggdensity(data0306LRdG$SeptoF3,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$SeptoF3)

b <- ggdensity(data0306LRdG$SeptoF2,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$SeptoF2)

c <- ggdensity(data0306LRdG$SeptoF1,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$SeptoF1)


## Rouille jaune
d <- ggdensity(data0306LRdG$RouilF3,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$RouilF3)


e <- ggdensity(data0306LRdG$RouilF2,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$RouilF2)


f <- ggdensity(data0306LRdG$RouilF1,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data0306LRdG$RouilF1)

fig <- ggarrange(a, b, c, d, e, f,
                 labels = c("A", "B", "C", "D", "E", "F"),
                 ncol = 3, 
                 nrow = 2)
annotate_figure(fig,
                top=text_grob("Diagrammes de densité des résultats: % de surface infestée à la date du 03/06 - LRdG", 
                              color = "red", 
                              face = "bold", 
                              size = 12))

# 23/06 ----

## Septo
a <- ggdensity(data2306Manu$SeptoF3,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$SeptoF3)

b <- ggdensity(data2306Manu$SeptoF2,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$SeptoF2)

c <- ggdensity(data2306Manu$SeptoF1,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$SeptoF1)


## Rouille jaune
d <- ggdensity(data2306Manu$RouilF3,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F3")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$RouilF3)


e <- ggdensity(data2306Manu$RouilF2,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F2")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$RouilF2)


f <- ggdensity(data2306Manu$RouilF1,fill = "lightgray", 
               xlab="% d'infestation rouille jaune sur F1")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(data2306Manu$RouilF1)

fig <- ggarrange(a, b, c, d, e, f,
                 labels = c("A", "B", "C", "D", "E", "F"),
                 ncol = 3, 
                 nrow = 2)
annotate_figure(fig,
                top=text_grob("Diagrammes de densité des résultats: % de surface infestée à la date du 23/06 - LRdG", 
                              color = "red", 
                              face = "bold", 
                              size = 12))

# Essai Partenaire ----

a <- ggdensity(dataPartenaire$Surface_Percent_Septo_F3_T20j,fill = "lightgray", 
xlab="% d'infestation septoriose sur F3 (témoin)")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(dataPartenaire$Surface_Percent_Septo_F3_T20j)


b <- ggdensity(dataPartenaire$Surface_Percent_Septo_F2_T32j,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F2 (tout)")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(dataPartenaire$Surface_Percent_Septo_F2_T32j)


c <- ggdensity(dataPartenaire$Surface_Percent_Septo_F1_T40j,fill = "lightgray", 
               xlab="% d'infestation septoriose sur F1 (tout)")+
  theme(axis.title.x = element_text(size = rel(.75)))
ggqqplot(dataPartenaire$Surface_Percent_Septo_F1_T40j)

fig <- ggarrange(a, b, c,
                 labels = c("14/06", "28/06", "06/07"),
                 ncol = 3, 
                 nrow = 1)
annotate_figure(fig,
                top=text_grob("Diagrammes de densité des résultats: % de surface infestée sur l'essai partenaire", 
                              color = "red", 
                              face = "bold", 
                              size = 12))


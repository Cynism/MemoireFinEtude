setwd("chemin du dossier de téléchargement")

# installation des packages R
'install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggplot2")
install.packages("reshape2")'

# Démarrage -----

# Initialisation des packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(forcats)
library(reshape2)

# Import des données .csv situées dans le Working Directory
data1205LRdG <- read.table("Relevés12-05-2021_LRdG.csv", header=TRUE, sep=";")
# Réarrangement de l'ordre des répétition. De base elles n'étaient pas dans l'ordre.
data1205LRdG$ï..Repet = fct_relevel(data1205LRdG$ï..Repet, c( 
  "Temoin ZA", "Temoin ZB", "80UN ZA",
  "80UN ZB", "240UN ZA", "240UN ZB"))

data0306LRdG <- read.table("Relevés03-06-2021_LRdG.csv", header=TRUE, sep=";")
data0306LRdG$ï..Repet = fct_relevel(data0306LRdG$ï..Repet, c( 
  "Temoin ZA", "Temoin ZB", "80UN ZA",
  "80UN ZB", "240UN ZA", "240UN ZB"))

data2306Manu <- read.table("Relevés23-06-2021_Manu.csv", header=TRUE, sep=";")

dataPartenaire <- read.table("RelevesEvalPartenaire.csv", header=TRUE, sep=";")
dataPartenaire$N.Produit_Dose = fct_relevel(dataPartenaire$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))
  
dataPartenaireT32 <- read.table("EssaiFongiGivryT32jours.csv", header=TRUE, sep=";")
# Melting des données = A chaque modalité (12 lignes) on étudie 25 feuilles (25 colonnes). Le melting
# permet donc de synthétiser les 25 colonnes en 1 seule : "value" et devant chaque individu se retrouve
# les variables associées (ici : les colonnes 1 à 3), et le reste des colonnes (4 à 28) forme les lignes.
# Cela permet de traiter l'ensembke des données avec 1 seule colonne au lieu de le faire colonne par
# colonne alors qu'il s'agit des mêmes variables.
dataPartenaireT32 <- melt(dataPartenaireT32,id=1:3)
dataPartenaireT32$N.Produit_Dose = fct_relevel(dataPartenaireT32$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))

dataPartenaireT40 <- read.table("EssaiFongiGivryT40jours.csv", header=TRUE, sep=";")
dataPartenaireT40 <- melt(dataPartenaireT40,id=1:3)
dataPartenaireT40$N.Produit_Dose = fct_relevel(dataPartenaireT40$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))


# Essai du 12/05 ----

## Septoriose

# Création de diagramme en violon des données de contamination
ggplot(data1205LRdG,aes(x = factor(ï..Repet),y = SeptoF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F3, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data1205LRdG, aes(x = factor(ï..Repet), y = SeptoF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F2, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data1205LRdG, aes(x = factor(ï..Repet), y = SeptoF1 + 1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F1, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

## Rouille jaune
ggplot(data1205LRdG,aes(x = factor(ï..Repet),y = RouilF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F3, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))


ggplot(data1205LRdG, aes(x = factor(ï..Repet), y = RouilF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F2, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data1205LRdG, aes(x = factor(ï..Repet), y = RouilF1 + 1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F1, daté du 12/05")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))


# Essai du 03/06 ------------------------

## Septoriose
ggplot(data0306LRdG,aes(x = factor(ï..Repet),y = SeptoF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F3, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data0306LRdG, aes(x = factor(ï..Repet), y = SeptoF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F2, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data0306LRdG, aes(x = factor(ï..Repet), y = SeptoF1 + 1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F1, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

## Rouille jaune
ggplot(data0306LRdG,aes(x = factor(ï..Repet),y = RouilF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F3, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data0306LRdG, aes(x = factor(ï..Repet), y = RouilF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F2, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data0306LRdG, aes(x = factor(ï..Repet), y = RouilF1 + 1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F1, daté du 03/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

# Essai du 23/06 ----

## Septoriose
ggplot(data2306Manu,aes(x = factor(ï..Repet),y = SeptoF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F3, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data2306Manu,aes(x = factor(ï..Repet),y = SeptoF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F2, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data2306Manu,aes(x = factor(ï..Repet),y = SeptoF1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F1, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

## Rouille jaune
ggplot(data2306Manu,aes(x = factor(ï..Repet),y = RouilF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F3, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data2306Manu,aes(x = factor(ï..Repet),y = RouilF2)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F2, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(data2306Manu,aes(x = factor(ï..Repet),y = RouilF1)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par la rouille jaune sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par la rouille jaune sur la F1, daté du 23/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

# Essai du partenaire ----

ggplot(dataPartenaire,aes(x = factor(N.Produit_Dose),y = Surface_Percent_Septo_F3_T20j)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F3, daté du 14/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(dataPartenaire,aes(x = factor(N.Produit_Dose),y = Surface_Percent_Septo_F2_T32j)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F2, daté du 28/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75),scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.1, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

ggplot(dataPartenaire,aes(x = factor(N.Produit_Dose),y = Surface_Percent_Septo_F1_T40j)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F1, daté du 06/07")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

# Essai du partenaire T32 ----

ggplot(dataPartenaireT32,aes(x = factor(N.Produit_Dose),y = value)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F2, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F2, daté du 28/06")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

# Essai du partenaire T40 ----

ggplot(dataPartenaireT40,aes(x = factor(N.Produit_Dose),y = value)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F1, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F1, daté du 06/07")+
  geom_violin(draw_quantiles = c(.25, .5, .75), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2, fill="grey")+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))


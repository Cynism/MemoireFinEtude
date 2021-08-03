setwd("chemin du dossier de téléchargement")

# installation des packages R
'install.packages("vegan")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggplot2")
install.packages("UsingR")
install.packages("reshape2")'

# Initialisation des packages
library(vegan)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(UsingR)
library(forcats)
library(reshape2)

par(mfrow=c(1,1))

# Import des données .csv situées dans le Working Directory
data1205LRdG_TUN <- read.table("Relevés12-05-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data1205LRdG_80UN <- read.table("Relevés12-05-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data1205LRdG_240UN <- read.table("Relevés12-05-2021_LRdG_240UN.csv", header=TRUE, sep=";")
data0306LRdG_TUN <- read.table("Relevés03-06-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data0306LRdG_80UN <- read.table("Relevés03-06-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data0306LRdG_240UN <- read.table("Relevés03-06-2021_LRdG_240UN.csv", header=TRUE, sep=";")
data2306Manu <- read.table("Relevés23-06-2021_Manu.csv", header=TRUE, sep=";")

dataPartenaireT32 <- read.table("EssaiPartenaireT32jours.csv", header=TRUE, sep=";")
# Réarrangement de l'ordre des répétition. De base elles n'étaient pas dans l'ordre.
dataPartenaireT32$N.Produit_Dose = fct_relevel(dataPartenaireT32$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))
# Melting des données = A chaque modalité (12 lignes) on étudie 25 feuilles (25 colonnes). Le melting
# permet donc de synthétiser les 25 colonnes en 1 seule : "value" et devant chaque individu se retrouve
# les variables associées (ici : les colonnes 1 à 3), et le reste des colonnes (4 à 28) forme les lignes.
# Cela permet de traiter l'ensembke des données avec 1 seule colonne au lieu de le faire colonne par
# colonne alors qu'il s'agit des mêmes variables.
dataPartenaireT32 <- melt(dataPartenaireT32,id=1:3)

dataPartenaireT40 <- read.table("EssaiPartenaireT40jours.csv", header=TRUE, sep=";")
dataPartenaireT40$N.Produit_Dose = fct_relevel(dataPartenaireT40$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))
dataPartenaireT40 <- melt(dataPartenaireT40,id=1:3)

dataPartenaireTot <- read.table("EssaiPartenaireT32T40.csv", header=TRUE, sep=";")
dataPartenaireTot$N.Produit_Dose = fct_relevel(dataPartenaireTot$N.Produit_Dose, c( 
  "1-Temoin", "2-Keynote_08", "3_RevystarXL_04", 
  "4_RevystarXL_08", "5_RevystarXL_12", "6_CodeA_066",
  "7_CodeA_1325", "8_CodeA_20", "9_CodeB_10",
  "10-RevyXB_06", "11-Atrium+Pavecto60_05+12", "12-Imtrex+Sulky_08+08"))

dataPartenaireTotT32 <- melt(dataPartenaireTot,id=1:3,measure= 4)
dataPartenaireTotT40 <- melt(dataPartenaireTot,id=1:3,measure= 5)

# LRdG 12/05 ----

# Analyse de similitudes des distributions d'une même modalité
com1205_TUN = data1205LRdG_TUN[,2:ncol(data1205LRdG_TUN)]
m_com1205_TUN = as.matrix(com1205_TUN)
sim1205_TUN = anosim(m_com1205_TUN, data1205LRdG_TUN$ï..Repet, distance = "euclidean", permutations = 9999);sim1205_TUN
plot(sim1205_TUN)

com1205_80UN = data1205LRdG_80UN[,2:ncol(data1205LRdG_80UN)]
m_com1205_80UN = as.matrix(com1205_80UN)
sim1205_80UN = anosim(m_com1205_80UN, data1205LRdG_80UN$ï..Repet, distance = "euclidean", permutations = 9999);sim1205_80UN
plot(sim1205_80UN)

com1205_240UN = data1205LRdG_240UN[,2:ncol(data1205LRdG_240UN)]
m_com1205_240UN = as.matrix(com1205_240UN)
sim1205_240UN = anosim(m_com1205_240UN, data1205LRdG_240UN$ï..Repet, distance = "euclidean", permutations = 9999);sim1205_240UN
plot(sim1205_240UN)


# LRdG 03/06 ----
com0306_TUN = data0306LRdG_TUN[,2:ncol(data0306LRdG_TUN)]
m_com0306_TUN = as.matrix(com0306_TUN)
sim0306_TUN = anosim(m_com0306_TUN, data0306LRdG_TUN$ï..Repet, distance = "euclidean", permutations = 9999);sim0306_TUN
plot(sim0306_TUN)

com0306_80UN = data0306LRdG_80UN[,2:ncol(data0306LRdG_80UN)]
m_com0306_80UN = as.matrix(com0306_80UN)
sim0306_80UN = anosim(m_com0306_80UN, data0306LRdG_80UN$ï..Repet, distance = "euclidean", permutations = 9999);sim0306_80UN
plot(sim0306_80UN)

com0306_240UN = data0306LRdG_240UN[,2:ncol(data0306LRdG_240UN)]
m_com0306_240UN = as.matrix(com0306_240UN)
sim0306_240UN = anosim(m_com0306_240UN, data0306LRdG_240UN$ï..Repet, distance = "euclidean", permutations = 9999);sim0306_240UN
plot(sim0306_240UN)

# Manu 23/06 ----
com2306_Manu = data2306Manu[,2:ncol(data2306Manu)]
m_com2306_Manu = as.matrix(com2306_Manu)
sim2306_Manu = anosim(m_com2306_Manu, data2306Manu$ï..Repet, distance = "euclidean", permutations = 9999);sim2306_Manu
plot(sim2306_Manu)

# Partenaire T32 ----
# version sans les moyennes
comPartenaire_T32 = dataPartenaireT32[,5]
m_comPartenaire_T32 = as.matrix(comPartenaire_T32)
simPartenaire_T32 = anosim(m_comPartenaire_T32, dataPartenaireT32$N.Produit_Dose, distance = "euclidean", permutations = 999);simPartenaire_T32
plot(simPartenaire_T32)

# version avec les moyennes
comPartenaire_TotT32 = dataPartenaireTotT32[,5]
m_comPartenaire_TotT32 = as.matrix(comPartenaire_TotT32)
simPartenaire_TotT32 = anosim(m_comPartenaire_TotT32, dataPartenaireTotT32$N.Produit_Dose, distance = "euclidean", permutations = 9999);simPartenaire_TotT32
plot(simPartenaire_TotT32)

# Partenaire T40 ----
# version sans les moyennes
comPartenaire_T40 = dataPartenaireT40[,5]
m_comPartenaire_T40 = as.matrix(comPartenaire_T40)
simPartenaire_T40 = anosim(m_comPartenaire_T40, dataPartenaireT40$N.Produit_Dose, distance = "euclidean", permutations = 999);simPartenaire_T40
plot(simPartenaire_T40)

# version avec les moyennes
comPartenaire_TotT40 = dataPartenaireTotT40[,5]
m_comPartenaire_TotT40 = as.matrix(comPartenaire_TotT40)
simPartenaire_TotT40 = anosim(m_comPartenaire_TotT40, dataPartenaireTotT40$N.Produit_Dose, distance = "euclidean", permutations = 9999);simPartenaire_TotT40
plot(simPartenaire_TotT40)

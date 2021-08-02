setwd("chemin du dossier de téléchargement")

# installation des packages R
'install.packages("vegan")'

# Initialisation des packages
library(vegan)

par(mfrow=c(1,3))

# Import des données .csv situées dans le Working Directory
data1205LRdG_TUN <- read.table("Relevés12-05-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data1205LRdG_80UN <- read.table("Relevés12-05-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data1205LRdG_240UN <- read.table("Relevés12-05-2021_LRdG_240UN.csv", header=TRUE, sep=";")
data0306LRdG_TUN <- read.table("Relevés03-06-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data0306LRdG_80UN <- read.table("Relevés03-06-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data0306LRdG_240UN <- read.table("Relevés03-06-2021_LRdG_240UN.csv", header=TRUE, sep=";")
data2306Manu <- read.table("Relevés23-06-2021_Manu.csv", header=TRUE, sep=";")

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


setwd("chemin du dossier de téléchargement")

# installation des packages R
'install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggplot2")'

# Démarrage -----


# Initialisation des packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

# Import des données .csv situées dans le Working Directory
data1205LRdG <- read.table("Relevés12-05-2021_LRdG.csv", header=TRUE, sep=";")
data0306LRdG <- read.table("Relevés03-06-2021_LRdG.csv", header=TRUE, sep=";")
data2306Manu <- read.table("Relevés23-06-2021_Manu.csv", header=TRUE, sep=";")
data1205LRdG_TUN <- read.table("Relevés12-05-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data1205LRdG_80UN <- read.table("Relevés12-05-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data1205LRdG_240UN <- read.table("Relevés12-05-2021_LRdG_240UN.csv", header=TRUE, sep=";")
data0306LRdG_TUN <- read.table("Relevés03-06-2021_LRdG_TUN.csv", header=TRUE, sep=";")
data0306LRdG_80UN <- read.table("Relevés03-06-2021_LRdG_80UN.csv", header=TRUE, sep=";")
data0306LRdG_240UN <- read.table("Relevés03-06-2021_LRdG_240UN.csv", header=TRUE, sep=";")


# Essai du 12/05 ----

## Septoriose

# Création de diagramme en violon des données de contamination
ggplot(data1205LRdG,aes(x = factor(ï..Repet),y = SeptoF3)) +
  xlab(label = "Répétition de modalité") +
  ylab(label = "Contamination \n par septoriose sur F3, \n en % de feuille") +
  ggtitle("Diagrammes en violon de représentation du % de surface contaminée 
          par septoriose sur la F3, daté du 12/05")+
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
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
  geom_violin(draw_quantiles = c(.05,.25, .5, .75, .95), scale="width")+
  #geom_jitter(height = 0, width = 0.1, color = "red")+
  geom_boxplot(width=0.2)+
  #stat_summary(fun=mean, geom="point", shape=23, size=2)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  #scale_y_continuous(limits = c(0,21), expand = c(0,0))+
  theme(
    axis.text.x = element_text(angle=30, hjust=1, vjust=1),
    legend.position="none",
    plot.title = element_text(color="red", size=10, hjust=.5, face="italic"),
    axis.title.y = element_text(angle=0,hjust=.5,vjust=.5, size = 8, face="bold"),
    axis.title.x = element_text(size = 8,face="bold"))

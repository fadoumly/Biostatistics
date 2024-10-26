#Fadoum Ousmane LY
#UE Biostatistiques
#M1 ST2AE
#2023 - 2024
#TD1 

#Definir mon dossier de travail 
setwd("~/TD1 BIOSTATS")

#Importer mes données 
df <- read.csv("statistiques-de-controle-des-peches.csv") #permet d'importer les données
#sous forme de tableau et de renommer le tableau "df"
head(df) #vérifier les premières lignes 
df[1,3]  # Valeur se trouvant à la 1ère ligne et à la  3ème colonne 
df[1, ]  # Donne toutes les valeurs/élements se trouvant dans la 1ère ligne 
df[ ,2]  #Tous les éléments se trouvant dans la 2ème colonne 

#1.Explorer les données en calculant les statistiques résumées 
ncol(df) #Nombre de colonnes = Nombre de variables 
nrow(df) #Nombre de lignes
names(df) #Noms des des varibales 
table(df$facade) #Nombre de valeurs dans chaque groupe de la variable facade
table(df$control_year)

mean(df$infraction_rate) #Moyenne du taux d'infraction 
sd(df$infraction_rate) #Ecart-type du taux d'infraction
var(df$infraction_rate) #Variance du taux d'infraction

summary(df)

#2.Explorer les données en visualisant la distribution du taux d’infraction
hist(df$infraction_rate)
boxplot(df$infraction_rate, ylab="taux d'infraction") 
#Donne une représentation des quartiles 

#3.3. On soupçonne que le taux d'infraction est différent entre les régions Atlantique/Manche et Méditerranée
#Test de cette hypothèse en suivant les étapes suivantes 
#1ère étape: Comparaison graphique du taux d'infraction des deux goupes 
boxplot(infraction_rate ~ facade, data = df,
        ylab="Taux d'infraction", 
        xlab="Façade maritime", 
        col=c("lightblue", "orange"), 
        names=c("Meditéranée", "Atlantique/Manche"))
#2ème étape: Test de normalité des variables par shapiro; Ho:les données suivent une loi normale; Rejeter Ho si p-value<0,05
tapply(df$infraction_rate, df$facade, shapiro.test)
# p-value = 0.001101, on rejette H0, les données ne suivent pas une loi normale 

#2ème étape bis: Test d'égalité des variances; Ho: Les variances sont égales
var.test(infraction_rate ~ facade, data=df)
tapply(df$infraction_rate, df$facade, var.test)
#p-value = 0.0007396, les variances ne sont pas égales. 

#3ème étape: Test de wilcoxon pour comparer les médianes car une des des séries des données ne suit pas la loi normale
wilcox.test(infraction_rate ~ facade, data = df) # p-value = 0.0001276 < 0.05, les médianes sont différentes
tapply(df$infraction_rate, df$facade, median) # calculer les médianes MED=0.2090442    NAMO=0.1045916 

#4ème étape: Conclusion
#Le taux d'infraction médian observé lors de contrôles maritimes en Méditerranée (0.21) 
# est significativement supérieur (test de Mann-Whitney Wilcoxon, W = 114, p-val=e = 0.0001) 
# au taux d'infraction médian en Atlantique (0.10)


# alternative si variables normales: test t (comparaison des moyennes)
t.test(infraction_rate ~ facade, data = df)

# 4. Considérer la relation entre les taux d’infraction constatés dans les deux régions maritimes
# séparer les données
df_MED <- subset(df, df$facade == "MED") 
df_ATL <- subset(df, df$facade == "NAMO")

# Nuage de points
plot(df_MED$infraction_rate, df_ATL$infraction_rate,
     xlab = "Taux d'infraction en Méditerranée",
     ylab = "Taux d'infraction en Atlantique/Manche",
     pch = 16,
     col = "red4",
     cex = 1.2)

# tester la normalité des du taux d'infraction dans les deux groupes
# à noter qu'on l'avait déjà fait différemment avant !
shapiro.test(df_MED$infraction_rate)
shapiro.test(df_ATL$infraction_rate)

# test de corrélation de Spearman (car une série de données ne suit pas un loi Normale)
cor.test(df_MED$infraction_rate, df_ATL$infraction_rate, method = 'spearman')
# alternative si données Normales (test de corrélation de Pearson)
cor.test(df_MED$infraction_rate, df_ATL$infraction_rate, method = 'pearson')

# 4. Tests de corrélation entre le temps et le taux d'infraction
# Med
shapiro.test(df_MED$infraction_rate)
shapiro.test(df_MED$control_year)
cor.test(df_MED$infraction_rate, df_MED$control_year, method = "pearson")

# Atl
shapiro.test(df_ATL$infraction_rate)
shapiro.test(df_ATL$control_year)
cor.test(df_ATL$infraction_rate, df_ATL$control_year, method = "spearman")

# représentation graphique sous la forme d'un nuage de point
# pour aller plus loin on utilise la fonction ggplot du package ggplot2
library(ggplot2) # charger le package (à installer avec la fonction install.packages si non disponible)

ggplot(data = df, # objet contenant les données
       aes(y = infraction_rate, color = facade, x = control_year, group = facade)) + # variables à représenter
  geom_point(size = 3) + # type de représentation graphique (ici: points)
  geom_path() + # pour relier les points entre eux
  scale_x_continuous("Année", breaks = 2012:2023) + # personnaliser l'axe des abscisses 
  scale_y_continuous("Taux d'infraction") + # personnaliser l'axe des ordonnées
  scale_color_manual("Façade maritime", values = c("blue4", "orange3")) + #personnaliser la légende de couleurs
  theme_classic() + # changer le thème graphique
  theme(legend.position = c(.8,.8), # positionner la légende
        legend.background = element_rect(color = 'black')) # personnaliser le thème graphique de la légende 

# figures alternatives avec ggplot2
# histogrammes
ggplot(data = df, aes(x = infraction_rate)) + # variables à afficher
  facet_grid(~ facade) + # diviser le graphe en fonction d'une variable
  geom_histogram(bins = 10, color = "white") + # afficher un histogramme de distribution
  scale_y_continuous("Fréquence") +  # titre axe y
  scale_x_continuous("Taux d'infraction") + # titre axe x
  theme_bw() # thème visuel

# boxplots
ggplot(data = df, aes(y = infraction_rate, x = facade)) +
  geom_boxplot() + 
  scale_x_discrete("Façade maritime") + 
  scale_y_continuous("Taux d'infraction") +
  theme_classic()

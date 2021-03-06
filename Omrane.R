#installation des packages nécessaires
install.packages("ggcorrplot")



#Le chargement des librairies nécessaire

# Manipulation
library(dplyr)
library(tidyverse)
library(caTools)
library(lubridate)

# Visualization
library(ggplot2)
library(plotly)
library(ggthemes)
library(corrplot)
library(ggcorrplot)
library(ggstatsplot)
library(ggpubr)

# EDA REPORT
library(dlookr)
library(visdat)
library(DataExplorer)
library(SmartEDA)
library(flextable)
library(ISLR)


# Machine Learning
library(missRanger)
library(rpart)
library(randomForest)
library(Amelia)
library(psych)
library(caret)
library(Hmisc)




#Importation

options(scipen = 999, "digits"=3)
# Importer les donnée qui ont dans le fichier Omrane.csv qui est dans la même répertoire
data.org<- read.csv(file = "./Datasets/Omrane.csv", header = TRUE, sep="\t", dec=",")
#data$X <- NULL
# les premières observations des données
head(data, n=3)

# les dernières observations des données
tail(data)


print(paste("Nombre d'observations",nrow(data) , sep = " "))
print(paste("Nombre de colonnes:",ncol(data)  , sep = " "))

# La structure des attributs
str(data)

# Statiques de bases sur l'ensemble des attributs
summary(data)
describe(data)

# Les valeurs non-observés pour chaque attribut
missmap(data)



#Pre-Processing, correction et nettoyage des données

# deleting the columns that we don't need in our analysis
data.clean = select(data.org, c(-"N..Lot",-"TitreFoncier",-"CIN.RC",-"DateDeCréation", -"TotalAvances"))
colnames(data.clean)[16] <- "DésignationTypeDeDocument"
# generer une liste des indexes aléatoires
shuffle_index <- sample(1:nrow(data.clean))

# On va utiliser ses indexes pour mélanger les donnée
data.clean <- data.clean[shuffle_index, ]
#glimpse(data)


#fixing the column type
cols.num <- c("NombreUnité","StatutADV", "Superficie", "PrixUnitaire", "PrixTotal", "Vers1", "Vers2", "Vers3", "TotalVersement",
               "VersRegularis","TropPerçuRéglé", "TropPerçuNonRéglé", "ResteàPayer")
cols.char = c("NuméroClient")
cols.date = c("DateLivraison","DateContrat","DateDeCréation")
data.clean[cols.num] <- sapply(data.clean[cols.num],as.numeric)
data.clean[cols.char] <- sapply(data.clean[cols.char],as.character)
data.clean$Création <- ymd(data.clean$Création)
data.clean$DateContrat <- ymd(data.clean$DateContrat)
data.clean$DateLivraison <- ymd(data.clean$DateLivraison)


sapply(data.clean, class)



 
# Every Product should have a Unite Price
data.clean<-data.clean[!(data.clean$PrixUnitaire==0),]
# to purchase a product you need pay the Vers1
nrow(data.clean[data.clean$Vers1 == 0,])
data.clean = subset(data.clean, Vers1 != 0)
# Fixing Relation between the variables
data.clean$PrixTotal = data.clean$Superficie * data.clean$PrixUnitaire
data.clean$TotalVersement = data.clean$Vers1 + data.clean$Vers2 + data.clean$Vers3
data.clean$ResteàPayer = (data.clean$PrixTotal - data.clean$TotalVersement )
nrow(data.clean[data.clean$ResteàPayer < 0,])
data.clean<-data.clean[!(data.clean$ResteàPayer < 0),]


data.clean = select(data.clean, c(-"TropPerçuRéglé", -"TropPerçuNonRéglé",-"VersRegularis"))

#data.clean$Crédit = data.clean$TropPerçuNonRéglé + data.clean$TropPerçuRéglé + data.clean$ResteàPayer 
#dataprices = select(data,c("Superficie","PrixUnitaire","PrixTotal","TotalVersement","TropPerçuRéglé", "TropPerçuNonRéglé","ResteàPayer"))
#dataNegRes<-dataprices[(dataprices$ResteàPayer < 0),]



#L'imputation des valeurs manquantes

# Les valeurs non-observés pour chaque attribut
data.clean %>% plot_missing()
# since the two columns DateContrat and DateLivraison are 100% missing and the company can't provide a new dataset that fix the problem of the missing Feautures unless they are very important and less than 50% in the case we could use them in Analysis in our case the mentioned variables should be dropped
plot_na_pareto(data.clean, only_na = TRUE)
# ggplotly(plot_na_pareto(data,  only_na = TRUE))
# the problem with pareto chart is that we don't know whether the missing values in different columns belong to the same row of observations
# so visualize the missing values across columns
plot_na_intersect(data.clean)
#vis_miss(data) %>% ggplotly()

#to reduce bias we remove the two variables
data.clean = select(data.clean, c(-"DateLivraison",-"DateContrat"))

# to impute the numeric continueous missing values & outliers we're going to use machine learning algorithms :
# rpart - Recursive Partitioning and Regression Trees.
# mice - Multivariate Imputation by Chained Equations
# mean
# dlookr imputation
#set.seed(123)
#data_na <- generateNA(data) %>% mutate(Designation = factor(Designation))
#plot_na_intersect(data_na, only_na = TRUE)
#
#imputate_na(data_na, Création, StatutADV, method = "mean") %>% 
#  plot()
#imputate_na(data_na, Création, StatutADV, method = "rpart") %>% plot()
#imputate_na(data_na, Création, StatutADV, method = "mice", seed = 111) %>% plot()
#
#Miss Ranger Imputation
#data_imputed <- missRanger(data_na, formula = . ~ ., num.trees = 1000, seed = 3)



#Outliers

boxplot(data.clean$Superficie)
   boxplot(data.clean$PrixUnitaire)
data.clean<-data.clean[!(data.clean$PrixUnitaire==2600000.00),]
data.clean<-data.clean[!(data.clean$PrixUnitaire==170300.00),]
data.clean<-data.clean[!(data.clean$Superficie==21020.0),]

sum(data.clean$TotalVersement)
#sum(data.clean$TotalAvances)


#Univariate Analysis for numeric variables

densityVers2 = density(data$TotalVersement)
histVers2 = hist(data$TotalVersement, bw = 1000)
densityDivision = density(data$Division)
histDivision = hist(data$Division)

plot(histVers2)
plot(densityVers2)
plot(histDivision)
plot(densityDivision)
#Echantiollonage
#set.seed(2) #2 représente 20% de chaque groupe
#n = 4000 
#sample= data[sample(1:nrow(data),n), ]
# 7 -> 10

#A p-value less than 0.05 (typically ≤ 0.05) is statistically significant. ...
#A p-value higher than 0.05 (> 0.05) is not statistically significant and indicates strong evidence for the null
#If P(real) = 0.9, there is only a 10% chance that the null hypothesis is true at the outset.
# value of the Shapiro-Wilk Test is greater than 0.05, the data is normal. If it is below 0.05, the data significantly deviate from a normal distribution. If you need to use skewness and kurtosis values to determine normality, rather the Shapiro-Wilk test, you will find these in our enhanced testing for normality guide.

shapiro.test(data$Division[0:5000])
shapiro.test(data$Vers1[0:5000])
shapiro.test(data.without.outliers$Vers1[0:5000])

wilcox.test(data$Vers1)



#EDA Explatory Data Analysis with DataExplorer
# -DataExplorer

# EDA with 

  config <- configure_report(
    #add_plot_str = FALSE,
    #add_plot_qq = FALSE,
    add_plot_prcomp = FALSE,
    add_plot_density = TRUE,
    #add_plot_boxplot = FALSE,
    #add_plot_scatterplot = FALSE,
    global_ggtheme = quote(theme_minimal(base_size = 14))
  )
  
data.clean %>% create_report(
    output_file = "Omrane_DataExplorerEDA",
    #y="Description",
    report_title = "EDA Report - Omrane Population",
    config = config
  )

?create_report

sample %>% create_report(
  output_file = "Omrane_Sample_Report",
  #y="Description",
  report_title = "EDA Report - Omrane Sample",
  config = config
)



# 2.0 Data Introduction
data %>% introduce()
data %>% plot_intro()



# 3.0 Missing Values
data %>% plot_missing()
data %>% profile_missing()


# 4.0 Continuous Features
data.clean %>% plot_density()
data %>% plot_histogram()
# Explore Numeric Variable Via Descriptive Statistics
dlookr::describe(data) %>% flextable()

data %>%
    group_by(Description) %>%
    univar_numeric(TotalAvances)

data %>%
  #group_by(Description) %>%
  diagnose_numeric() %>%
  flextable()



# 5.0 Categorical Features (Explore Categorical Variables with Fisher's & Chi-Square
data %>% plot_bar()
plot_bar(data, by = "Description")



# 6.0 Relationship non-parametric Kendall OR spearman
ggplotly(plot_correlation(data.clean))
#cor.ci(data, method="spearman")
ggcorrmat(data=data, output="dataframe")


# Normality Diagnosis
plot_qq(data)
plot_qq(data, by="Description")
#ggqqplot(data, "Superficie", facet.by = "Description")




# EDA with SmartEDA

ExpReport(
     data.clean, 
     op_file="Omrane_SmartEDA.html", 
     op_dir = tempdir()
  )
?ExpReport



# EDA with dlookr

eda_report(
      data.clean,  
      target = "Description", 
      output_format = "html",
      output_file = "Omrane_dlookrEDA.html",
      output_dir = ".",
    )
?eda_report


#Preparing data for the ML Models

data.ml = select(data, c(-"Opération",-"Division",-"Client",-"NombreUnité",-"TypeDocument",
                         -"VersRegularis",-"TropPerçuRéglé", -"TropPerçuNonRéglé",-"ResteàPayer",-"StatutADV",-"Création",
                         -"Dossier",-"NuméroClient",-"Vers1",-"Vers2",-"Vers3",-"TotalVersement",-"DésignationTypeDeDocument",-"PrixTotal"))
#data.ml = select(data.ml,c(-"DésignationTypeDeDocument"))
summary(data.ml)
colnames(data.ml)[3] <- "TypeUnite"


# Exporting Datasets
# Removing Outliers
#boxplot(data.ml$Superficie)
#boxplot(data.ml$PrixUnitaire)
#data.ml<-data.ml[!(data.ml$PrixUnitaire==2600000.00),]
#data.ml<-data.ml[!(data.ml$PrixUnitaire==170300.00),]
#data.ml<-data.ml[!(data.ml$Superficie==21020.0),]
#data.ml_out_rm = data.ml[!data.ml %in% boxplot.stats(data.ml)$out]
write.csv(data.ml,"data-ml.csv",row.names = FALSE)
write.csv(data,"data.csv",row.names = FALSE)


summary(data.clean$Vers2)
max(data.clean$Superficie)


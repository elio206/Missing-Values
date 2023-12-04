#question 1#(imports the data and changes column names)
data=read.csv("C:/Users/LF ELEC/Desktop/CNAM/STA- 211/adult.csv", 
              col.names = c("Age","Workclass","fnlwgt","Edication_level",
                            "Education_Num", "Maritual_status","Occupation",
                            "Relationship","Race","Sex","Capital_Gain",
                            "Capital_Loss","Hours_per_Week","Native_Counry",
                            "Revenu"),na.strings=c("",".","NA","?"))
View(data)
#question 2#(gives us the class & type of the data)
attributes(data)
#question 3 & 4#(gives us the structure and comprehension of each var )
str(data)
#question 5#(descriptive statistics of var Education_Num)
library(Hmisc)
describe(data$Education_Num)
#question 6#(box plot & histogram for var Education_Num)
boxplot(data$Education_Num,ylab="Education number",
        xlab="Education box plot",col = "red")
hist(data$Education_Num,main = "Education Histogram",
     xlab ="Number of education",ylab ="Frequency",border ="black",
     col = "red")

#question 7#(number of NA & representation)
#before we have to replace "?" by NA
data=as.data.frame(data)
data_NA=data
data_NA[data_NA == "?"]=NA
data_NA[data_NA ==" ?"]=NA
View(data_NA)
########nbr de NA manquantes en utilisant plusieurs methodes
#workclass,occupation,nativecountry :are the var with NA#
#methode 1 #urilisant missForest onpeut s'avoir 20% de NA, 30% ou 100%
data.miss=missForest::prodNA(data, noNA = 0.2)
summary(data.miss)
#methode 2 #utilisant fct sapply
sapply(X = data_NA, FUN = function(x) sum(is.na(x)))

#######representation de NA
#methode 1#utilisant fct vis_miss
library(visdat)
vis_miss(data_NA)
#methode 2#utilisant fct gg_miss_var
if(!require('naniar')) {
  install.packages('naniar')
  library('naniar')
}
library(ggplot2)
gg_miss_var(data_NA) + labs(y = "Look at all the missing ones")
#methode 3#
library(mice)
library(VIM) 
aggr(data_NA, col = c("navyblue","red"), sortVars = TRUE, numbers = TRUE)
#methode 4#
library(mice)
md.pattern(data_NA)
colSums(is.na(data_NA)/nrow(data_NA))
rowSums(is.na(data_NA)/ncol(data_NA))

#question 8#eliminant les NA colonne par colonne(on efface le row contenant NA)
d1=na.omit(data_NA)
View(d1)

#question 9#
#methode using gsub#
d1$Edication_level=gsub("HS-grad","High school",d1$Edication_level)
d1$Edication_level=gsub("11th","High school",d1$Edication_level)
d1$Edication_level=gsub("some-college","Hight school",d1$Edication_level)
d1$Edication_level=gsub("Assoc-acdm","Undergraduated degree",d1$Edication_level)
d1$Edication_level=gsub("5th-6th","Undergraduted degree",d1$Edication_level)
d1$Edication_level=gsub("Bachelors","BA degree",d1$Edication_level)
d1$Edication_level=gsub("Masters","MSc degree",d1$Edication_level)
d1$Edication_level=gsub("Doctorate","PHD degree",d1$Edication_level)
d1$Edication_level=gsub("Prof-school","PHD degree",d1$Edication_level)
View(d1)
#question 10#
#methode using gsub#
d1$Occupation=gsub("Exec-managerial","Business",d1$Occupation)
d1$Occupation=gsub("Handlers-cleaners","Public",d1$Occupation)
d1$Occupation=gsub("Prof-speciality","Business",d1$Occupation)
d1$Occupation=gsub("Other service","Public",d1$Occupation)
d1$Occupation=gsub("Adm-clerical","Business",d1$Occupation)
d1$Occupation=gsub("sales","Business",d1$Occupation)
d1$Occupation=gsub("Transport-moving","Public",d1$Occupation)
d1$Occupation=gsub("Forming-fishing","Public",d1$Occupation)
d1$Occupation=gsub("Machine-op-inspect","Business",d1$Occupation)
d1$Occupation=gsub("Tech-support","Business",d1$Occupation)
d1$Occupation=gsub("Craft-repair","Public",d1$Occupation)
d1$Occupation=gsub("Protective-serv","Business",d1$Occupation)
View(d1)
#question 11#
#method using gsub#
d1$Workclass=sub("Self-emp-not-inc","Private",d1$Workclass)
d1$Workclass=sub("Private","Private",d1$Workclass)
d1$Workclass=sub("State-gov","Private",d1$Workclass)
d1$Workclass=sub("Federal-gov","Private",d1$Workclass)
d1$Workclass=sub("Local-gov","Private",d1$Workclass)
View(d1)
#question 12#(Methode 1)
#data adult contient des variables Quanitatives et Qualitatives
#alors en utilisant Analyse Factorielle des Donnes Mixtes(AFDM)
#le but: 1)Reduire la dimension (le nbr de variable)
#       2)enlever la multicolinearite entre les variables
#en utilisant 
library("FactoMineR")
library("factoextra")
#utilisant la fct FAMD pour:
AFDM=FAMD(d1, graph = FALSE);AFDM;
#a)Extraction des valeurs propres / variances des composantes principales.
get_eigenvalue(AFDM)
#maintenant on a reduit la dimension de 14 var a 5
#b)Visualisation des valeurs propres.
library(dplyr)
fviz_eig(AFDM) %>% plotly::ggplotly()
#c)pour extraire les résultats pour les variables
var=get_famd_var (AFDM) ; var;
# Coordonnées des variables
head(var$coord)
# Cos2: qualité de représentation
head(var$cos2)
# Contributions aux dimensions
head(var$contrib)
# pour visualiser les variables quantitatives et qualitatives
# Graphique des variables
fviz_famd_var (AFDM, repel = TRUE)
#pour visualiser la contribution des variables aux axes principaux
# Contribution à la première dimension
fviz_contrib (AFDM, "var", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (AFDM, "var", axes = 2)
# Contribution à la troisieme dimension
fviz_contrib (AFDM, "var", axes = 3)
# Contribution à la qatieme dimension
fviz_contrib (AFDM, "var", axes = 4)
########VARIABLE QUANTITATIVES######
#Dans cette section, nous décrirons comment visualiser les variables
#quantitatives. De plus, nous allons montrer comment mettre en évidence les 
#variables selon i) leurs qualités de représentation ou ii) leurs contributions 
#aux dimensions.
#Pour extraire les résultats pour les variables quantitatives
#ICI ON PEUT PARLER SUR QUALITER DE REPRESENTATION & CONTRIBUTION & COORDONNEES
quanti.var <- get_famd_var(AFDM, "quanti.var")
quanti.var
#REMARQUE: parametre gradient color c'est pour varier les couler
#I)QUALITE DE REPRESENTATION DES VARIABLES QUAN
fviz_famd_var(AFDM, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800",
                                         "#FC4E07","#fc38e4","#38fc50"), repel = TRUE)
                                         #II)CONTRIBUTION AUX DIMENSIONS
fviz_famd_var(AFDM, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", 
                                         "#FC4E07","#fc38e4","#38fc50"), repel = TRUE)
                                         #######VARIABLES QUALTATIVES######
#Dans cette section, nous décrirons comment visualiser les variables
#qualitatives. De plus, nous allons montrer comment mettre en évidence les 
#variables selon i) leurs qualités de représentation ou ii) leurs contributions 
#aux dimensions.
#Pour extraire les résultats pour les variables qualitatives
#ICI ON PEUT PARLER SUR QUALITER DE REPRESENTATION & CONTRIBUTION & COORDONNEES
quali.var <- get_famd_var(AFDM, "quali.var")
quali.var 
#I)QUALITE DE REPRESENTATION DES VARIABLES QUAL
fviz_famd_var(AFDM, "quali.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07",
                                         "#fc38e4", "#38fc50"), repel = TRUE)
                                         #II)CONTRIBUTION AUX DIMENSIONSDES VARIABLES QUAL
fviz_famd_var(AFDM, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"
                                         ,"#fc38e4","#38fc50"),repel = TRUE)
#QUESTION 12#(methode 2)
---install.packages (c ("Factoshiny","missMDA","FactoInvestigate"))--
  library(Factoshiny)
result=Factoshiny(d1)
Factoshinny(result)

#QUESTIONS 13#
#remplacons les NA aleatoirement
#les 3 variables qualitatives contiennent dess NA
library(Hmisc)
data_imp_simple=impute(data_NA$Workclass,"random");data_imp_simple;
data_imp_simple=impute(data_NA$Occupation,"random");data_imp_simple;
data_imp_simple=impute(data_NA$Native_Counry,"random");data_imp_simple;
summary(data_imp_simple)
#QUESTION 14#
#Algorithme d'imputation
#Renplacons les valeurs manquantes par Mice:
#le tableau final qui ne contient pas des NA est note par "dm"
#la fonction mice dans la library MICE travaille a imputer les NA
#le nombre d'imputation multiple est m=5 (5 possibilites) 
#le nombre d'iteration est maxit=50 (chaqune contienne 5 possibilites)  
#le parametre print nous donne les differentes imputations de chaque iteration 
#le param seed=entier positif por initialiser la generation aleatoire des nbrs 
#methode 1#
library(mice)
#On déclare les variables workclass,occupation,NativeCountry qualitatives :
data_NA$Workclass=as.factor(data_NA$Workclass) 
data_NA$Occupation=as.factor(data_NA$Occupation) 
data_NA$Native_Counry=as.factor(data_NA$Native_Counry) 
data_imp_multiple = mice(data_NA, m = 5, seed=1 , print = FALSE,
                         remove_collinear=FALSE,maxit = 50)
summary(data_imp_multiple)
#on peut voir les nouvelle valeurs remplacees au lieux de NA
data_imp_multiple$imp
#on peut comparer le tableau initial qui contient NA
#avec le tableau cree apres imputation en regardant les statistiques univariee 
#par exemple tableau initial et le 1er tableau apres imputatuion
c1=complete(data_imp_multiple,1);summary(c1);
c2=complete(data_imp_multiple,2)
c3=complete(data_imp_multiple,3)
c4=complete(data_imp_multiple,4)
c5=complete(data_imp_multiple,5)
View(data_NA)
#methode 2#
library(VIM)
data_imp_multiple2=kNN(data_NA,variable =c("Workclass","Occupation",
                                           "Native_Country"),k=5,
                       dist_var =c("Workclass","Education_Num","Matitual_status",
                                   "Occupation","Race","Sex","Native_Country","Revenu"))
data_imp_multiple2=kNN(data_NA,variable=colnames(data),
                       k=5,dist_var=colnames(data))
str(data_imp_multiple2)
summary(data_imp_multiple2)

#QUESTION 15#
#c'est une donnee supervisee
#methode 1:methode ascendante de chi-Merge de Kerber
###prenons chaque variable quantitative avec la variable "REVENU"
library(discretization)

#cas 1: Age et Revenu################
d_cas1=d1[,c("Age","Revenu")]
str(d_cas1)
res2=chiM(d_cas1,alpha = 0.01)
res2
#En utilisant la variable "cutp" en obtient la borne du decoupage
res2$cutp
# base de donnees apres decoupage
d_cas1_result=res2$Disc.data
d_cas1_result
str(d_cas1_result)
summary(d_cas1_result)
d_cas1_result$Age=factor(d_cas1_result$Age,levels=c(1:11),
                         labels=c("<20.5","[20.5,23.5[",
                           "[25.5,24.5[","[24.5,27.5[",
                          "[27.5,29.5[","[29.5,33.5[",
                         "[33.5,36.5[","[36.5,43.5[",
                         "[43.5,54.5[","[54.5,61.5[",
                         ">=61.5"))
d_cas1_result$Age
# tableau de contingence
t2=table(d_cas1_result$Age,d_cas1_result$Revenu)
t2
# tester l'independance de y et la variable group 
#H0:independance entre les deux
#H1: il y a une dependance entre les var.
chisq.test(t2)
####------------------------------------#########
##case 2:fnlwg et Revenu###
d_cas2=d1[,c("fnlwgt","Revenu")]
str(d_xas2)
res3=chiM(d3,alpha = 0.01)
res3
# borne du decoupage
res3$cutp
# base de donnees apres decoupage
d_cas2_result=res2$Disc.data
d_cas2_result
str(d_cas2_result)
summary(d_cas2_result)
d_cas2_result$fnlwgt=factor(d_cas2_result$fnlwg,
                  levels=c(1:11),labels=c("<20.5","[20.5,23.5[",
                "[25.5,24.5[","[24.5,27.5[","[27.5,29.5[","[29.5,33.5[",
            "[33.5,36.5[","[36.5,43.5[","[43.5,54.5[","[54.5,61.5[",">=61.5"))
# tableau de contingence
t3=table(d_cas2_result$fnlwg,d_cas2_result$Revenu)
t3
# tester l'independance de y et la variable group 
#H0:independance entre les deux
#H1: il y a une dependance entre les var.
chisq.test(t3)
#####de meme pour les autres variables quantitatives###
##meme methode pour les variables :
#Education_Num, Capital_Gain, Capital_Loss, Hours_per_week

#QUESTION 15#
#c'est une donnee supervisee
##Methode 2: arbre de decision
# decoupage selon l'arbre de decision- donnees supervisee

library(rpart)
r=rpart(Revenu~Age, method="class",data=d1)
summary(r)

r2=rpart(Revenu ~ Age, method="class",data=d2, control=rpart.control(minsplit=5,cp=0.05))

r3=rpart(Revenu ~ fnlwgt, method="class", data=d3, control=rpart.control(cp=0.05))
r4=rpart(Revenu ~ Education_Num, method="class", data=d4, control=rpart.control(cp=0.05))
r5=rpart(Revenu ~ Capital_Gain, method="class", data=d5, control=rpart.control(cp=0.05))          
r6=rpart(Revenu ~ Capital_Loss, method="class", data=d6, control=rpart.control(cp=0.05))                  
r7=rpart(Revenu ~ Hours_per_Week, method="class", data=d7, control=rpart.control(cp=0.05))

print(r2)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)

##### decoupage par arbre de decision
library(rpart)
#parametres d'analyse 
#"cp = 0.15" est la reduction relative minimale du critere de qualite de l'arbre
paramArbre2 <- rpart.control(minsplit=5,cp=0.15)
arbre2 <- rpart(Revenu ~ Age, data = d2, control = paramArbre2)
print(arbre2)

paramArbre3 <- rpart.control(cp=0.15)
arbre3 <- rpart(Revenu ~ fnlwgt, data = d3, control = paramArbre3)
print(arbre3)

paramArbre4 <- rpart.control(cp=0.15)
arbre4 <- rpart(Revenu ~ Education_Num, data = d4, control = paramArbre4)
print(arbre4)


paramArbre5 <- rpart.control(cp=0.15)
arbre5 <- rpart(Revenu ~ Capital_Gain, data = d5, control = paramArbre5)
print(arbre5)

paramArbre6 <- rpart.control(cp=0.15)
arbre6 <- rpart(Revenu ~ Capital_Loss, data = d6, control = paramArbre6)
print(arbre6)

paramArbre7 <- rpart.control(cp=0.15)
arbre7 <- rpart(revenu ~ Hours_per_Week, data = d7, control = paramArbre7)
print(arbre7)

#affichage sympathique 
library(rpart.plot)
rpart.plot(arbre2,digits=3)
rpart.plot(arbre3,digits=3)
rpart.plot(arbre4,digits=3)
rpart.plot(arbre5,digits=3)
rpart.plot(arbre6,digits=3)
rpart.plot(arbre7,digits=3)

#description des decoupages 
print(r2$splits)
arbre2$splits
print(arbre3$splits)
arbre3$splits
print(arbre4$splits)
arbre4$splits
print(arbre5$splits)
arbre5$splits
print(arbre6$splits)
arbre6$splits

#delimitation des intervalles de discretisation 

r2$splits
bornes2 <- arbre2$splits[,'index'] 
bornes2 <- c(-Inf,sort(bornes2),+Inf) 
print(bornes2)

r3$splits
bornes3 <- arbre3$splits[,'index'] 
bornes3 <- c(-Inf,sort(bornes3),+Inf) 
print(bornes3)

r4$splits
bornes4 <- arbre4$splits[,'index'] 
bornes4 <- c(-Inf,sort(bornes4),+Inf) 
print(bornes4)

r5$splits
bornes5 <- arbre5$splits[,'index'] 
bornes5 <- c(-Inf,sort(bornes5),+Inf) 
print(bornes5)

r6$splits
bornes6 <- arbre6$splits[,'index'] 
bornes6 <- c(-Inf,sort(bornes2),+Inf) 
print(bornes6)

r7$splits
bornes7 <- arbre7$splits[,'index'] 
bornes7 <- c(-Inf,sort(bornes7),+Inf) 
print(bornes7)

#appliquer aux donnes
gcount=cut(d1$age,born,labels=F);gcount
str(gcount)
d$gcount=factor(gcount,labels=c("<2.5","[2.5,6.5[",">=6.5"))
summary(d1)
View(d)

#appliquer aux donnees
a=cut(d1$Age,bornes2,labels=F);a
b=cut(d1$fnlwgt,bornes3,labels=F);b
c=cut(d1$Educatio_Nnum,bornes4,labels=F);c
d=cut(d1$Capital_Gain,bornes5,labels=F);d
e=cut(d1$Capital_Loss,bornes6,labels=F);e
f=cut(d1$Hours_per_Week,bornes7,labels=F);f


###-------------------------------###
### Algorithme  descendsante -methode MDLP (Fayyad et Irani, 1993) 

#chargement du package discretization 
library(discretization)
#discretisation supervisee des variables
discAR2 <- mdlp(d2)
discAR3 <- mdlp(d3)
discAR4 <- mdlp(d4)
discAR5 <- mdlp(d5)
discAR6 <- mdlp(d6)
discAR7 <- mdlp(d7)

#bornes de discretisation 
print(discAR2$cutp) 
print(head(discAR2$Disc.data))

print(discAR3$cutp) 
print(head(discAR3$Disc.data))

print(discAR4$cutp) 
print(head(discAR4$Disc.data))

print(discAR5$cutp) 
print(head(discAR5$Disc.data))

print(discAR6$cutp) 
print(head(discAR6$Disc.data))

print(discAR7$cutp) 
print(head(discAR7$Disc.data))

#qualite du decoupage 
print(table(d2$Revenu,discAR2$Disc.data$Age))
print(table(d3$Revenu,discAR3$Disc.data$fnlwgt))
print(table(d4$Revenu,discAR4$Disc.data$Education_Num))
print(table(d5$Revenu,discAR5$Disc.data$Capital_Gain))
print(table(d6$Revenu,discAR6$Disc.data$Capital_Loss))
print(table(d7$Revenu,discAR7$Disc.data$Hours_per_Week))

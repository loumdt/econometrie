library(readxl);
library(ggplot2);
library(reshape2);
library(emmeans)
library(dplyr)
library(lmtest)
library(kableExtra)
library(FactoMineR)
library(factoextra)
library(cowplot)
library(ppcor)
library(MASS)


dataset <- read_excel("D:/Ubuntu/M2_EEET/Econométrie/Transport_France2019_v2.xlsx");
#View(dataset);
df = data.frame(dataset)
df = subset (df, select = -c(GDP))
View(df)


#attach(dataset);
names(dataset);
year_beg = dataset$Year[1]
year_end = sapply(list(dataset$Year), tail, 1)




png(file = "Qtt_transport_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = Qtt_Trsp_route, colour = "Qtt_Trsp_route")) +
  geom_line(aes(y = Qtt_Trsp_train, colour = "Qtt_Trsp_train")) +
  scale_colour_manual("", 
                      breaks = c("Qtt_Trsp_route", "Qtt_Trsp_train"),
                      values = c("Qtt_Trsp_route"="green", "Qtt_Trsp_train"="red")) +
  xlab("Années") +
  scale_y_continuous("Transport", limits = c(0,350)) + 
  labs(title="Quantités transportées par route et par train, 1985-2019")
dev.off();



png(file = "Qdiesel_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = Qdieselcamion, colour = "Qdieselcamion")) +
  geom_line(aes(y = QDiesel, colour = "QDiesel")) +
  scale_colour_manual("", 
                      breaks = c("Qdieselcamion", "QDiesel"),
                      values = c("Qdieselcamion"="green", "QDiesel"="red")) +
  xlab("Années") +
  scale_y_continuous("Transport", limits = c(7500,35000)) + 
  labs(title="Quantités de diesel consommées totale et par les camions routiers, 1985-2019")
dev.off();

png(file = "PIB_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = `PIB en volume (en milliards d'euros 2014)`, colour = "PIB")) +
  xlab("Années") +
  labs(title="Evolution du PIB (en milliards d'euros de 2014), 1985-2019")
dev.off();

png(file = "CPI_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = CPI, colour = "CPI")) +
  xlab("Années") +
  labs(title="Evolution du l'Indice des Prix à la Consommation (base 2015), 1985-2019")
dev.off();

png(file = "Pdiesel_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = Pdiesel, colour = "Pdiesel")) +
  xlab("Années") +
  labs(title="Evolution du prix du Diesel (base 2015), 1985-2019")
dev.off();



#==================================#
#                                  #
#      Matrice de corrélation      #
#                                  #
#==================================#
library(corrplot);
df_noyear = subset (df, select = -c(Year))
names(df_noyear)

names(df_noyear)[names(df_noyear)=="PIB.en.volume..en.milliards.d.euros.2014."] <- "PIB"

mat_cor<-cor(df_noyear) ; #Correlation matrix

par(xpd=TRUE)
png(file = "Mat_correlation_all_var.png", width = 800, height = 700)
corrplot(mat_cor, type="upper", tl.col="black", tl.srt=45) #Correlation matrix plot
dev.off()


#==================================#
#                                  #
# Choix des variables explicatives #
#                                  #
#==================================#

y = df$Qdieselcamion ; #variable expliquée
var_exo = data.frame(df$Qtt_Trsp_route,df$Qtt_Trsp_train,df$Pdiesel,df$QDiesel,df$CPI,df$PIB.en.volume..en.milliards.d.euros.2014.);

f <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

#all_var_combination = f(var_exo)

donnees <- data.frame(df$Qtt_Trsp_route,df$Qtt_Trsp_train,y)
modele <- lm(y ~ ., donnees)

print("Modèle des moindres carrés : ")
print(modele)
summary(modele)



##analyse en composantes principales
png(file='analyse_composantes_principales.png',width=1200,height=1000)
res.pca=PCA(df[,1:8], scale.unit=TRUE,ncp=9,graph=F)
get_eigenvalue(res.pca)
fviz_eig(res.pca,addlabels=TRUE,ylim=c(0,70))
var=get_pca_var(res.pca)
fviz_pca_var(res.pca,geom=c("text","arrow"),col.var="cos2",axes=1:2,repel=TRUE)+theme_classic()
ind=get_pca_ind(res.pca)
cos2=rowSums(res.pca$ind$cos2[,1:2])
fviz_pca_ind(res.pca,col.ind = 'cos2',axes=1:2,repel=TRUE)
dev.off()



# RÃ©sultats des variables
eig.val <- res.pca$eig ;

res.var <- res.pca$var ;
res.var$coord    ;       # CoordonnÃ©es
res.var$contrib   ;      # Contributions aux axes
res.var$cos2   ;         # QualitÃ© de reprÃ©sentation
# RÃ©sultats des individus
res.ind <- res.pca$ind ;
res.ind$coord     ;      # CoordonnÃ©es
res.ind$contrib    ;     # Contributions aux axes
res.ind$cos2   ;         # QualitÃ© de reprÃ©sentation

#res.var;

#png(file = "PCA.png",  width = 1200, height = 1000) ;
#fviz_pca_var(res.pca, axes = c(1, 2)) ;
#dev.off() ;





## corrélation partielle 
png(file = "correlation_partielle.png",  width = 1200, height = 1000) ;
correlation_part=ppcor::pcor(df[,1:8])$estimate
correlation
kable(correlation_part,digits=3)
corrplot(correlation_part)
dev.off()


##### modèle ancova 1

## modèle ancova 1
png(file = "modele_ancova_toutes_var.png",  width = 1200, height = 1000) ;
ancova_Qdieselcamion=lm(Qdieselcamion~Year+Qtt_Trsp_train+Pdiesel+QDiesel+CPI+PIB.en.volume..en.milliards.d.euros.2014.,data=df)
print(ancova_Qdieselcamion)
par(mfrow=c(2,2))
plot(ancova_Qdieselcamion)
dev.off()


plot(ancova_Qdieselcamion)
anova(ancova_Qdieselcamion)

## modèle ancova 2 : QDiesel + Pdiesel
png(file="modele_ancova_Qdiesel_Pdiesel.png",width=1200,height=1000)
ancova2_Qdiesel=lm(Qdieselcamion~Pdiesel*QDiesel,data=df)
print(ancova2_Qdiesel)
par(mfrow=c(2,2))
plot(ancova2_Qdiesel)
dev.off()

plot(ancova2_Qdiesel)
anova(ancova2_Qdiesel)

## modèle ancova 2 : qtt_trsp_train + QDiesel
png(file="modele_ancova_Qdiesel_qtt_trsp_train.png",width=1200,height=1000)
ancova3_Qdiesel=lm(Qdieselcamion~Qtt_Trsp_train*QDiesel,data=df)
print(ancova3_Qdiesel)
par(mfrow=c(2,2))
plot(ancova3_Qdiesel)
dev.off()

plot(ancova3_Qdiesel)
anova(ancova3_Qdiesel)

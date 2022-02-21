library(readxl);
library(ggplot2);
library(reshape2);


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

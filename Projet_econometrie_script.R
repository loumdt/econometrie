library(readxl);
library(ggplot2);
library(reshape2);


dataset <- read_excel("D:/Ubuntu/M2_EEET/Econom�trie/Transport_France2019_v2.xlsx");
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
  xlab("Ann�es") +
  scale_y_continuous("Transport", limits = c(0,350)) + 
  labs(title="Quantit�s transport�es par route et par train, 1985-2019")
dev.off();



png(file = "Qdiesel_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = Qdieselcamion, colour = "Qdieselcamion")) +
  geom_line(aes(y = QDiesel, colour = "QDiesel")) +
  scale_colour_manual("", 
                      breaks = c("Qdieselcamion", "QDiesel"),
                      values = c("Qdieselcamion"="green", "QDiesel"="red")) +
  xlab("Ann�es") +
  scale_y_continuous("Transport", limits = c(7500,35000)) + 
  labs(title="Quantit�s de diesel consomm�es totale et par les camions routiers, 1985-2019")
dev.off();

png(file = "PIB_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = `PIB en volume (en milliards d'euros 2014)`, colour = "PIB")) +
  xlab("Ann�es") +
  labs(title="Evolution du PIB (en milliards d'euros de 2014), 1985-2019")
dev.off();

png(file = "CPI_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = CPI, colour = "CPI")) +
  xlab("Ann�es") +
  labs(title="Evolution du l'Indice des Prix � la Consommation (base 2015), 1985-2019")
dev.off();

png(file = "Pdiesel_historique.png",  width = 1200, height = 1000) ;
ggplot(data = df, aes(x = Year)) +
  geom_line(aes(y = Pdiesel, colour = "Pdiesel")) +
  xlab("Ann�es") +
  labs(title="Evolution du prix du Diesel (base 2015), 1985-2019")
dev.off();






#==================================#
#                                  #
# Choix des variables explicatives #
#                                  #
#==================================#

y = df$Qdieselcamion ; #variable expliqu�e
var_exo = data.frame(df$Qtt_Trsp_route,df$Qtt_Trsp_train,df$Pdiesel,df$QDiesel,df$CPI,df$PIB.en.volume..en.milliards.d.euros.2014.);

f <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

#all_var_combination = f(var_exo)

donnees <- data.frame(df$Qtt_Trsp_route,df$Qtt_Trsp_train,y)
modele <- lm(y ~ ., donnees)

print("Mod�le des moindres carr�s : ")
print(modele)
summary(modele)
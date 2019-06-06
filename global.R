## global.R ##
library("dplyr")
library("zoo")
library("stringr")
####################################################################################################
# DONNEES CARTOGRAPHIQUES
####################################################################################################
# 10 VARIABLES FACTOR 
# Station;Commune;Adresse;Identifiant;Type;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;
# 4 INT Numero;CodeInsee;lat;lon
#
#df <- read.csv2("F:/SHINY-PRODUCTION/UC2-IRVE/bornes.csv",encoding = "UTF-8")
#df <- read.csv2("bornes.csv",encoding = "UTF-8")
# 3/01/19 Bornes avec ruralite
df <- read.csv2("bornesWithRuralite.csv",encoding = "UTF-8")
# comRurales <- df %>% group_by(Commune) %>% 
#                   summarise(Ruralite=unique(Ruralite),nbStation=n_distinct(Station),nbpdc=n_distinct(CodePDC))
# table(comRurales$Ruralite)
# nbPDCRurales <- df %>% group_by(Ruralite) %>% 
#   summarise(nbCommune=n_distinct(Commune),nbStation=n_distinct(Station),nbpdc=n_distinct(CodePDC))

#liste des stations
listStations <- levels(df$Station)
nbstations <- length(listStations)
#liste des PDC
listPDC <- levels(df$CodePDC)
nbPDC <- length(listPDC)
####################################################################################################
# DONNEES HISTORIQUES
####################################################################################################
# Borne;Type;Ville;DateDebut;HeureDebut;
# DureeChargemin;Consommationkwh;DateFin;HeureFin;TypeClient;Status;DateTimeDebut;DateTimeFin
# Borne = CodePDC
transdf <- read.csv2("trans.csv",encoding = "UTF-8")
transactions <- read.csv2("trans.csv",encoding = "UTF-8")

######################################################################################################
# bornes qui n existent pas => A SUPPRIMER
# listBorne <- c("FR*S05*S05006*A*B2","FR*S05*S05046*E*B1","FR*S05*S05114*A*B1","FR*S05*S05046*E*B1","FR*S05*S05114*A*B1","FR*S05*S05177*A*B1")
# test <- filter(transactions, Borne %in% listBorne)
# conso a 0 => 1440 observations
#consonulle <- filter(transactions, Consommationkwh <= 0)
# chargemin a 0 => 751 observation
#chargenulle <- filter(transactions, DureeChargemin <= 0)
#transactions => 1790 observations sur 3294
transactions <- filter(transactions, (Consommationkwh > 0)&(DureeChargemin > 0))
transactions$Borne <- droplevels(transactions$Borne)
transactions$DateDebut <- lubridate::dmy(transactions$DateDebut)
#Ajouter moisDebut et moisFin
#debut
transactions$MoisDebut <- lubridate::month(transactions$DateDebut)
transactions$MoisFactorDebut <- as.factor(transactions$MoisDebut)
transactions$MonthDebut <- as.Date(as.yearmon(transactions$DateTimeDebut))
levels(transactions$MoisFactorDebut)<-c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre")

#Ajouter l'annee
transactions$an <- lubridate::year(transactions$DateDebut)
transactions2018 <- filter(transactions,an==2018)

# Mettre la ville au format transaction dans la cartographie des bornes
transVilles <- transactions2018 %>% group_by(Borne,Ville) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                        DureeChargemin=sum(DureeChargemin),
                                                                        nbTransaction=n())
transVilles <- rename(transVilles,CodePDC=Borne)
transVilles$CodePDC<- as.character(transVilles$CodePDC)

#ajout du champ Ville => 12 NA's
df <- left_join(df,transVilles,by="CodePDC")
#suppression des NA
df$nbTransaction <- ifelse(is.na(df$nbTransaction),0,df$nbTransaction)
df$Consommationkwh <- ifelse(is.na(df$Consommationkwh),0,df$Consommationkwh)
df$DureeChargemin <- ifelse(is.na(df$DureeChargemin),0,df$DureeChargemin)
#calcul des moyennes
df$meanTrans <- mean(df$nbTransaction)
df$meanDuree <- mean(df$DureeChargemin)
df$meanConso <- mean(df$Consommationkwh)
#calcul des ecarts sur annee 2018
# df$Ecart <- df$nbTransaction - mean(df$nbTransaction)
df$EcartTrans <- round(df$nbTransaction - mean(df$nbTransaction),0)
df$EcartDuree <- round(df$DureeChargemin - mean(df$DureeChargemin),0)
df$EcartConso <- round(df$Consommationkwh - mean(df$Consommationkwh),0)
# 
# df$EcartT <- cut(df$EcartTrans,c(-19.281,-14.281,-9.281,3.719,86.719))
# table(df$EcartT)
# levels(df$EcartT) <- c("4eme Quartile","3eme quartile","2eme quartile","1er quartile")
# df$EcartC <- cut(df$EcartConso,c(-276.15,-234.89,-122.88,43.38,1700.99))
# levels(df$EcartC) <- c("4eme Quartile","3eme quartile","2eme quartile","1er quartile")
# df$EcartD <- cut(df$EcartDuree,c(-3670.7,-3259.2,-2639.7,6808.2,37316.3))
# levels(df$EcartD) <- c("4eme Quartile","3eme quartile","2eme quartile","1er quartile")
# summary(df)

#liste des Villes
listVilles <- levels(df$Ville)
nbVilles <- length(listVilles)

#test <- filter(transactions, Borne %in% listBorne)
######################################################################################################
#CONSOLIDATION JOUR 
#PAR TYPE DE JOUR
#regroupement des data par jour en kWh
transactionsPerJour <- transactions %>% group_by(DateDebut,Borne) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                          DureeChargemin=sum(DureeChargemin),
                                                                          nbTransaction=n())
transactionsPerJour$DateDebut <- lubridate::ymd(transactionsPerJour$DateDebut)
transactionsPerJour$typeJourDebut <- lubridate::wday(transactionsPerJour$DateDebut)
transactionsPerJour$typeJourFactorDebut <- as.factor(transactionsPerJour$typeJourDebut)
levels(transactionsPerJour$typeJourFactorDebut)<-c("Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi")

#Ajouter l'annee
transactionsPerJour$an <- lubridate::year(transactionsPerJour$DateDebut)
transactionsPerJour2017 <- filter(transactionsPerJour,an==2017)
transactionsPerJour2018 <- filter(transactionsPerJour,an==2018)

################################
# CONSOLIDATION MOIS 
# regroupement des data par mois en kWh 
transactionsPerMois <- transactions %>% group_by(MonthDebut) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
                                                                           Consommationkwh=sum(Consommationkwh),
                                                                           DureeChargemin=sum(DureeChargemin),
                                                                           nbTransaction=n())
#regroupement par mois et par borne
transactionsPerMoisPerBorne <- transactions %>% group_by(MonthDebut,Borne) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
                                                                           Consommationkwh=sum(Consommationkwh),
                                                                           DureeChargemin=sum(DureeChargemin),
                                                                           nbTransaction=n())
#choix de bornes
#data1 <- filter(transactionsPerMois, Borne == "FR*S05*S05170*D*B1")
#target <- c("FR*S05*S05170*C*B1", "FR*S05*S05097*A*B1", "FR*S05*S05164*C*B1", "FR*S05*S05132*A*B1", "FR*S05*S05070*A*B2")
#data5 <- filter(transactionsPerMois, Borne %in% target)
allBorne <- transactions %>% group_by(Borne) %>% summarise(nbTransaction=n())
allBorne <- allBorne[order(allBorne$nbTransaction, decreasing=TRUE),] # df <- df[order(df$ID),]
listBornes <- levels(allBorne$Borne)
nbornes <- length(listBornes)
#datalist <- filter(transactionsPerMoisPerBorne, Borne %in% listBornes)
#Ajouter l'annee
transactionsPerMois$an <- lubridate::year(transactionsPerMois$MonthDebut)
transactionsPerMois2017 <- filter(transactionsPerMois,an==2017)
transactionsPerMois2018 <- filter(transactionsPerMois,an==2018)
#Ajouter l'annee
transactionsPerMoisPerBorne$an <- lubridate::year(transactionsPerMoisPerBorne$MonthDebut)
transactionsPerMoisPerBorne2017 <- filter(transactionsPerMoisPerBorne,an==2017)
transactionsPerMoisPerBorne2018 <- filter(transactionsPerMoisPerBorne,an==2018)

#######################################################################################################
#CONSOLIDATION SEMAINE sur l'annee 2018
transactions2018$week <- lubridate::week(transactions2018$DateDebut)
#regroupement par semaine et par commune
transactionsPerSemainePerVille2018 <- transactions2018 %>% group_by(week,Ville) %>% 
  summarise(Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
#les bornes par ville
BornePerVille2018 <- transactions2018 %>% group_by(Ville) %>% 
  summarise(nbBorne=n_distinct(Borne),
            nbTransaction=n())
#regroupement par semaine et par borne
transactionsPerSemainePerBorne2018 <- transactions2018 %>% group_by(week,Borne) %>% 
  summarise(Ville=unique(Ville),
            Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
#Ajout de la ville a la Borne
transactionsPerSemainePerBorne2018 <- mutate(transactionsPerSemainePerBorne2018,VilleBorne=paste(str_sub(Ville,1,-5),Borne))
# VISU COLONNE + transactionsPerSemainePerBorne2018
ggplot(transactionsPerSemainePerBorne2018, aes_string("week","DureeChargemin")) +
  geom_col(colour="purple",fill="pink")+ facet_wrap(~ VilleBorne) +
  labs(title = "Charge en minutes par semaine, année 2018", x = "semaine", y="minute")
######################################################################################################
#CONSOLIDATION MOIS sur l'annee 2018
#regroupement par mois et par Ville
transactionsPerMoisPerVille2018 <- transactions2018 %>% group_by(MonthDebut,Ville) %>% 
  summarise(MoisFactorDebut=unique(MoisFactorDebut),
  Consommationkwh=sum(Consommationkwh),
  DureeChargemin=sum(DureeChargemin),
  nbTransaction=n())

#regroupement par mois et par borne
# transactionsPerMoisPerBorneVille2018 <- transactions2018 %>% group_by(MonthDebut,Borne) %>% 
#   summarise(meanConso=mean(Consommationkwh),
#   Commune=unique(Ville),
#   MoisFactorDebut=unique(MoisFactorDebut),
#   Consommationkwh=sum(Consommationkwh),
#   DureeChargemin=sum(DureeChargemin),
#   nbTransaction=n())
# #regroupement des data par mois en kWh 
# transactionsPerMois <- transactions %>% group_by(MonthDebut) %>% summarise(MoisFactorDebut=unique(MoisFactorDebut),
#                                                                            Consommationkwh=sum(Consommationkwh),
#                                                                            DureeChargemin=sum(DureeChargemin),
#                                                                            nbTransaction=n())
#regroupement par mois et par borne
transactionsPerMoisPerBorne2018 <- transactions2018 %>% group_by(MonthDebut,Borne) %>% 
  summarise(Ville=unique(Ville),
            MoisFactorDebut=unique(MoisFactorDebut),
            Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
#Ajout de la ville a la Borne => champ VilleBorne
transactionsPerMoisPerBorne2018 <- mutate(transactionsPerMoisPerBorne2018,VilleBorne=paste(str_sub(Ville,1,-5),Borne))

############################
# CALCUL DE LA MOYENNE PAR BORNE PAR MOIS sur l'annee 2018 (mais il n'y a pas toutes les bornes)
moyennePerMois2018 <- transactionsPerMoisPerBorne2018 %>% group_by(MonthDebut) %>% summarise(meanConso=mean(Consommationkwh),
                                                                                           meanCharge=mean(DureeChargemin),
                                                                                           meanTrans=mean(nbTransaction))

summary(moyennePerMois2018)
# on rÃ©cupere le mois d'octobre
#ecartOctobre <- filter(transactionsPerMoisPerBorne2018,MoisFactorDebut=="octobre")
#ecartOctobre <- select(ecartOctobre, c(Borne,Consommationkwh,nbTransaction,DureeChargemin))
# ajoute aux bornes df

# decoupe en 4 classes par quartile

#ajout de l'ecart par rapport a la moyenne
# transactionsPerMoisPerBorne2018$ecartConso <- round(transactionsPerMoisPerBorne2018$Consommationkwh-60.38,0)
# transactionsPerMoisPerBorne2018$ecartTrans <- round(transactionsPerMoisPerBorne2018$nbTransaction-3.7,0)
# transactionsPerMoisPerBorne2018$ecartDuree <- round(transactionsPerMoisPerBorne2018$DureeChargemin-887.1,0)
# summary(transactionsPerMoisPerBorne2018)
# ECART de consommation
#transactionsPerMoisPerBorne2018$Ecart <- cut(transactionsPerMoisPerBorne2018$ecartConso,c(-60.378,-47.163,-26.713,16.320,675))
#levels(transactionsPerMoisPerBorne2018$Ecart)
#table(transactionsPerMoisPerBorne2018$Ecart)
# decoupe en 4 classes par quartile
#ecartOctobre <- rename(ecartOctobre,CodePDC=Borne)
#ecartOctobre$CodePDC<- as.character(ecartOctobre$CodePDC)

#Consommation totale deja ajoutee
#ajouter en precisant octobre
#df <- left_join(df,ecartOctobre,by="CodePDC")

#################################################################################################
# RAF consolidation heure et VISU HEURE
transactionsPerHeure <- transactions %>% group_by(DateTimeDebut,Borne) %>% summarise(Consommationkwh=sum(Consommationkwh),
                                                                               DureeChargemin=sum(DureeChargemin),
                                                                               nbTransaction=n())
#Ajouter HHDebut 
transactionsPerHeure$HH <- lubridate::hour(transactionsPerHeure$DateTimeDebut)
transactionsPerHeure$HHFactor <- as.factor(transactionsPerHeure$HH)
#Ajouter l'annee
transactionsPerHeure$an <- lubridate::year(transactionsPerHeure$DateTimeDebut)
transactionsPerHeure2017 <- filter(transactionsPerHeure,an==2017)
transactionsPerHeure2018 <- filter(transactionsPerHeure,an==2018)
########################################################################################################################################################
# METEO ET TRANSACTIONS
# 11/01/2019
########################################################################################################################################################
# meteoTransactionPerMois, file="meteoTransMOIS.csv"
# meteoTransactionPerJour, file="meteoTransJOUR.csv"
meteoTransactionPerJour <- read.csv2("meteoTransJOUR.csv",encoding = "UTF-8")
meteoTransactionPerJour$DateJour <- as.Date(meteoTransactionPerJour$DateJour)
meteoTransactionPerMois <- read.csv2("meteoTransMOIS.csv",encoding = "UTF-8")
meteoTransactionPerMois$Month <- as.Date(meteoTransactionPerMois$Month)
meteoTransactionsMois <- select(meteoTransactionPerMois,c(temp,humidite,pointRosee,visu,pluie1,pluie3,Consommationkwh,DureeChargemin,nbTransaction))
tsmeteoTransactionPerJour <- meteoTransactionPerJour[27:675,] #premieres transactions le 27 janvier
#preferable s'il n'y a pas l'annee entiere 2018
y <- ts(tsmeteoTransactionPerJour,frequency=365,start=c(2017,27),end=c(2018,310))
########################################################################################################################################################
# UC2 - reservations des bornes du 05
########################################################################################################################################################
# F:/Flexgrid/BORNES IRVE/UC2/
resabornes05 <- read.csv2("resaNovembre.csv",header=FALSE,encoding = "UTF-8",row.names=NULL)
# changement des noms de colonnes => deviennent les noms de ligne
colnames(resabornes05)<- c(1:31)
# transposition de la matrice
resa05 <- as.data.frame(t(resabornes05))
# noms des colonnes
colnames(resa05) <- c("date","nbResa","Bornes")
# suppression de la ligne qui correspond aux anciens noms de colonne (par defaut)
resa05 <- resa05[-1,]
################################################################################
# format date
resa05$date <- lubridate::as_date(resa05$date)
resa05$nbResa <- as.numeric(resa05$nbResa)
# basic plot 
#calendarPlot(resa05, pollutant="nbResa",year = 2018, month=6:12)
#calendarPlot(resa05, pollutant="nbResa",year = 2018, 
#             month=6:12, cols=c("green","blue","orange","red"))
resa05$week <- factor(strftime(resa05$date,format="%V"))
resa05$month <- factor(strftime(resa05$date,format="%b"),levels = c("oct.","nov."))
resa05$ddate <- factor(strftime(resa05$date,format="%d"))
#il faut ordonner les facteur pour avoir lundi Ã  dimanche !
#comme il s'agit d'un Y c'est de dimanche Ã  lundi
resa05$day <- factor(strftime(resa05$date,format="%a"),levels = rev(c("lun.","mar.","mer.","jeu.","ven.","sam.","dim.")))
# add date tracks
resa05$comment <- "Libre"
resa05$comment[resa05$date>=as.Date('2018-11-26') & resa05$date<=as.Date('2018-11-28')] <- "Reservee"
resa05$comment[resa05$date>=as.Date('2018-11-01') & resa05$date<=as.Date('2018-11-01')] <- "Ferie"
resa05$comment[resa05$date>=as.Date('2018-11-05') & resa05$date<=as.Date('2018-11-06')] <- "Indisponible"
resa05$comment[resa05$date>=as.Date('2018-11-11') & resa05$date<=as.Date('2018-11-11')] <- "Ferie"
resa05$comment[resa05$date>=as.Date('2018-11-07') & resa05$date<=as.Date('2018-11-10')] <- "Reservee"
#resa05$comment[resa05$day=="sam." | resa05$day=="dim."] <- "Weekend"
resa05$comment <- factor(resa05$comment,levels=c("Libre","RÃ©servÃ©e","Indisponible","FeriÃ©"))

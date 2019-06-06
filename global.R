## global.R ##
library("dplyr")
library("zoo")
library("stringr")
####################################################################################################
# DONNEES CARTOGRAPHIQUES : BORNES
####################################################################################################
# Bornes avec donnees de ruralite
# 15 VARIABLES : 
# Station;Commune;Adresse;Identifiant;Type;Numero;CodeInsee;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;lat;lon;Ruralite
# TOURTON;CHABOTTES;Parking Haute Plaine SO 05260 CHABOTTES (05);FR*XXX*XXXXX*A*B1*D;ACCELEREE;16322;5029;Droit;FR*XXX*XXXXX*A;FR*XXX*XXXXX*A-1;FR*XXX*XXXXX*A*B1;PDLXXXXXXXXXXX;44,6428413391113;6,16910123825073;400 - Com. isolées hors influence des pôles
df <- read.csv2("bornesWithRuralite.csv",encoding = "UTF-8")

#liste des stations
listStations <- levels(df$Station)
nbstations <- length(listStations)
#liste des Points De Charge - PDC
listPDC <- levels(df$CodePDC)
nbPDC <- length(listPDC)
####################################################################################################
# DONNEES HISTORIQUES : TRANSACTIONS
####################################################################################################
# Borne;Type;Ville;DateDebut;HeureDebut;DureeChargemin;Consommationkwh;DateFin;HeureFin;TypeClient;Status;DateTimeDebut;DateTimeFin
# FR*XXX*XXXXX*A*B1;Rapide;ANCELLE (05);23/03/2017; 12:28;0;0;23/03/2017; 12:28;Individuel;CHARGE;2017-03-23 12:28:00;2017-03-23 12:30:00
# Borne = CodePDC
transactions <- read.csv2("trans.csv",encoding = "UTF-8")

######################################################################################################
# bornes qui n existent pas => A SUPPRIMER
# Consommation nulle ou Duree de charge nulle => transactions supprimees
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

#ajout du champ Ville 
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

#liste des Villes
listVilles <- levels(df$Ville)
nbVilles <- length(listVilles)

######################################################################################################
# CONSOLIDATION JOUR 
# PAR TYPE DE JOUR
# regroupement des data par jour en kWh
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
  labs(title = "Charge en minutes par semaine, annÃ©e 2018", x = "semaine", y="minute")
######################################################################################################
#CONSOLIDATION MOIS sur l'annee 2018
#regroupement par mois et par Ville
transactionsPerMoisPerVille2018 <- transactions2018 %>% group_by(MonthDebut,Ville) %>% 
  summarise(MoisFactorDebut=unique(MoisFactorDebut),
  Consommationkwh=sum(Consommationkwh),
  DureeChargemin=sum(DureeChargemin),
  nbTransaction=n())

#regroupement par mois et par borne
transactionsPerMoisPerBorne2018 <- transactions2018 %>% group_by(MonthDebut,Borne) %>% 
  summarise(Ville=unique(Ville),
            MoisFactorDebut=unique(MoisFactorDebut),
            Consommationkwh=sum(Consommationkwh),
            DureeChargemin=sum(DureeChargemin),
            nbTransaction=n())
#Ajout de la ville a la Borne => champ VilleBorne
transactionsPerMoisPerBorne2018 <- mutate(transactionsPerMoisPerBorne2018,VilleBorne=paste(str_sub(Ville,1,-5),Borne))

######################################################################################################
# CALCUL DE LA MOYENNE PAR BORNE PAR MOIS sur l'annee 2018 (mais il n y a pas toutes les bornes)
moyennePerMois2018 <- transactionsPerMoisPerBorne2018 %>% group_by(MonthDebut) %>% summarise(meanConso=mean(Consommationkwh),
                                                                                           meanCharge=mean(DureeChargemin),
                                                                                           meanTrans=mean(nbTransaction))

#################################################################################################
# Consolidation heure et VISU HEURE
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

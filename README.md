# UC2Public
Smart Charging Prototype public
Pour fonctionner, ce site shiny doit avoir 2 fichiers de données :
- La description des bornes de recharge : bornesWithRuralite.csv avec les champs suivants :
Station;Commune;Adresse;Identifiant;Type;Numero;CodeInsee;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;lat;lon;Ruralite
- Les transactions de rechargement : trans.csv
Chaque transaction est décrite avec les champs suivants
Borne;Type;Ville;DateDebut;HeureDebut;DureeChargemin;Consommationkwh;DateFin;HeureFin;TypeClient;Status;DateTimeDebut;DateTimeFin

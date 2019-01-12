#----librairies----

library(here)
here()

library(tidyverse)
library(reshape)
library(utf8)

#Cartographie
library(sf)
#library(SpatialPosition)
library(cartography)

#----chargement des donn�es----
#Cartes
pathAE <- "./ADMIN-EXPRESS-COG_1-0__SHP__FRA_2017-06-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2017-06-19/ADE-COG_1-0_SHP_LAMB93_FR"
comn <- st_read(dsn = pathAE, layer="COMMUNE", stringsAsFactors=FALSE)
dept <- st_read(dsn = pathAE, layer="DEPARTEMENT", stringsAsFactors=FALSE)

#Donnes 
insee <- read.csv2(file="./base-cc-emploi-pop-active-2015.csv")
insee2010 <- read.csv2(file="./base-cc-emploi-pop-active-2010.csv")
ze <- read.csv2(file="./ze.csv")
etud <- read_csv2(file="./fr-esr-atlas_regional-effectifs-d-etudiants-inscrits.csv" )



#----Preparation des donnees----

#correction sur etud --> conversion geo_id
etud$geo_id_copy <- etud$geo_id 
etud$geo_id <- as.character(etud$geo_id)
etud$geo_id <- str_pad(etud$geo_id,5,pad="0")
etud$geo_id <- as.factor(etud$geo_id)

#correction sur etud --> correction de la variable effectif (pb d'import � cause du ".")
etud$effectif <- etud$effectif / 10 

#correction de CODGEP sur ze
ze$CODGEO_copy <- ze$CODGEO 
ze$CODGEO <- as.character(ze$CODGEO)
ze$CODGEO <- str_pad(ze$CODGEO,5,pad="0")
ze$CODGEO <- as.factor(ze$CODGEO)

ze$ZE2010 <- as.factor(ze$ZE2010)

#correction de CODGEP sur insee
insee$CODGEO_copy <- insee$CODGEO 
insee$CODGEO <- as.character(insee$CODGEO)
insee$CODGEO <- str_pad(insee$CODGEO,5,pad="0")
insee$CODGEO <- as.factor(insee$CODGEO)

#fusion carto, insee et ze
comn2 <- left_join(comn, ze, by=c("INSEE_COM"="CODGEO"))
comn2 <- left_join(comn2, insee, by = c("INSEE_COM"="CODGEO"))
comn2 <- left_join(comn2, insee2010, by = c("INSEE_COM"="CODGEO"))
#calcul du taux de chomage

comn2$chomage <- comn2$P15_CHOM1564 / comn2$P15_ACT1564 * 100
comn2$VARP_POP1564 <- ((comn2$P15_POP1564 - comn2$P10_POP1564) / comn2$P10_POP1564 )*100
comn2$VARP_POP1524 <- ((comn2$P15_POP1524 - comn2$P10_POP1524) / comn2$P10_POP1524 )*100
comn2$VARP_POP2554 <- ((comn2$P15_POP2554 - comn2$P10_POP2554) / comn2$P10_POP2554 )*100
comn2$VARP_POP5564 <- ((comn2$P15_POP5564 - comn2$P10_POP5564) / comn2$P10_POP5564 )*100

comn2$VAR_POP1564 <- (comn2$P15_POP1564 - comn2$P10_POP1564) 
comn2$VAR_POP1524 <- (comn2$P15_POP1524 - comn2$P10_POP1524)
comn2$VAR_POP2554 <- (comn2$P15_POP2554 - comn2$P10_POP2554)
comn2$VAR_POP5564 <- (comn2$P15_POP5564 - comn2$P10_POP5564) 

#----filtres sur les donn�es carto----
#code ZE2010 : chaumont --> 2106 ; vitry le francois --> 2107
#code ZE2010 : Gap --> 9304 (probleme sur les donn�es insee)
#code ZE2010 : Aurillac --> 8304 

code_ze <- c(8304)
path_ze <- "./Aurillac/"
name_ze <- "le bassin d'Aurillac"

code_ze <- c(2106,2107)
path_ze <- "./Chaumont-VLF/"
name_ze <- "les bassins de Chaumont et Vitry le Fran�ois"

code_ze <- c(5304)
path_ze <- "./Loudeac/"
name_ze <- "le bassin de Loudeac"

code_ze <- c(5207)
path_ze <- "./Saumur/"
name_ze <- "le bassin de Saumur"

code_ze <- c(2304)
path_ze <- "./Vernon/"
name_ze <- "le bassin de Vernon"

code_ze <- c(9304)
path_ze <- "./Gap/"
name_ze <- "le bassin de Gap"


if(length(code_ze)==1){
  ze_1 <- comn2 %>% filter(ZE2010 == code_ze[1])
  ze_merge <- comn2 %>% filter(ZE2010 == code_ze[1])
} else if(length(code_ze)==2) {
  ze_1 <- comn2 %>% filter(ZE2010 == code_ze[1])
  ze_2 <- comn2 %>% filter(ZE2010 == code_ze[2])
  ze_merge <- rbind(ze_1, ze_2)
} else {}

#----Contours des zones d'emplois----

if(length(code_ze)==1){
  ze_1_cont <- st_union(ze_1)
  ze_1_cont <- as(ze_1_cont,"Spatial")
  ze_merge_cont <- as(ze_1_cont,"Spatial")
} else if(length(code_ze)==2) {
  ze_1_cont <- st_union(ze_1)
  ze_1_cont <- as(ze_1_cont,"Spatial")
  ze_2_cont <- st_union(ze_2)
  ze_2_cont <- as(ze_2_cont,"Spatial")
  ze_merge_cont <- st_union(ze_merge)
  ze_merge_cont <- as(ze_merge_cont,"Spatial")
} else {}

#----Tests----
t_comn2 <- comn2 %>% filter(ZE2010 == code_ze[1])

t_comn2 <- comn2 %>% filter(NOM_COM == "Gap")
t_ze <- ze %>% filter(LIBGEO == "Gap")


t_insee_2010 <- insee2010 %>% filter(LIBGEO == "Gap")
t_insee <- insee %>% filter(LIBGEO == "Gap")


#----preparation etudiants----


PrepToCarto <- function(data_etud, regroup){
  etud2 <- left_join(data_etud, ze, by = c("geo_id"="CODGEO"))
  if(length(code_ze)==1){
    etud_ze_1_2 <-etud2 %>% filter(ZE2010 == code_ze[1]) # 1ze
  } else if(length(code_ze)==2) {
    etud_ze_1 <- etud2 %>% filter(ZE2010 == code_ze[1]) #2ze
    etud_ze_2 <- etud2 %>% filter(ZE2010 == code_ze[2]) #2ze
    etud_ze_1_2 <- rbind(etud_ze_1, etud_ze_2) #2ze
  } else {}
  etud_ze_1_2_C <- etud_ze_1_2 %>% filter(regroupement != regroup) 
  etud_ze_1_2_C <- etud_ze_1_2_C %>% filter(niveau_geographique == "Commune")
  etud_ze_1_2_D <- etud_ze_1_2_C %>% dplyr::select(rentree, geo_nom, geo_id, 
                                                   ZE2010, secteur, sexe_de_l_etudiant, 
                                                   effectif)
  etud_ze_1_2_D[is.na(etud_ze_1_2_D)] <- 0
  etud_ze_1_2_E <- cast(etud_ze_1_2_D, rentree + geo_nom + ZE2010 + geo_id ~ . , sum)
  etud_ze_1_2_E <- etud_ze_1_2_E %>% dplyr::rename( effectif = '(all)') 
  etud_ze_1_2_F <- cast(etud_ze_1_2_E, geo_nom + ZE2010 + geo_id ~ rentree )
  etud_ze_1_2_F <- etud_ze_1_2_F %>% dplyr::rename( eff_2016 = '2016') %>% 
    dplyr::rename( eff_2001 = '2001')
  comn_etud_ze_1_2 <- left_join(comn, etud_ze_1_2_F, by=c("INSEE_COM"="geo_id"))
  comn_etud_ze_1_2$var_2001_2016 <- comn_etud_ze_1_2$eff_2016 - comn_etud_ze_1_2$eff_2001
  comn_etud_ze_1_2 <- comn_etud_ze_1_2 %>% filter(!is.na(var_2001_2016))
}

comn_etud_ze_carte_2 <- PrepToCarto(etud, "TOTAL")

PrepToGraph <- function(data_etud, regroup){
  etud2 <- left_join(data_etud, ze, by = c("geo_id"="CODGEO"))
  etud2$effectif <- as.numeric(etud2$effectif)
  if(length(code_ze)==1){
    etud_ze_1_2 <-etud2 %>% filter(ZE2010 == code_ze[1]) # 1ze
  } else if(length(code_ze)==2) {
    etud_ze_1 <- etud2 %>% filter(ZE2010 == code_ze[1]) #2ze
    etud_ze_2 <- etud2 %>% filter(ZE2010 == code_ze[2]) #2ze
    etud_ze_1_2 <- rbind(etud_ze_1, etud_ze_2) #2ze
  } else {}
  etud_ze_1_2_C <- etud_ze_1_2 %>% filter(regroupement != regroup) 
  etud_ze_1_2_C <- etud_ze_1_2_C %>% filter(niveau_geographique == "Commune")
  etud_ze_1_2_D <- etud_ze_1_2_C %>% dplyr::select(rentree, rgp_formations_ou_etablissements, 
                                             sexe_de_l_etudiant, 
                                             effectif)
  etud_ze_1_2_D[is.na(etud_ze_1_2_D)] <- 0 
  etud_ze_1_2_E <- cast(etud_ze_1_2_D, rentree + rgp_formations_ou_etablissements ~ . , sum)
  etud_ze_1_2_E <- etud_ze_1_2_E %>% dplyr::rename( effectif = '(all)') 
  return(etud_ze_1_2_E)
}

comn_etab_graph_2 <- PrepToGraph(etud, "TOTAL")




PrepToGraph2 <- function(data_etud, regroup){
  etud2 <- left_join(data_etud, ze, by = c("geo_id"="CODGEO"))
  etud2$effectif <- as.numeric(etud2$effectif)
  if(length(code_ze)==1){
    etud_ze_1_2 <-etud2 %>% filter(ZE2010 == code_ze[1]) # 1ze
  } else if(length(code_ze)==2) {
    etud_ze_1 <- etud2 %>% filter(ZE2010 == code_ze[1]) #2ze
    etud_ze_2 <- etud2 %>% filter(ZE2010 == code_ze[2]) #2ze
    etud_ze_1_2 <- rbind(etud_ze_1, etud_ze_2) #2ze
  } else {}
  etud_ze_1_2_C <- etud_ze_1_2 %>% filter(regroupement != regroup) 
  etud_ze_1_2_C <- etud_ze_1_2_C %>% filter(niveau_geographique == "Commune")
  etud_ze_1_2_D <- etud_ze_1_2_C %>% dplyr::select(rentree, rgp_formations_ou_etablissements, 
                                             sexe_de_l_etudiant, geo_nom, geo_id,
                                             effectif)
  etud_ze_1_2_D[is.na(etud_ze_1_2_D)] <- 0 
  etud_ze_1_2_E <- cast(etud_ze_1_2_D, rentree + rgp_formations_ou_etablissements + geo_id + geo_nom ~ . , sum)
  etud_ze_1_2_E <- etud_ze_1_2_E %>% dplyr::rename( effectif = '(all)') 
  return(etud_ze_1_2_E)
}

comn_etab_ze_graphe_1 <- PrepToGraph2(etud, "TOTAL")



PrepToGraph3 <- function(data_etud, regroup){
  etud2 <- left_join(data_etud, ze, by = c("geo_id"="CODGEO"))
  etud2$effectif <- as.numeric(etud2$effectif)
  if(length(code_ze)==1){
    etud_ze_1_2 <-etud2 %>% filter(ZE2010 == code_ze[1]) # 1ze
  } else if(length(code_ze)==2) {
    etud_ze_1 <- etud2 %>% filter(ZE2010 == code_ze[1]) #2ze
    etud_ze_2 <- etud2 %>% filter(ZE2010 == code_ze[2]) #2ze
    etud_ze_1_2 <- rbind(etud_ze_1, etud_ze_2) #2ze
  } else {}
  etud_ze_1_2_C <- etud_ze_1_2 %>% filter(regroupement == regroup) 
  etud_ze_1_2_C <- etud_ze_1_2_C %>% filter(niveau_geographique == "Commune")
  etud_ze_1_2_D <- etud_ze_1_2_C %>% dplyr::select(rentree, rgp_formations_ou_etablissements, 
                                                   sexe_de_l_etudiant, geo_nom, geo_id,
                                                   secteur, effectif)
  etud_ze_1_2_D[is.na(etud_ze_1_2_D)] <- 0 
  etud_ze_1_2_E <- cast(etud_ze_1_2_D, rentree + geo_nom ~ ., sum)
  etud_ze_1_2_E <- etud_ze_1_2_E %>% dplyr::rename( effectif = '(all)') 
  return(etud_ze_1_2_E)
}

comn_etab_ze_graphe_3 <- PrepToGraph3(etud, "TOTAL")


PrepToCarto2 <- function(data_etud, regroup){
  etud2 <- left_join(data_etud, ze, by = c("geo_id"="CODGEO"))
  etud2$effectif <- as.numeric(etud2$effectif)
  if(length(code_ze)==1){
    etud_ze_1_2 <-etud2 %>% filter(ZE2010 == code_ze[1]) # 1ze
  } else if(length(code_ze)==2) {
    etud_ze_1 <- etud2 %>% filter(ZE2010 == code_ze[1]) #2ze
    etud_ze_2 <- etud2 %>% filter(ZE2010 == code_ze[2]) #2ze
    etud_ze_1_2 <- rbind(etud_ze_1, etud_ze_2) #2ze
  } else {}
  etud_ze_1_2_C <- etud_ze_1_2 %>% filter(regroupement != regroup) 
  etud_ze_1_2_C <- etud_ze_1_2_C %>% filter(niveau_geographique == "Commune")
  etud_ze_1_2_D <- etud_ze_1_2_C %>% dplyr::select(rentree, rgp_formations_ou_etablissements, 
                                             sexe_de_l_etudiant, geo_id, geo_nom,
                                             effectif)
  etud_ze_1_2_E <- cast(etud_ze_1_2_D, rentree + rgp_formations_ou_etablissements + geo_id + geo_nom ~ . , sum)
  etud_ze_1_2_E <- etud_ze_1_2_E %>% dplyr::rename( effectif = '(all)') 
  etud_ze_1_2_F <- cast(etud_ze_1_2_E, geo_nom + geo_id + rgp_formations_ou_etablissements  ~  rentree )
  etud_ze_1_2_F <- etud_ze_1_2_F %>% dplyr::rename( eff_2016 = '2016') %>% 
    dplyr::rename( eff_2001 = '2001')
  #remplacer les NA par 0
  etud_ze_1_2_F[is.na(etud_ze_1_2_F)] <- 0 
  comn_etud_ze_1_2 <- left_join(comn, etud_ze_1_2_F, by=c("INSEE_COM"="geo_id"))
  comn_etud_ze_1_2$var_2001_2016 <- comn_etud_ze_1_2$eff_2016 - comn_etud_ze_1_2$eff_2001
  comn_etud_ze_1_2$var_2001_2016
  comn_etud_ze_1_2 <- comn_etud_ze_1_2 %>% filter(!is.na(var_2001_2016))
  return(comn_etud_ze_1_2) 
}
comn_etud_carte_1 <- PrepToCarto2(etud, "TOTAL")

sum(comn_etud_carte_1$eff_2016)
sum(comn_etud_ze_carte_2$eff_2016)

#-----graphique 1 - type etablissments crois� commune----
dev.off() 

#parametres export de la carte
hauteur <- 2500 * length(unique(comn_etab_ze_graphe_1$geo_nom))
largeur <- 2000 * length(unique(comn_etab_ze_graphe_1$rgp_formations_ou_etablissements))
resolution = (hauteur + largeur) / 18
  
png(paste(path_ze,"graph_etablissements_commune.png",sep=""),  
    width= largeur , 
    height= hauteur ,
    res=resolution)

p1_titre <- paste("Evolution du nombre d'�tudiants par typologie d'�tablissement\nsur ",name_ze)

#graphique
p1 <- ggplot(data=comn_etab_ze_graphe_1, aes(y=(effectif), x=rentree, 
                                      group=rgp_formations_ou_etablissements, 
                                      colour=rgp_formations_ou_etablissements ) )  
p1 <- p1 +geom_line(size=1)+ facet_grid(geo_nom ~ rgp_formations_ou_etablissements,
                                        labeller = label_wrap_gen(width=20))
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + ggtitle(p1_titre)
p1 <- p1 + xlab("Ann�e de la rentr�e") + ylab("Nombre d'�tudiants")
p1 <- p1 + labs(caption = "Source: syst�mes d'information et enqu�tes du minist�re de l'�ducation nationale, \nde l'Enseignement sup�rieur et de la Recherche, \ndes minist�res en charge de l'Agriculture, \nde la P�che, de la Culture, de la Sant� et des Sports.")
p1

dev.off() 

#-----graphique 2 - type etablissments----
dev.off() 
#parametres export de la carte
hauteur <- 2500
largeur <- 1300 * length(unique(comn_etab_ze_graphe_1$rgp_formations_ou_etablissements))
resolution = (hauteur + largeur) / 12

png(paste(path_ze,"graph_etablissements.png",sep=""),  
    width= largeur , 
    height= hauteur ,
    res=resolution)

p1_titre <- paste("Evolution du nombre d'�tudiants par typologie d'�tablissement\nsur ",name_ze)

#graphique
p1 <- ggplot(data=comn_etab_graph_2, aes(y=(effectif), x=rentree, 
                                      group=rgp_formations_ou_etablissements, 
                                      colour=rgp_formations_ou_etablissements ) )  
p1 <- p1 +geom_line(size=1)+ facet_wrap(~ rgp_formations_ou_etablissements,
                                        labeller = label_wrap_gen(width=20),
                                        nrow=1)
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + ggtitle(p1_titre)
p1 <- p1 + xlab("Ann�e de la rentr�e") + ylab("Nombre d'�tudiants")
p1 <- p1 + labs(caption = "Source: syst�mes d'information et enqu�tes du minist�re de l'�ducation nationale, \nde l'Enseignement sup�rieur et de la Recherche, \ndes minist�res en charge de l'Agriculture, \nde la P�che, de la Culture, de la Sant� et des Sports.")
p1

dev.off() 

#-----graphique 3 - etudiants----
dev.off() 
#parametres export de la carte
hauteur <- 2500 
largeur <- 600 + (1300 * length(unique(comn_etab_ze_graphe_3$geo_nom)))
resolution = (hauteur + largeur) / 12

png(paste(path_ze,"graph_etudiants.png",sep=""),  
    width= largeur , 
    height= hauteur ,
    res=resolution)

p1_titre <- paste("Evolution du nombre d'�tudiants par Commune\nsur ",name_ze)


#graphique
p1 <- ggplot(data=comn_etab_ze_graphe_3, aes(y=(effectif), x=rentree, group=geo_nom, colour=geo_nom ) )  
p1 <- p1 +  geom_line(size=1)+ facet_wrap(~ geo_nom,
                                          labeller = label_wrap_gen(width=20),
                                          nrow=1)
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + ggtitle(p1_titre)
p1 <- p1 + xlab("Ann�e de la rentr�e") + ylab("Nombre d'�tudiants")
p1 <- p1 + labs(caption = "Source: syst�mes d'information et enqu�tes du minist�re de l'�ducation nationale, \nde l'Enseignement sup�rieur et de la Recherche, \ndes minist�res en charge de l'Agriculture, \nde la P�che, de la Culture, de la Sant� et des Sports.")
p1

dev.off() 

#------carte 2 - etudiants----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_etudiants.png",sep=""),  width= 3600 , height= 3000 ,res=550)

#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des d�partements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                               ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)

#plot tot de chomages et nombres d'actifs
propSymbolsChoroLayer(comn_etud_ze_carte_2, var = "eff_2016", var2 = "var_2001_2016", 
                      method = "quantile", nclass = 5, 
                      #breaks = c(0,5,7.5,10,12.5,15,20,50),
                      border = "white", lwd = 0.5, 
                      col = carto.pal(pal1="red.pal", n1 = 3 , pal2 = "green.pal", n2 = 2),
                      legend.var.pos = "topleft", 
                      legend.var.title.txt = "Nombre d'�tudiant\nen 2016", legend.var2.pos = "left", 
                      legend.var2.title.txt = "Variation du nombre\nd'�tudiants entre\n2001 et 2016", 
                      legend.var2.values.rnd = 3)

#plot villes et noms de villes importantes
plot(st_geometry(st_centroid(comn_etud_ze_carte_2)), pch = 20, cex = 1.5, add=TRUE)
labelLayer(filter(comn_etud_ze_carte_2, INSEE_COM != 52125), top10@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = 0.5, pos = 2, font = 4, offset = 0.2)
labelLayer(filter(comn_etud_ze_carte_2, INSEE_COM == 52125), top10@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = 0.5, 
           pos = 4, font = 4, offset = 0.2)

# Ajout de l'habillage
layoutLayer(title = "Variation du nombre d'�tudiants entre 2001 et 2016", 
            sources = "Source: syst�mes d'information et enqu�tes du minist�re de l'�ducation nationale, \nde l'Enseignement sup�rieur et de la Recherche, \ndes minist�res en charge de l'Agriculture, \nde la P�che, de la Culture, de la Sant� et des Sports.", 
            author = "Carte R�alis�e avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 




#------carte 3 - chomage----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_chomage.png",sep=""),  width= 3600 , height= 3000 ,res=550)
C3_titre <- paste("R�partition du ch�mage sur ",name_ze)
#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des d�partements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                 ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
if(length(code_ze)==1){
  plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
} else if(length(code_ze)==2) {
  plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
  plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
} else {}

#plot tot de chomages et nombres d'actifs
propSymbolsChoroLayer(ze_merge, var = "P15_ACT1564", var2 = "chomage", 
                      method = "equal", nclass = 5, 
                      #breaks = c(0,5,7.5,10,12.5,15,20,50),
                      border = "white", lwd = 0.5, 
                      col = carto.pal("red.pal", 7), legend.var.pos = "topleft", 
                      legend.var.title.txt = "Pop. active", legend.var2.pos = "left", 
                      legend.var2.title.txt = "Tx de ch�mage\n(en %)", 
                      legend.var2.values.rnd = 3)


#plot villes et noms de villes importantes
top <- ze_merge[order(ze_merge$P15_ACT1564, decreasing = T), 
             ][1:5, ]
plot(st_geometry(st_centroid(top)), pch = 20, cex = 1.5, add=TRUE)
labelLayer(top, top@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = c(0.9, 
                                                        0.7, rep(0.5, 8)), pos = 2, font = 4, offset = 0.2)

# Ajout de l'habillage
layoutLayer(title = C3_titre, 
            sources = "INSEE : recensement 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte R�alis�e avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 


#------carte 4 - emploi----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_emplois.png",sep=""),  width= 3600 , height= 3000 ,res=550)
C3_titre <- paste("R�partition des emplois au lieu de travail sur ",name_ze)

#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des d�partements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                               ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
if(length(code_ze)==1){
  plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
} else if(length(code_ze)==2) {
  plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
  plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
} else {}

#plot tot de chomages et nombres d'actifs
propSymbolsChoroLayer(ze_merge, var = "P15_EMPLT", var2 = "P15_EMPLT", 
                      method = "equal", nclass = 5, 
                      #breaks = c(0,2500,5000,7500,10000,15482),
                      border = "white", lwd = 0.5, 
                      col = carto.pal("green.pal", 5), legend.var.pos = "topleft", 
                      legend.var.title.txt = "Nombre d'emplois\nau lieu de travail", 
                      legend.var2.pos = "left", 
                      legend.var2.title.txt = "Nombre d'emplois\nau lieu de travail", 
                      legend.var2.values.rnd = 3)

#plot villes et noms de villes importantes
top <- ze_merge[order(ze_merge$P15_ACT1564, decreasing = T), 
                 ][1:5, ]
plot(st_geometry(st_centroid(top)), pch = 20, cex = 1.5, add=TRUE)
labelLayer(top, top@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = c(0.9, 
                                                        0.7, rep(0.5, 8)), pos = 2, font = 4, offset = 0.2)

# Ajout de l'habillage
layoutLayer(title = C3_titre, 
            sources = "INSEE : recensement 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte R�alis�e avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 
#------carte 5 - demographie----

popC5 <- c("population ag�e de 15 � 64 ans",
           "population ag�e de 15 � 24 ans",
           "population ag�e de 25 � 54 ans")
popLC5 <- c("Pop. 15-64 ans\nen2015",
            "Pop. 15-24 ans\nen2015",
            "Pop. 25-54 ans\nen2015")
popL2C5 <- c("Variation pop 15-64\n entre 2010 et 2015",
            "Variation pop 15-24\n entre 2010 et 2015",
            "Variation pop 25-54\n entre 2010 et 2015")
zoneC5 <- name_ze
var1C5 <- c("P15_POP1564","P15_POP1524","P15_POP2554")
var2C5 <- c("VAR_POP1564","VAR_POP1524","VAR_POP2554")


dev.off() 

i<-1
for(i in 1:3){
  #titre
  titreC5 <- paste("R�partition (2015) et variation (entre 2010 et 2015)\nde la ", popC5[i], " sur", zoneC5)
  
  #export de la carte
  png(paste(path_ze,"carte_demographie_",popC5[i],".png",sep=""),  width= 3600 , height= 3000 ,res=550)
  
  #marges
  opar <- par(mfrow = c(1,1), mar = c(0,0.1,1.6,0.1))

  # Affichage des d�partements
  plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
       xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2,
                                                                   ])
  #Affichage de limites des communes
  plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
       add = TRUE)
  
  #limites des ZE
  if(length(code_ze)==1){
    plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
  } else if(length(code_ze)==2) {
    plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
    plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
  } else {}
  
  #varaiable
  propSymbolsChoroLayer(ze_merge, var = var1C5[i], var2 = var2C5[i],
                   method = "q6", 
                   col = carto.pal(pal1="red.pal", n1 = 3 , pal2 = "green.pal", n2 = 3),
                   border = "white", lwd = 0.5, nclass = 5,
                   legend.var.pos = "topleft", legend.var2.pos = "left",
                   legend.var.title.txt = popLC5[i], legend.var2.title.txt = popL2C5[i])
  
  #plot villes et noms de villes importantes
  top <- ze_merge[order(ze_merge$P15_POP1564, decreasing = T), 
                  ][1:5, ]
  plot(st_geometry(st_centroid(top)), pch = 20, cex = 1.5, add=TRUE)
  labelLayer(top, top@data, spdfid = "INSEE_COM", 
             dfid = "INSEE_COM", txt = var2C5[i], cex = c(0.9, 
                                                              0.7, rep(0.5, 8)), pos = 4, font = 4, offset = 0.2)
  labelLayer(top, top@data, spdfid = "INSEE_COM", 
             dfid = "INSEE_COM", txt = "NOM_COM", cex = c(0.9, 
                                                          0.7, rep(0.5, 8)), pos = 2, font = 4, offset = 0.3)
  
  # Ajout de l'habillage
  layoutLayer(title = titreC5,
              sources = "INSEE : recensements 2010 et 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte R�alis�e avec les librairies : SF et Cartography", 
              scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
              coltitle = "black")
  
  dev.off() 
}



#------Carte 5 - test


## -- 1ere carte : 2010
# 
# titreC5 <- paste("R�partition de la ", popC5, " dans les", zoneC5, " ",varC5[2])
# # Affichage des d�partements
# plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
#      xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
#                                                                  ])
# #Affichage de limites des communes
# plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
#      add = TRUE)
# 
# #limites des ZE
# plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
# plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
# 
# summary(ze_merge$P15_POP1564)
# Brk <- c(0,250,500,1000,2500,5000,7500,10000,12500,15000,15710)
# 
# #varaiable
# propSymbolsLayer(ze_merge, var = "P10_POP1564",
#                  #method = "quantile", nclass = 10, 
#                  #breaks = Brk,col = carto.pal("blue.pal", 10)
#                  border = "white", lwd = 0.5, 
#                  col = "#BF2A3B", legend.pos = "topleft", 
#                  legend.title.txt = popLC5)
# 
# #plot villes et noms de villes importantes
# top <- ze_merge[order(ze_merge$P10_POP1564, decreasing = T), 
#                 ][1:5, ]
# plot(st_geometry(st_centroid(top)), pch = 20, cex = 1.5, add=TRUE)
# labelLayer(top, top@data, spdfid = "INSEE_COM", 
#            dfid = "INSEE_COM", txt = "P10_POP1564", cex = c(0.9, 
#                                                             0.7, rep(0.5, 8)), pos = 4, font = 4, offset = 0.2)
# labelLayer(top, top@data, spdfid = "INSEE_COM", 
#            dfid = "INSEE_COM", txt = "NOM_COM", cex = c(0.9, 
#                                                         0.7, rep(0.5, 8)), pos = 2, font = 4, offset = 0.3)
# 
# # Ajout de l'habillage
# layoutLayer(title = titreC5,
#             sources = "INSEE : recensements 2010 et 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte R�alis�e avec les librairies : SF et Cartography", 
#             scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
#             coltitle = "black")
# 
# 
# 
# 
# ## -- 2eme carte : 2015
# 
# titreC5 <- paste("R�partition de la ", popC5, " dans les", zoneC5, " ",varC5[1])
# # Affichage des d�partements
# plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
#      xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
#                                                                  ])
# #Affichage de limites des communes
# plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
#      add = TRUE)
# 
# #limites des ZE
# plot(ze_2_cont, border="grey15", add = TRUE , lwd = 1.1)
# plot(ze_1_cont, border="grey15", add = TRUE , lwd = 1.1)
# 
# summary(ze_merge$P15_POP1564)
# Brk <- c(0,250,500,1000,2500,5000,7500,10000,12500,15000,15710)
# 
# #varaiable
# propSymbolsLayer(ze_merge, var = "P15_POP1564",
#                  #method = "quantile", nclass = 10, 
#                  #breaks = Brk,col = carto.pal("blue.pal", 10)
#                  border = "white", lwd = 0.5, 
#                  col = "#BF2A3B", legend.pos = "topleft", 
#                  legend.title.txt = popLC5)
# 
# #plot villes et noms de villes importantes
# top <- ze_merge[order(ze_merge$P15_POP1564, decreasing = T), 
#                   ][1:5, ]
# plot(st_geometry(st_centroid(top)), pch = 20, cex = 1.5, add=TRUE)
# labelLayer(top, top@data, spdfid = "INSEE_COM", 
#            dfid = "INSEE_COM", txt = "P15_POP1564", cex = c(0.9, 
#                                                         0.7, rep(0.5, 8)), pos = 4, font = 4, offset = 0.2)
# labelLayer(top, top@data, spdfid = "INSEE_COM", 
#            dfid = "INSEE_COM", txt = "NOM_COM", cex = c(0.9, 
#                                                         0.7, rep(0.5, 8)), pos = 2, font = 4, offset = 0.3)
# 
# # Ajout de l'habillage
# layoutLayer(title = titreC5,
#             sources = "INSEE : recensements 2010 et 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte R�alis�e avec les librairies : SF et Cartography", 
#             scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
#             coltitle = "black")
#
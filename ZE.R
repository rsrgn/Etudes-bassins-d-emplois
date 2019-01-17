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
library(spatialEco)

#----chargement des données----
#Cartes
pathAE <- "./ADMIN-EXPRESS-COG_1-0__SHP__FRA_2017-06-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2017-06-19/ADE-COG_1-0_SHP_LAMB93_FR"
comn <- st_read(dsn = pathAE, layer="COMMUNE", stringsAsFactors=FALSE)
dept <- st_read(dsn = pathAE, layer="DEPARTEMENT", stringsAsFactors=FALSE)

#Donnes 
insee <- read.csv2(file="./base-cc-emploi-pop-active-2015.csv")
insee2010 <- read.csv2(file="./base-cc-emploi-pop-active-2010.csv")
ze <- read.csv2(file="./ze.csv")
etud <- read_csv2(file="./fr-esr-atlas_regional-effectifs-d-etudiants-inscrits.csv" )
ees <- read_csv2(file="./etablissements_denseignement_superieur.csv")
ees2 <- read.csv2(file="./etablissements_denseignement_superieur.csv")
eesec <- read_csv2(file="./etablissements_denseignement_secondaire.csv")
eesec2 <- read.csv2(file="./etablissements_denseignement_secondaire.csv")


#----Preparation des donnees----

#etablissements
ees <- ees %>% dplyr::rename( longitude = `longitude (X)`)
ees <- ees %>% dplyr::rename( latitude = `latitude (Y)`)
eesec <- eesec %>% dplyr::rename( longitude = `longitude (X)`)
eesec <- eesec %>% dplyr::rename( latitude = `latitude (Y)`)
eesec <- eesec %>% dplyr::rename( type_etablissement = `type d'établissement`)


ees$longitude <- as.numeric(as.character(ees2$longitude))
ees$latitude <- as.numeric(as.character(ees2$latitude))
eesec$longitude <- as.numeric(as.character(eesec2$longitude))
eesec$latitude <- as.numeric(as.character(eesec2$latitude))

ees <- ees %>% dplyr::filter(is.na(longitude) == FALSE)
ees <- ees %>% dplyr::filter(région != "La Réunion" | 
                               région != "Guadeloupe" |
                               région != "Guyane" |
                               région != "Collectivités d'Outre Mer" |
                               région != "Martinique")

eesec <- eesec %>% dplyr::filter(is.na(longitude) == FALSE)

s_ees <- st_as_sf(x=ees, coords=c("longitude","latitude"), crs=4326) 
s_ees <- st_transform(s_ees, crs="+proj=lcc +lat_1=44 +lat_2=49 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")

s_eesec <- st_as_sf(x=eesec, coords=c("longitude","latitude"), crs=4326) 
s_eesec <- st_transform(s_eesec, crs="+proj=lcc +lat_1=44 +lat_2=49 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")


#correction sur etud --> conversion geo_id
etud$geo_id_copy <- etud$geo_id 
etud$geo_id <- as.character(etud$geo_id)
etud$geo_id <- str_pad(etud$geo_id,5,pad="0")
etud$geo_id <- as.factor(etud$geo_id)

#correction sur etud --> correction de la variable effectif (pb d'import à cause du ".")
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

#----Selection des ZE----
#code ZE2010 : chaumont --> 2106 ; vitry le francois --> 2107
#code ZE2010 : Gap --> 9304 (probleme sur les données insee)
#code ZE2010 : Aurillac --> 8304 

code_ze <- c(8304)
path_ze <- "./Aurillac/"
name_ze <- "le bassin d'Aurillac"

code_ze <- c(2106,2107)
path_ze <- "./Chaumont-VLF/"
name_ze <- "les bassins de Chaumont et Vitry le François"

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

#----filtres sur les données carto----
for(i in 1:length(code_ze)){
  assign(paste("ze_", i, sep = ""), comn2 %>% filter(ZE2010 == code_ze[i]) )   
}
ze_merge <- ze_1
if(length(code_ze)>1){
  for(i in 2:length(code_ze)){
    ze_merge <- rbind(ze_merge, get(paste("ze_", i, sep = "")))   
  }
} else {}

#----Contours des zones d'emplois----

for(i in 1:length(code_ze)){
  assign(paste("ze_", i, "_cont",sep = ""), st_union(get(paste("ze_", i, sep = ""))) )
  assign(paste("ze_", i, "_cont",sep = ""), as(get(paste("ze_", i, "_cont", sep = "")),"Spatial") )
}
ze_merge_cont <- st_union(ze_merge)
ze_merge_cont <- as(ze_merge_cont,"Spatial")

#----decoupage du fichier etablissements----
s_ees_ze <- point.in.poly(s_ees,ze_merge_cont, sp = FALSE)
s_ees_ze <- s_ees_ze %>% dplyr::filter(is.na(poly.ids) == FALSE)

s_eesec_ze <- point.in.poly(s_eesec,ze_merge_cont, sp = FALSE)
s_eesec_ze <- s_eesec_ze %>% dplyr::filter(is.na(poly.ids) == FALSE)

Label_wrapper <- function(lab_list){
  lab_list$nom2 <- lab_list$nom
  for(i in 1:nrow(lab_list)){
    lab <- strwrap(lab_list$nom[i], prefix="\n", initial="", width = 35, simplify=TRUE)
    lab_merge <- lab[1]
    if(length(lab)>1){
      for(j in 2:length(lab) ){
        lab_merge <- paste(lab_merge,lab[j], sep="")
      }
    } else {}
    lab_list$nom2[i] <- lab_merge
  }
  return(lab_list)
}
s_ees_ze <- Label_wrapper(s_ees_ze)


#----preparation etudiants----


FilterEtudOnZe <- function(df){
  etud2 <- left_join(df, ze, by = c("geo_id"="CODGEO"))
  for(i in 1:length(code_ze)){
    assign(paste("etud_ze_", i, sep = ""),etud2 %>% filter(ZE2010 == code_ze[i]) )   
  }
  etud_ze_1_2 <- etud_ze_1
  if(length(code_ze)>1){
    for(i in 2:length(code_ze)){
      etud_ze_1_2 <- rbind(etud_ze_1_2, get(paste("etud_ze_", i, sep = "")))   
    }
  } else {}
  return(etud_ze_1_2)
}

PrepToCarto <- function(data_etud, regroup){
  etud_ze_1_2 <- FilterEtudOnZe(data_etud)
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
  etud_ze_1_2 <- FilterEtudOnZe(data_etud)
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
  etud_ze_1_2 <- FilterEtudOnZe(data_etud)
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
  etud_ze_1_2 <- FilterEtudOnZe(data_etud)
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
  etud_ze_1_2 <- FilterEtudOnZe(data_etud)
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

#vérifications : si les sommes sont différents il faut investiguer
sum(comn_etud_carte_1$eff_2016)
sum(comn_etud_ze_carte_2$eff_2016)
sum(comn_etab_graph_2 %>% filter(rentree == 2016) %>% dplyr::select(effectif))
sum(comn_etab_ze_graphe_1 %>% filter(rentree == 2016) %>% dplyr::select(effectif))
sum(comn_etab_ze_graphe_3 %>% filter(rentree == 2016) %>% dplyr::select(effectif))


#-----graphique 1 - type etablissments croisé commune----
dev.off() 

#parametres export de la carte
hauteur <- 2500 * length(unique(comn_etab_ze_graphe_1$geo_nom))
largeur <- 2000 * length(unique(comn_etab_ze_graphe_1$rgp_formations_ou_etablissements))
resolution = (hauteur + largeur) / 18
  
png(paste(path_ze,"graph_etablissements_commune.png",sep=""),  
    width= largeur , 
    height= hauteur ,
    res=resolution)

p1_titre <- paste("Evolution du nombre d'étudiants par typologie d'établissement\nsur ",name_ze)

#graphique
p1 <- ggplot(data=comn_etab_ze_graphe_1, aes(y=(effectif), x=rentree, 
                                      group=rgp_formations_ou_etablissements, 
                                      colour=rgp_formations_ou_etablissements ) )  
p1 <- p1 +geom_line(size=1)+ facet_grid(geo_nom ~ rgp_formations_ou_etablissements,
                                        labeller = label_wrap_gen(width=20))
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + ggtitle(p1_titre)
p1 <- p1 + xlab("Année de la rentrée") + ylab("Nombre d'étudiants")
p1 <- p1 + labs(caption = "Source: systèmes d'information et enquêtes du ministère de l'Éducation nationale, \nde l'Enseignement supérieur et de la Recherche, \ndes ministères en charge de l'Agriculture, \nde la Pêche, de la Culture, de la Santé et des Sports.")
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

p1_titre <- paste("Evolution du nombre d'étudiants par typologie d'établissement\nsur ",name_ze)

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
p1 <- p1 + xlab("Année de la rentrée") + ylab("Nombre d'étudiants")
p1 <- p1 + labs(caption = "Source: systèmes d'information et enquêtes du ministère de l'Éducation nationale, \nde l'Enseignement supérieur et de la Recherche, \ndes ministères en charge de l'Agriculture, \nde la Pêche, de la Culture, de la Santé et des Sports.")
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

p1_titre <- paste("Evolution du nombre d'étudiants par Commune\nsur ",name_ze)


#graphique
p1 <- ggplot(data=comn_etab_ze_graphe_3, aes(y=(effectif), x=rentree, group=geo_nom, colour=geo_nom ) )  
p1 <- p1 +  geom_line(size=1)+ facet_wrap(~ geo_nom,
                                          labeller = label_wrap_gen(width=20),
                                          nrow=1)
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + ggtitle(p1_titre)
p1 <- p1 + xlab("Année de la rentrée") + ylab("Nombre d'étudiants")
p1 <- p1 + labs(caption = "Source: systèmes d'information et enquêtes du ministère de l'Éducation nationale, \nde l'Enseignement supérieur et de la Recherche, \ndes ministères en charge de l'Agriculture, \nde la Pêche, de la Culture, de la Santé et des Sports.")
p1

dev.off() 

#------carte 2 - etudiants----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_etudiants.png",sep=""),  width= 3600 , height= 3000 ,res=550)

#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des départements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                               ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
for(i in 1:length(code_ze)){
  plot(get(paste("ze_", i,"_cont", sep = "")), 
       border="grey15", add = TRUE , lwd = 1.1)
}

#plot tot de chomages et nombres d'actifs
propSymbolsChoroLayer(comn_etud_ze_carte_2, var = "eff_2016", var2 = "var_2001_2016", 
                      method = "quantile", nclass = 5, 
                      #breaks = c(0,5,7.5,10,12.5,15,20,50),
                      border = "white", lwd = 0.5, 
                      col = carto.pal(pal1="red.pal", n1 = 3 , pal2 = "green.pal", n2 = 2),
                      legend.var.pos = "topleft", 
                      legend.var.title.txt = "Nombre d'étudiant\nen 2016", legend.var2.pos = "left", 
                      legend.var2.title.txt = "Variation du nombre\nd'étudiants entre\n2001 et 2016", 
                      legend.var2.values.rnd = 3)

#plot villes et noms de villes importantes
plot(st_geometry(st_centroid(comn_etud_ze_carte_2)), pch = 20, cex = 1.5, add=TRUE)
labelLayer(filter(comn_etud_ze_carte_2, INSEE_COM != 52125), top10@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = 0.5, pos = 2, font = 4, offset = 0.2)
labelLayer(filter(comn_etud_ze_carte_2, INSEE_COM == 52125), top10@data, spdfid = "INSEE_COM", 
           dfid = "INSEE_COM", txt = "NOM_COM", cex = 0.5, 
           pos = 4, font = 4, offset = 0.2)

# Ajout de l'habillage
layoutLayer(title = "Variation du nombre d'étudiants entre 2001 et 2016", 
            sources = "Source: systèmes d'information et enquêtes du ministère de l'Éducation nationale, \nde l'Enseignement supérieur et de la Recherche, \ndes ministères en charge de l'Agriculture, \nde la Pêche, de la Culture, de la Santé et des Sports.", 
            author = "Carte Réalisée avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 




#------carte 3 - chomage----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_chomage.png",sep=""),  width= 3600 , height= 3000 ,res=550)
C3_titre <- paste("Répartition du chômage sur ",name_ze)
#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des départements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                 ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
for(i in 1:length(code_ze)){
  plot(get(paste("ze_", i,"_cont", sep = "")), 
       border="grey15", add = TRUE , lwd = 1.1)
}

#plot tot de chomages et nombres d'actifs
propSymbolsChoroLayer(ze_merge, var = "P15_ACT1564", var2 = "chomage", 
                      method = "equal", nclass = 5, 
                      #breaks = c(0,5,7.5,10,12.5,15,20,50),
                      border = "white", lwd = 0.5, 
                      col = carto.pal("red.pal", 7), legend.var.pos = "topleft", 
                      legend.var.title.txt = "Pop. active", legend.var2.pos = "left", 
                      legend.var2.title.txt = "Tx de chômage\n(en %)", 
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
            sources = "INSEE : recensement 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte Réalisée avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 


#------carte 4 - emploi----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_emplois.png",sep=""),  width= 3600 , height= 3000 ,res=550)
C3_titre <- paste("Répartition des emplois au lieu de travail sur ",name_ze)

#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des départements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2, 
                                                               ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
     add = TRUE)

#limites des ZE
for(i in 1:length(code_ze)){
  plot(get(paste("ze_", i,"_cont", sep = "")), 
       border="grey15", add = TRUE , lwd = 1.1)
}

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
            sources = "INSEE : recensement 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte Réalisée avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 
#------carte 5 - demographie----

popC5 <- c("population agée de 15 à 64 ans",
           "population agée de 15 à 24 ans",
           "population agée de 25 à 54 ans")
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
  titreC5 <- paste("Répartition (2015) et variation (entre 2010 et 2015)\nde la ", popC5[i], " sur", zoneC5)
  
  #export de la carte
  png(paste(path_ze,"carte_demographie_",popC5[i],".png",sep=""),  width= 3600 , height= 3000 ,res=550)
  
  #Marges
  opar <- par(mfrow = c(1,1), mar = c(0,0.1,1.6,0.1))

  #Affichage des départements
  plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
       xlim = bbox(ze_merge_cont)[1, ], ylim = bbox(ze_merge_cont)[2,
                                                                   ])
  #Affichage de limites des communes
  plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#8A5543", lwd = 0.5, 
       add = TRUE)
  
  #Affichage des limites des ZE
  for(i in 1:length(code_ze)){
    plot(get(paste("ze_", i,"_cont", sep = "")), 
         border="grey15", add = TRUE , lwd = 1.1)
  }
  
  #Affichage des varaiables
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
              sources = "INSEE : recensements 2010 et 2015 et base des bassins d'emplois\nIGN : ADMIN EXPRESS", author = "Carte Réalisée avec les librairies : SF et Cartography", 
              scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
              coltitle = "black")
  
  dev.off() 
}
#------carte 6 - etablissements----

dev.off() 

#export de la carte
png(paste(path_ze,"carte_ees.png",sep=""),  width= 3600 , height= 3000 ,res=550)

#marges
opar <- par(mfrow = c(1,1), mar = c(0,0,1.6,0))

# Affichage des départements
plot(st_geometry(dept), col = "#FAEBD6", border = "grey80", lwd = 2, 
     xlim = bbox(ze_merge_cont)[1, ]*c(0.98,1.02), ylim = bbox(ze_merge_cont)[2, 
                                                                 ])
#Affichage de limites des communes
plot(st_geometry(ze_merge), col = "#F1EEE8", border = "#F1EEE8", lwd = 0.5, 
     add = TRUE)

#limites des ZE
for(i in 1:length(code_ze)){
  plot(get(paste("ze_", i,"_cont", sep = "")), 
       border="grey15", add = TRUE , lwd = 1.1)
}



s_eesec_ze_f <- s_eesec_ze %>% filter(type_etablissement != "Collège")

plot(st_geometry((s_eesec_ze_f)), pch = 18, cex = 0.8, add=TRUE, col="darkblue")
labelLayer(s_eesec_ze_f, txt = "type_etablissement", cex = 0.4, pos = 2, font = 4, 
           offset = 0.2, overlap = FALSE, col="darkblue")

plot(st_geometry((s_ees_ze)), pch = 20, cex = 1.5, add=TRUE, col="darkgreen")
labelLayer(s_ees_ze, txt = "nom2", cex = 0.5, pos = 4, font = 4, 
           offset = 0.4, overlap = FALSE, col="darkgreen")

# Ajout de l'habillage
layoutLayer(title = "Localisation des établissements d'enseignements", 
            sources = "Source : ONISEP", 
            author = "Carte Réalisée avec les librairies : SF et Cartography", 
            scale = 10, south = TRUE, frame = FALSE, col = "#cdd2d4", 
            coltitle = "black")
dev.off() 





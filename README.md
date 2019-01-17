# Etudes-bassins-d-emplois
Production des cartes et graphiques par bassin d'emplois

# Sources de données

## pour la cartographie :
* ADMIN EXPRESS : http://professionnels.ign.fr/adminexpress
* Zones d'emplois : https://www.insee.fr/fr/information/2114596

## données des recensements :

* Emploi au lieu de travail 2015 : https://www.insee.fr/fr/statistiques/3561209?sommaire=3561215
* Emploi au lieu de travail 2010 : https://www.insee.fr/fr/statistiques/2053578?sommaire=2118671

## Effectifs étudiants dans l'enseignement supérieur : 

https://www.data.gouv.fr/fr/datasets/effectifs-d-etudiants-inscrits-dans-les-etablissements-et-les-formations-de-l-enseignement-superieur/#_

## Etablissements d'enseignement supérieur :

* ONISEP : http://opendata.onisep.fr/data/57da952417293/2-etablissements-d-enseignement-superieur.htm

# Librairies : 

## traitement des données
* Tidyverse
* Reshape2 (pour les fonction melt et cast qui permette de restructurer facilement des jeux de données)

## Cartographie
* SF
* Cartography

# A faire :

- [x] permettre la création de carte pour n'importe quelle nombre de zones d'emplois (a ce stade on peut seulement utiliser le code pour 1 ou 2 zones d'emplois sur la même carte). remplacer le if par une boucle for. 
- [ ] ajouter une cartographie des temps de trajets
- [ ] Note de prise en main du code
- [ ] améliorier la génération des échelles sur les cartes, en particulier pour les cas ou il y a moins de 6 valeurs. 
- [ ] ajouter une cartographie des établissements d'enseignements
- [ ] Voir si il est possible d'obtenir des statistiques sur les moteurs de recherches / réseaux sociaux sur les recherches de formation par zone d'emplois
- [ ] Voir si il est possible d'automatiser un positionnements des labels sur les cartes de manière à ce qu'ils ne puissent pas se recouvrir
- [ ] Sur les cartes ajouter un labbel avec le nom de la zone d'emplois pour les cartes comportant plus d'une zone

# Exemple de productions : Chaumont et Vitry le François

## Carte emplois
![Carte emplois](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_emplois.png?raw=true "Carte emploist")

## Carte chomage
![Carte chomage](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_chomage.png?raw=true "Carte chomage")

## Carte pupulation 15-64 ans
![Carte pupulation 15-64 ans](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_demographie_population%20ag%C3%A9e%20de%2015%20%C3%A0%2064%20ans.png?raw=true "Carte pupulation 15-64 ans")

## Carte pupulation 15-24 ans
![Carte pupulation 15-24 ans](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_demographie_population%20ag%C3%A9e%20de%2015%20%C3%A0%2024%20ans.png?raw=true "Carte pupulation 15-24 ans")

## Carte pupulation 25-54 ans
![Carte pupulation 25-54 ans](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_demographie_population%20ag%C3%A9e%20de%2025%20%C3%A0%2054%20ans.png?raw=true "Carte pupulation 25-54 ans")

## Carte étudiants
![Carte étudiants](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/carte_etudiants.png?raw=true "Carte étudiants")

## Graphique étudiants par communes
![Graphique étudiants par communes](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/graph_etudiants.png?raw=true "Graphique étudiants par communes")

## Graphique étudiants par fillière
![Graphique étudiants par fillière](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/graph_etablissements_commune.png?raw=true "Graphique étudiants par fillière")

## Graphique étudiants par communes et par fillière
![Graphique étudiants par communes et par fillière](https://github.com/rsrgn/Etudes-bassins-d-emplois/blob/master/Chaumont-VLF/graph_etablissements.png?raw=true "Graphique étudiants par communes et par fillière")

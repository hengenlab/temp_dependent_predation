
# 1_project_to_gpkg.sh

# ---------------------------------------------
# Project shapefile into cylindrical equal area projection and write to GPKG.
#
# HOW TO RUN:
# 1. Open Terminal
# 2. Navigate to this script's directory, or let it auto-set
# 3. Make executable (once): chmod +x 1_project_to_gpkg.sh
# 4. Run: ./1_project_to_gpkg.sh
#
# Output saved to:
#   Richness_Distributions/rangepolygons
# ---------------------------------------------


#!/bin/bash
set -e

# Update Path
path="/Users/jgradym/Desktop/Predation_Data"

# Construct full paths
inpath="${path}/Richness_Distributions/rangepolygons_raw"
polygonpath="${path}/Richness_Distributions/rangepolygons"
temppath="${path}/Richness_Distributions/tmprasters"
outpath="${path}/Richness_Distributions/richnessrasters"
specieslistpath="${path}/Richness_Distributions/marine_spp"
output_csv="${path}/Richness_Distributions/species_counts.csv"

#marine species
ls "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/marine_spp"
marine_birds=$(tail -n +2 "${specieslistpath}/marine_bird_spp.csv" | sed "s/\(.*\)/'\1'/g" | paste -sd, -)
echo $marine_birds
marine_reptiles=$(tail -n +2 "${specieslistpath}/marine_reptiles.csv" | sed "s/\(.*\)/'\1'/g" | paste -sd, -)
echo $marine_reptiles
soricinae=("Sorex" "Blarina" "Cryptotis" "Neomys" "Notiosorex" "Anourosorex" "Chimarrogale" "Nectogale" "Megasorex" "Solisorex")
crocidurinae=("Crocidura" "Diplomesodon" "Feroculus" "Paracrocidura" "Ruwenzorisorex" "Scutisorex" "Solisorex" "Suncus")

#
filters="origin = 1 AND seasonal != 4 AND seasonal != 5 AND presence != 5"
FILTERS="ORIGIN = 1 AND SEASONAL != 4 AND SEASONAL != 5 AND PRESENCE != 5"

ceaproj="+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

#--------- Amphibians --------
# Salamanders
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM CAUDATA WHERE ${filters}" \
  ${polygonpath}/caudata.gpkg ${inpath}/CAUDATA/CAUDATA.shp
  

# Frogs part 1
ogr2ogr -of "GPKG" -nlt "POLYGON" -progress -t_srs "${ceaproj}" \
 -sql "SELECT * FROM ANURA_PART1 WHERE ${filters}" \
 ${polygonpath}/anura1.gpkg ${inpath}/ANURA/ANURA_PART1.shp 

# Frogs part 2
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
 -sql "SELECT * FROM ANURA_PART2 WHERE ${filters}" \
 ${polygonpath}/anura2.gpkg ${inpath}/ANURA/ANURA_PART2.shp 
 
# Caecilians
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM GYMNOPHIONA WHERE ${filters}" \
${polygonpath}/gymnophiona.gpkg ${inpath}/GYMNOPHIONA/GYMNOPHIONA.shp 

#--------Reptiles ---------
# All Reptiles 
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
 -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles})" \
 ${polygonpath}/reptiles.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 

#Squamates
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
 -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group IN ('lizard', 'worm lizard', 'snake')" \
 ${polygonpath}/squamates.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 
   
#Lizard 
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'lizard'" \
 ${polygonpath}/lizard.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 

#Snakes
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
 -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'snake'" \
 ${polygonpath}/snakes.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 

#Turtles 
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'turtle'" \
${polygonpath}/turtles.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 
  
#Crocs
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'croc'" \
${polygonpath}/crocodile.gpkg ${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp 
  


#--------Endotherms ---------
#Birds
ogr2ogr -f "GPKG" -nlt "MULTIPOLYGON"  -t_srs "${ceaproj}" -sql "SELECT * FROM All_Species WHERE SCINAME NOT IN (${marine_birds}) AND ${FILTERS}" \
 ${polygonpath}/birds.gpkg ${inpath}/BOTW_2017-1/BOTW.gdb  

#Mammals
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM MAMMALS_TERRESTRIAL_ONLY WHERE ${filters}" \
${polygonpath}/mammals.gpkg ${inpath}/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp 

#shrews
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM MAMMALS_TERRESTRIAL_ONLY WHERE ${filters} AND family = 'SORICIDAE'" \
${polygonpath}/shrews.gpkg ${inpath}/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp 

# Soricinae
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM MAMMALS_TERRESTRIAL_ONLY WHERE ${filters} AND family = 'SORICIDAE' AND genus IN ('Sorex', 'Blarina', 'Cryptotis', 'Neomys', 'Notiosorex', 'Anourosorex', 'Chimarrogale', 'Nectogale', 'Megasorex', 'Solisorex')" \
  ${polygonpath}/soricinae.gpkg ${inpath}/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp 

# Crocidurinae
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM MAMMALS_TERRESTRIAL_ONLY WHERE ${filters} AND family = 'SORICIDAE' AND genus IN ('Crocidura', 'Diplomesodon', 'Feroculus', 'Paracrocidura', 'Ruwenzorisorex', 'Scutisorex', 'Solisorex', 'Suncus')" \
  ${polygonpath}/crocidurinae.gpkg ${inpath}/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp 

#Birds
ogr2ogr -f "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
-sql "SELECT * FROM All_Species WHERE SCINAME NOT IN (${marine_birds}) AND ${FILTERS}" \
${polygonpath}/birds.gpkg ${inpath}/BOTW_2017-1/BOTW.gdb 
    


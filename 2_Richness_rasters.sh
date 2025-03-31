#!/bin/bash

# This script generates richness rasters from polygon shapefiles for each taxonomic group.
# Steps:
# 1. Project shapefile into cylindrical equal area projection and write to GPKG.
# 2. Get number of features (species) in the shapefile.
# 3. Loop through features, rasterizing each at the desired resolution.
# 4. Sum individual rasters to produce a richness raster.
# 5. Postprocess: sum Anura parts, species counts.

#Prevent from going to sleep
caffeinate -i ./your_script.sh

# Define base path (users should update this if needed)
base_path="/Users/jgradym/Desktop/Predation_Data"

# Define subdirectories relative to base_path
richness_path="${base_path}/Richness_Distributions"
specieslistpath="${richness_path}/marine_spp"
inpath="${richness_path}/rangepolygons_raw"
polygonpath="${richness_path}/rangepolygons"
temppath="${richness_path}/tmprasters"
outpath="${richness_path}/richnessrasters"
output_csv="${richness_path}/species_counts.csv"

# Projection definition
ceaproj="+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Filters
filters="origin = 1 AND seasonal != 4 AND seasonal != 5 AND presence != 3 AND presence != 6"
FILTERS="ORIGIN = 1 AND SEASONAL != 4 AND SEASONAL != 5 AND PRESENCE != 3 AND PRESENCE != 6"

# Marine species lists
marine_birds=$(tail -n +2 "${specieslistpath}/marine_bird_spp.csv" | sed "s/\(.*\)/'\1'/g" | paste -sd, -)
marine_reptiles=$(tail -n +2 "${specieslistpath}/marine_reptiles.csv" | sed "s/\(.*\)/'\1'/g" | paste -sd, -)

# Reproject ESRI Shapefiles to GPKG (only needed for non-GPKG inputs)

#--------- Amphibians -------------
#Salamanders
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM CAUDATA WHERE ${filters}" \
  "${polygonpath}/caudata.gpkg" "${inpath}/CAUDATA/CAUDATA.shp"

#Caecilians
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM GYMNOPHIONA WHERE ${filters}" \
  "${polygonpath}/gymnophiona.gpkg" "${inpath}/GYMNOPHIONA/GYMNOPHIONA.shp"

#Frogs
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM ANURA_PART1 WHERE ${filters}" \
  "${polygonpath}/anura1.gpkg" "${inpath}/ANURA/ANURA_PART1.shp"

ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM ANURA_PART2 WHERE ${filters}" \
  "${polygonpath}/anura2.gpkg" "${inpath}/ANURA/ANURA_PART2.shp"

#--------- Mammals -------------
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM MAMMALS_TERRESTRIAL_ONLY WHERE ${filters}" \
  "${polygonpath}/mammals.gpkg" "${inpath}/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp"

# Shrews
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

#------ Reptiles --------

# All reptiles
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles})" \
  "${polygonpath}/reptiles.gpkg" "${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"

# Lizards
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'lizard'" \
  "${polygonpath}/lizard.gpkg" "${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"

# Snakes
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'snake'" \
  "${polygonpath}/snakes.gpkg" "${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"

# Turtles
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'turtle'" \
  "${polygonpath}/turtles.gpkg" "${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"

# Crocodiles
ogr2ogr -of "GPKG" -nlt "MULTIPOLYGON" -progress -t_srs "${ceaproj}" \
  -sql "SELECT * FROM modeled_reptiles WHERE Binomial NOT IN (${marine_reptiles}) AND Group = 'croc'" \
  "${polygonpath}/crocodile.gpkg" "${inpath}/Reptiles/doi_10_5061_dryad_83s7k__v20171009/GARD1.1_dissolved_ranges/modeled_reptiles.shp"

rasterizeranges() {
    # Get number of features in shapefile by parsing the output of ogrinfo
    nfeatures=$(ogrinfo -ro -al -so ${polygonpath}/${1}.gpkg | grep -i "Feature Count" | tr -d -c 0-9)

    # Rasterize each feature individually to desired resolution and extent
    for ((i=1; i<=$nfeatures; i++)); do
        gdal_rasterize -burn 1 -at -of GTiff -tr 48250 48250 -te -17367530 -7342230 17367530 7342230 -where fid=${i} ${polygonpath}/${1}.gpkg ${temppath}/${1}_species_${i}.tif
        gdal_rasterize -burn 1 -at -of GTiff -tr 96500 96500 -te -17367530 -7342230 17367530 7342230 -where fid=${i} ${polygonpath}/${1}.gpkg ${temppath}/${1}_species_${i}_2x.tif
        gdal_rasterize -burn 1 -at -of GTiff -tr 193000 193000 -te -17367530 -7342230 17367530 7342230 -where fid=${i} ${polygonpath}/${1}.gpkg ${temppath}/${1}_species_${i}_4x.tif
        echo "Species ${i} of ${nfeatures} rasterized"
    done
}

calculate_richness() {
    local shapefile_name=$1
    local start_index=${2:-1}  # Start index, defaults to 1

    # Get number of features in shapefile by parsing the output of ogrinfo
    nfeatures=$(ogrinfo -ro -al -so ${polygonpath}/${shapefile_name}.gpkg | grep -i "Feature Count" | tr -d -c 0-9)

    # Initialize richness raster if not already present
    gdal_create -if ${temppath}/${shapefile_name}_species_1.tif -burn 0 ${outpath}/${shapefile_name}_richness.tif
    gdal_create -if ${temppath}/${shapefile_name}_species_1_2x.tif -burn 0 ${outpath}/${shapefile_name}_richness_2x.tif
    gdal_create -if ${temppath}/${shapefile_name}_species_1_4x.tif -burn 0 ${outpath}/${shapefile_name}_richness_4x.tif

    # Loop through the species rasters and add each one to the richness raster
    for ((i=$start_index; i<=$nfeatures; i++)); do
        gdal_calc.py -A ${temppath}/${shapefile_name}_species_${i}.tif -B ${outpath}/${shapefile_name}_richness.tif --outfile=${outpath}/${shapefile_name}_richness.tif --calc="A+B" --overwrite --quiet
        gdal_calc.py -A ${temppath}/${shapefile_name}_species_${i}_2x.tif -B ${outpath}/${shapefile_name}_richness_2x.tif --outfile=${outpath}/${shapefile_name}_richness_2x.tif --calc="A+B" --overwrite --quiet
        gdal_calc.py -A ${temppath}/${shapefile_name}_species_${i}_4x.tif -B ${outpath}/${shapefile_name}_richness_4x.tif --outfile=${outpath}/${shapefile_name}_richness_4x.tif --calc="A+B" --overwrite --quiet
        echo "Species ${i} of ${nfeatures} added"
    done

    # Delete temporary species rasters
    rm ${temppath}/${shapefile_name}_species_*.tif
}

# Run the function on each shapefile

rasterizeranges shrews 
calculate_richness shrews

rasterizeranges soricinae 
calculate_richness soricinae

rasterizeranges crocidurinae #106 restart at stopped index if necessary
calculate_richness crocidurinae 176

rasterizeranges crocodile
calculate_richness crocodile

rasterizeranges turtles
calculate_richness turtles

rasterizeranges caudata
calculate_richness caudata 903

rasterizeranges gymnophiona
calculate_richness gymnophiona

rasterizeranges anura1
calculate_richness anura1

rasterizeranges anura2 
calculate_richness anura2 
# if crashes, pick up where you left off

gdal_calc.py -A ${outpath}/anura1_richness.tif -B ${outpath}/anura2_richness.tif --outfile=${outpath}/anura_richness.tif --calc="A+B"


rasterizeranges birds 11191 
calculate_richness birds


rasterizeranges mammals
calculate_richness mammals

rasterizeranges squamates
calculate_richness squamates

rasterizeranges lizards
calculate_richness lizards

rasterizeranges snakes
calculate_richness snakes

# Sum anura part 1 and anura part 2
gdal_calc.py -A ${outpath}/anura1_richness.tif -B ${outpath}/anura2_richness.tif --outfile=${outpath}/anura_richness.tif --calc="A+B"

# Process different taxonomic groups
for taxa in soricinae crocidurinae shrew; do
    rasterizeranges $taxa
    calculate_richness $taxa
done
# Process different taxonomic groups
for taxa in crocodile turtles caudata gymnophiona anura1 anura2 birds shrews soricinae crocidurinae mammals squamates lizards snakes; do
    rasterizeranges $taxa
    calculate_richness $taxa
done

# Final cleanup: remove all temporary raster files
echo "Cleaning up all temporary rasters..."
rm -rf "${temppath:?}"/*
echo "✅ Temporary rasters cleared from ${temppath}"

# Function to rasterize species ranges
rasterizeranges() {
    local shapefile_name=$1
    local start_index=${2:-1}
    nfeatures=$(ogrinfo -ro -al -so "${polygonpath}/${shapefile_name}.gpkg" | grep -i "Feature Count" | tr -d -c 0-9)

    echo "Starting rasterization at feature $start_index of $nfeatures"
    
    for ((i=start_index; i<=nfeatures; i++)); do
        gdal_rasterize -burn 1 -at -of GTiff -tr 48250 48250 -te -17367530 -7342230 17367530 7342230 \
        -where fid=${i} "${polygonpath}/${shapefile_name}.gpkg" "${temppath}/${shapefile_name}_species_${i}.tif"
        echo "Species ${i} of ${nfeatures} rasterized"
    done
}

# Function to calculate species richness
calculate_richness() {
    local shapefile_name=$1
    local start_index=${2:-1}
    
    nfeatures=$(ogrinfo -ro -al -so "${polygonpath}/${shapefile_name}.gpkg" | grep -i "Feature Count" | tr -d -c 0-9)

    if [ "$start_index" -eq 1 ]; then
        gdal_create -if "${temppath}/${shapefile_name}_species_1.tif" -burn 0 "${outpath}/${shapefile_name}_richness.tif"
    fi

    for ((i=start_index; i<=nfeatures; i++)); do
        raster_file="${temppath}/${shapefile_name}_species_${i}.tif"
        if [ -f "$raster_file" ]; then
            gdal_calc.py -A "$raster_file" -B "${outpath}/${shapefile_name}_richness.tif" \
            --outfile="${outpath}/${shapefile_name}_richness.tif" --calc="A+B" --overwrite --quiet
            echo "Species ${i} of ${nfeatures} added"
        fi
    done
    
    rm "${temppath}/${shapefile_name}_species_*.tif"
}


# Process different taxonomic groups
for taxa in crocodile turtles caudata gymnophiona anura1 anura2 birds shrews soricinae crocidurinae mammals squamates lizards snakes; do
    rasterizeranges $taxa
    calculate_richness $taxa
done
#clear temporary rasters
rm -rf "${temppath:?}"/*

# Sum Anura richness rasters
gdal_calc.py -A "${outpath}/anura1_richness.tif" -B "${outpath}/anura2_richness.tif" \
--outfile="${outpath}/anura_richness.tif" --calc="A+B"

# Generate species count CSV
echo "Taxa,Species_Count" > "$output_csv"

taxa_filepaths=("${polygonpath}/caudata.gpkg" "${polygonpath}/mammals.gpkg" "${polygonpath}/birds.gpkg")
taxa_layers=("caudata" "MAMMALS_TERRESTRIAL_ONLY" "All_Species")
taxa_columns=("sci_name" "sci_name" "SCINAME")

for i in $(seq 0 $((${#taxa_filepaths[@]} - 1))); do
    filepath="${taxa_filepaths[$i]}"
    layer="${taxa_layers[$i]}"
    column="${taxa_columns[$i]}"
    filename=$(basename "$filepath" .gpkg)
    count=$(ogrinfo -ro -q "$filepath" "$layer" -sql "SELECT DISTINCT $column FROM $layer" | grep "=" | awk -F'= ' '{print $2}' | sort -u | wc -l)
    echo "$filename,$count" >> "$output_csv"
done

echo "✅ CSV saved at: $output_csv"

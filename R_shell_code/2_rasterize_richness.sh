# 2_rasterize_richness.sh

# ---------------------------------------------
# Use this script to rasterize species ranges and calculate richness at
# 3 resolutions: standard (1x), 2x, and 4x
#
# HOW TO RUN:
# 1. Open Terminal
# 2. Navigate to this script's directory, or let it auto-set
# 3. Make executable (once): chmod +x 2_rasterize_richness.sh
# 4. Run: ./2_rasterize_richness.sh
#
# Output saved to:
#   Richness_Distributions/richnessrasters
# ---------------------------------------------

# Set working directory to script location
#cd "$(dirname "$0")"

# Steps
# 1. Loop through all the features in the GeoPackage, individually rasterize each one to the desired resolution and save as a separate raster. If any polygon touches the pixel at all, it is 1, else 0.
# 2. Sum up the individual species rasters to produce a richness raster.
# 3. Postprocess: add Anura part 1 and Anura part 2, then use the ocean shapefile to mask all ocean and large lake pixels from each richness raster.

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


# Define a function which rasterizes the species ranges and adds them up
# Takes the name of the input shapefile, without extension, as argument 
# Now, three versions are produced: 48250 m resolution (standard), 96500 m (2x), and 193000 m (4x)

rasterizeranges() {
    # Get number of features in shapefile by parsing the output of ogrinfo
    nfeatures=$(ogrinfo -ro -al -so ${polygonpath}/${1}.gpkg | grep -i "Feature Count" | tr -d -c 0-9)

    # Optional: Accept a start index as an argument
    start_index=${2:-1}  # Default to 1 if not provided

    # Rasterize each feature individually to the desired 2x and 4x resolutions
    for ((i=$start_index; i<=$nfeatures; i++)); do
        raster_std="${temppath}/${1}_species_${i}.tif"
        raster_2x="${temppath}/${1}_species_${i}_2x.tif"
        raster_4x="${temppath}/${1}_species_${i}_4x.tif"

        # Skip rasterizing if the temporary raster already exists
        if [ -f "$raster_2x" ] && [ -f "$raster_4x" ]; then
            echo "Temporary rasters for species ${i} already exist, skipping..."
            continue
        fi

        # Rasterize to standard, 2x and 4x resolutions
        gdal_rasterize -burn 1 -at -of GTiff -tr 48250 48250 -te -17367530 -7342230 17367530 7342230 \
            -where fid=${i} ${polygonpath}/${1}.gpkg $raster_std
        gdal_rasterize -burn 1 -at -of GTiff -tr 96500 96500 -te -17367530 -7342230 17367530 7342230 \
            -where fid=${i} ${polygonpath}/${1}.gpkg $raster_2x
        gdal_rasterize -burn 1 -at -of GTiff -tr 193000 193000 -te -17367530 -7342230 17367530 7342230 \
            -where fid=${i} ${polygonpath}/${1}.gpkg $raster_4x
        echo "Species ${i} of ${nfeatures} rasterized"
    done

    # Initialize richness raster as a raster of all zeros with same dimensions as the species rasters
    gdal_create -if ${temppath}/${1}_species_1.tif     -burn 0 ${outpath}/${1}_richness.tif
    gdal_create -if ${temppath}/${1}_species_1_2x.tif -burn 0 ${outpath}/${1}_richness_2x.tif
    gdal_create -if ${temppath}/${1}_species_1_4x.tif -burn 0 ${outpath}/${1}_richness_4x.tif

    # Loop through the species rasters and add each one to the richness raster
    for ((i=$start_index; i<=$nfeatures; i++)); do
        gdal_calc.py -A ${temppath}/${1}_species_${i}.tif -B ${outpath}/${1}_richness.tif \
                     --outfile=${outpath}/${1}_richness.tif --calc="A+B" --overwrite --quiet
        gdal_calc.py -A ${temppath}/${1}_species_${i}_2x.tif -B ${outpath}/${1}_richness_2x.tif \
                     --outfile=${outpath}/${1}_richness_2x.tif --calc="A+B" --overwrite --quiet
        gdal_calc.py -A ${temppath}/${1}_species_${i}_4x.tif -B ${outpath}/${1}_richness_4x.tif \
                     --outfile=${outpath}/${1}_richness_4x.tif --calc="A+B" --overwrite --quiet
        echo "Species ${i} of ${nfeatures} added"
    done

    # Delete temporary species rasters in chunks to avoid issues with deleting too many files at once
    find ${temppath} -type f -name "${1}_species_*.tif" -exec rm {} \;    
    find ${temppath} -type f -name "${1}_species_*_2x.tif" -exec rm {} \;
    find ${temppath} -type f -name "${1}_species_*_4x.tif" -exec rm {} \;

    echo "Temporary rasters deleted for ${1}"
}

# If stops, call the function for each taxon, with an optional starting index (e.g.,rasterizeranges reptiles 3171)

rasterizeranges gymnophiona
rasterizeranges caudata
rasterizeranges anura1
rasterizeranges anura2

rasterizeranges reptiles
rasterizeranges turtles
rasterizeranges crocodile 
rasterizeranges squamates
rasterizeranges lizard
rasterizeranges snakes

rasterizeranges birds
rasterizeranges mammals

rasterizeranges shrews 
rasterizeranges soricinae
rasterizeranges crocidurinae

# Sum anura part 1 and anura part 2
gdal_calc.py -A ${outpath}/anura1_richness.tif -B ${outpath}/anura2_richness.tif --outfile=${outpath}/anura_richness.tif --calc="A+B"
gdal_calc.py -A ${outpath}/anura1_richness_2x.tif -B ${outpath}/anura2_richness_2x.tif --outfile=${outpath}/anura_richness_2x.tif --calc="A+B"
gdal_calc.py -A ${outpath}/anura1_richness_4x.tif -B ${outpath}/anura2_richness_4x.tif --outfile=${outpath}/anura_richness_4x.tif --calc="A+B"




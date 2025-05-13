# Prey Capture Data Processing Scripts: Roach Solitary Data

1. r_run_roach_solitary_dlc_features_all.py
2. r_run_roach_solitary_dlc_features_all_temphum.py
3. r_run_roach_solitary_dlc_features_all_temphum_trial_frame.py

## 1. r_run_roach_solitary_dlc_features_all.py: Overview
This Python script processes DeepLabCut (DLC) pose estimates stored in CSV files and generates a consolidated output file. The script works by reading experimental metadata about solitary roach trials, processing DLC features, and combining them into a cohesive dataset.

### Script Explanation

#### <ins>Load Metadata</ins
- The experimental metadata `roach_solitary_trial_metadata.csv` file is read into a pandas DataFrame named `df_alltrials`.

#### <ins>Data Processing Steps</ins>
1. **Load Feature Data:** 
    - The script iterates through every DLC feature file and matches it with the corresponding metadata entry (i.e., trial) in `df_alltrials`.
    - The current DLC feature file is loaded into a single DataFrame `df_feat_sub`.
2. **Filter Trials:**
    - The subset of DLC features corresponding to the in-trial time range is extracted to `df_feat_sub_range`.
3. **Append Metadata:**
    - The script appends experimental metadata columns to `df_feat_sub_range`.

#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e.,`roach_solitary_dlc_features_all_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

## 2. r_run_roach_solitary_dlc_features_all_temphum.py: Overview
This Python script further processes DLC solitary roach data by associating it with temperature and humidity readings. The script reads an input CSV file containing consolidated DLC features from every trial, appends corresponding temperature and humidity data for every trial, and generates a consolidated output CSV file.

### Script Explanation

#### <ins>Load Initial Data</ins>
- The file for the consolidated DLC features from every trial `roach_solitary_dlc_features_all_final.csv` is read into a pandas DataFrame named `df_subset`.

#### <ins>Append Temperature and Humidity Data</ins>
- The script then iterates through each unique trial ID present in `df_subset`.
- For each unique trial ID, the corresponding temperature and humidity data that matches that date and arena are loaded.
- The subset of temperature and humidity data corresponding to the in-trial time range is extracted, and various statistical measures (e.g., median, mean, max, min) are calculated and appended.

#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e.,`roach_solitary_dlc_features_all_temphum_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

## 3. r_run_roach_solitary_dlc_features_all_temphum_trial_frame.py: Overview
This Python script further processes DLC solitary roach data by adding an additional column, `trial_frame`, which assigns a unique identifier for each frame of every trial.

### Script Explanation

#### <ins>Load Initial Data</ins>
- The file for the consolidated DLC features from every trial plus temperature and humidity data `roach_solitary_dlc_features_all_temphum_final.csv` is read into a pandas DataFrame named `df_subset`.

#### <ins>Video Frame Number Assignment</ins>
- The script iterates through each unique trial ID present in `df_subset`.
- For each unique trial ID, a new column `trial_frame` is added, which ranges from 1 to the length of the trial (i.e., the number of rows for that ID).

#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e., `roach_solitary_dlc_features_all_temphum_trial_frame_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

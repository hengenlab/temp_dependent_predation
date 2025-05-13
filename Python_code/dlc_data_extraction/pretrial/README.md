# Prey Capture Data Processing Scripts: Mouse Pretrial Data

1. r_run_capture_pretrial_dlc_features_all_ckbn.py
2. r_run_capture_pretrial_dlc_features_all_temphum_ckbn.py
3. r_run_capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn.py

## 1. r_run_capture_pretrial_dlc_features_all_ckbn.py: Overview
This Python script processes DeepLabCut (DLC) pose estimates stored in CSV files and generates a consolidated output file. The script works by reading experimental metadata about prey capture trials, processing DLC features, and combining them into a cohesive dataset.

### Script Explanation

#### <ins>Load Metadata</ins>
- The experimental metadata `capture_trial_metadata.csv` file is read into a pandas DataFrame named `df_alltrials`.

#### <ins>Process Data for Each Animal and Date</ins>
- The script iterates through each unique animal and date present in the experimental metadata.
- For each animal and date combination, the script locates the corresponding DLC feature files, subsets based on a specified pretrial time range (i.e., 60 seconds), appends experimental metadata, and saves to a consolidated output file.

#### <ins>Data Processing Steps</ins>
1. **Subset Metadata:** 
    - The experimental metadata DataFrame is filtered for the current animal and date.
2. **Load Feature Data:**
    - The DLC feature files for the current animal and date are concatenated into a single DataFrame `df_feat_sub`.
3. **Video Frame Number Assignment:**
    - A column named `video_frame` is added to `df_feat_sub`, assigning frame numbers sequentially.
4. **Filter Pretrials:**
    - For each row (i.e., trial) in the filtered metadata, the script checks for valid start and end capture times.
    - It then converts these times into frame numbers by calculating total seconds and multiplying by frame rate (30 fps).
    - The subset of DLC features corresponding to the 60 seconds preceding each trial (i.e., pretrial time range) is extracted to `df_feat_sub_range`.
5. **Append Metadata:**
    - The script appends experimental metadata columns to `df_feat_sub_range`.

#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e.,`capture_pretrial_dlc_features_all_ckbn_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

## 2. r_run_capture_pretrial_dlc_features_all_temphum_ckbn.py: Overview
This Python script further processes DLC pretrial data by associating it with temperature and humidity readings. The script reads an input CSV file containing consolidated DLC features from every pretrial period, appends corresponding temperature and humidity data for every pretrial period, and generates a consolidated output CSV file.

### Script Explanation

#### <ins>Load Initial Data</ins>
- The file for the consolidated DLC features from every pretrial period `capture_pretrial_dlc_features_all_ckbn_final.csv` is read into a pandas DataFrame named `df_subset`.

#### <ins>Data Processing Steps</ins>
1. **Add Arena Number:** 
    - The correct experimental arena number is added based on each animal, stored in a new column named `arenanum`.

2. **Create Trial ID:** 
    - Unique trial IDs are created for each trial, using the `date`, `arenanum`, and `trial_daily` columns in `df_subset`, stored in a new column named `id`.

3. **Append Temperature and Humidity Data:** 
    - The script then iterates through each unique trial ID present in `df_subset`.
    - For each unique trial ID, the corresponding temperature and humidity data that matches that date and arena are loaded.
    - The subset of temperature and humidity data corresponding to the 60 seconds preceding each trial (i.e., pretrial time range) is extracted, and various statistical measures (e.g., median, mean, max, min) are calculated and appended.


#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e.,`capture_pretrial_dlc_features_all_temphum_ckbn_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

## 3. r_run_capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn.py: Overview
This Python script further processes DLC pretrial data by adding an additional column, `trial_frame`, which assigns a unique identifier for each frame of every pretrial period.

### Script Explanation

#### <ins>Load Initial Data</ins>
- The file for the consolidated DLC features from every pretrial period plus temperature and humidity data `capture_pretrial_dlc_features_all_temphum_ckbn_final.csv` is read into a pandas DataFrame named `df_subset`.

#### <ins>Video Frame Number Assignment</ins>
- The script iterates through each unique trial ID present in `df_subset`.
- For each unique trial ID, a new column `trial_frame` is added, which ranges from 1 to the length of the corresponding pretrial period (i.e., the number of rows for that ID).

#### <ins>Save Processed Data</ins>
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e., `capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn_final.csv`).

#### <ins>Execution</ins>
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.

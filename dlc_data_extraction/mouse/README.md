# Prey Capture Data Processing Scripts: Mouse Trial Data

1. r_run_capture_dlc_features_all_ckbn.py
2. r_run_capture_dlc_features_all_temphum_ckbn.py
3. r_run_capture_dlc_features_all_temphum_trial_frame_ckbn.py

## 1. r_run_capture_dlc_features_all_ckbn.py: Overview
This Python script processes DeepLabCut (DLC) pose estimates stored in CSV files and generates consolidated output files. The script works by reading experimental metadata about prey capture trials, processing DLC features, and combining them into a cohesive dataset.

### Script Explanation

#### Load Metadata
- The experimental metadata `capture_trial_metadata.csv` file is read into a pandas DataFrame named `df_alltrials`.

#### Process Data for Each Animal and Date
- The script iterates through each unique animal and date present in the experimental metadata.
- For each animal and date combination, the script locates the corresponding DLC feature files.

##### Data Processing Steps
1. **Subset Metadata:** 
    - The experimental metadata DataFrame is filtered for the current animal and date.
2. **Load Feature Data:**
    - The DLC feature files for the current animal and date are concatenated into a single DataFrame `df_feat_sub`.
3. **Frame Number Assignment:**
    - A column named `video_frame` is added to `df_feat_sub`, assigning frame numbers sequentially.

4. **Filter Trials:**
    - For each row (i.e., trial) in the filtered metadata, the script checks for valid start and end capture times.
    - It then converts these times into frame numbers by calculating total seconds and multiplying by frame rate (30 fps).
    - The subset of DLC features corresponding to the in-trial time ranges is extracted to `df_feat_sub_range`.

5. **Append Metadata:**
    - The script appends experimental metadata columns to `df_feat_sub_range`.

##### Save Processed Data
- The consolidated data is saved to a CSV file, either creating a new file or appending to an existing one (i.e.,`capture_dlc_features_all_ckbn_final.csv`).

#### Execution
- To run the script, simply execute it in a Python environment. Ensure that paths specified for input files and output directories are correct and exist.
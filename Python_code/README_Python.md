# Python Workflow: Behavioral Tracking and Sensor Logging

This document describes the Python tools used in the temperature-dependent predation project to process behavioral video data and environmental sensor readings. These scripts are not required to reproduce figures or statistical results in the main analysis pipeline but are provided for reproducibility and raw data generation.

---

## Overview

The Python scripts are organized into two main components:

### 1. `dlc_data_extraction/`
Scripts that process pose estimation data from DeepLabCut (DLC) across different trial types.

#### `pretrial/`  
Processes the 60 seconds **before** each trial:
- `r_run_capture_pretrial_dlc_features_all_ckbn.py`
- `r_run_capture_pretrial_dlc_features_all_temphum_ckbn.py`
- `r_run_capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn.py`

#### `trial/`  
Processes **during-trial** behavior for mouse–roach interactions:
- `r_run_capture_dlc_features_all_ckbn.py`
- `r_run_capture_dlc_features_all_temphum_ckbn.py`
- `r_run_capture_dlc_features_all_temphum_trial_frame_ckbn.py`

#### `roach/`  
Processes **solitary roach control** trials:
- `r_run_roach_solitary_dlc_features_all.py`
- `r_run_roach_solitary_dlc_features_all_temphum.py`
- `r_run_roach_solitary_dlc_features_all_temphum_trial_frame.py`

Each folder contains its own README with further documentation.

---

### 2. `temp-hum-raspberrypi/`
Scripts and setup instructions for recording temperature and humidity data using a Raspberry Pi and I2C sensors (HTU21D and TMP117).  
Main script:
- `temperature_humidity_recording.py`

---

## Dependencies

To run these scripts, install the following Python packages:

- `pandas`
- `numpy`
- `adafruit-circuitpython-htu21d`
- `adafruit-circuitpython-tmp117`
- `adafruit-blinka`
- `RPi.GPIO`

Also install Raspberry Pi support libraries:

```bash
sudo apt-get install python3-pip python3-numpy python3-pandas python-smbus i2c-tools libatlas-base-dev
```

---

## Execution Notes

Each script can be run independently. Make sure:

- Metadata files (e.g., `capture_trial_metadata.csv`) are in place
- Input paths to DLC `.csv` files are set correctly
- Output paths exist or will be created

Scripts will produce enriched CSV datasets for downstream analysis.

import numpy as np
import seaborn as sns
import pandas as pd
import glob
import sys
import platform
from datetime import date
import os.path as op
import matplotlib.pyplot as plt
import json
import os
import neuraltoolkit as ntk
from datetime import datetime
from datetime import timedelta

import platform
import matplotlib.pyplot as plt
if platform == "darwin":
    # matplotlib.use('TkAgg')
    plt.switch_backend('TkAgg')
else:
    # matplotlib.use('Agg')
    plt.switch_backend('Agg')



df_subset = pd.read_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv') # , nrows=100)


for id_sub_i in df_subset.id.unique():
    print("\n" * 100, " id_sub_i ", id_sub_i)
    
    # create a local copy from id's
    tmp_df_temp_trails = None
    tmp_df_temp_trails = df_subset.loc[df_subset.id == id_sub_i]
    
    # Add new column trial_frame with range 1 to len of trial, len(tmp_df_temp_trails)
    # convert to int
    tmp_df_temp_trails.loc[tmp_df_temp_trails.id == id_sub_i, ['trial_frame']]  =\
        list(range(1, len(tmp_df_temp_trails)+1, 1))
    tmp_df_temp_trails = tmp_df_temp_trails.astype({"trial_frame": int})            
            
    # Append each trial and save
    if not os.path.exists('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv'):
        print("creating file", flush=True)
        # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv', mode = 'w', sep = ',', encoding='utf-8', header=True, index=False)
        with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv', 'w') as file:
            tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=True, index=False)
    else:
        print("appending to file", flush=True)
        # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv', mode = 'a', sep = ',', encoding='utf-8', header=False, index=False)
        with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv', 'a') as file:
            tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=False, index=False)


        
print("DONE", flush=True)
             

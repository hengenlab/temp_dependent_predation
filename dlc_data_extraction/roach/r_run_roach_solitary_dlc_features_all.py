from datetime import datetime
from datetime import timedelta

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



# Load all trials
fl = '/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/roach_solitary_trial_metadata.csv'
df_alltrials = pd.read_csv(fl) #, nrows=100)
df_alltrials.rename(columns={"across multiple videos": "across_multiple_videos"}, inplace=True)


# get all feature files
fl_feat_list = None
# fl_feat_list = ntk.natural_sort(glob.glob('/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/Roach_Movement/*/*/*_features.csv'))
# fl_feat_list = ntk.natural_sort(glob.glob('/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/Roach_Movement/*/*/*_features_noffill_and_likelihood_50_no_smooth.csv'))
fl_feat_list = ntk.natural_sort(glob.glob('/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/Roach_Movement/*/*/*_features_noffill_and_likelihood_50_no_smooth_fps_dva.csv'))

# for each feature file loop through and 
for fl_feat_list_sub in fl_feat_list:
    print(f"fl_feat_list_sub {fl_feat_list_sub}", flush=True)
    # video_name = op.basename(fl_feat_list_sub).replace('_features.csv', '')
    video_name = op.basename(fl_feat_list_sub).replace('_features_noffill_and_likelihood_50_no_smooth_fps_dva.csv', '')
    print(f"video_name {video_name}")
    
    # find start and stop frame for video_name from fl_feat_list_sub
    df_new = df_alltrials.loc[df_alltrials.video == video_name, ["date", "arena", "targ_temp", "trial_daily", "start_frame", "stop_frame", "id", "roach_mass", "keep", "notes", "across_multiple_videos"]]
    df_new['video_name'] = video_name
    print(f"df_alltrials.video {df_alltrials.video.unique()}")
    print(f"df_new {df_new.head(1)}")
    df_feat_sub = pd.read_csv(fl_feat_list_sub)
    # Yes frame_num is frame number
    df_feat_sub.rename(columns={'Unnamed: 0':'video_frame'}, inplace=True )


    # iter through rows in df_new and get ranges from df_feat_sub
    for index, row in df_new.iterrows():      
        print(f"start frame {row.start_frame} stop frame {row.stop_frame} size {row.stop_frame - row.start_frame}")
        df_feat_sub_range = None
        df_feat_sub_range = df_feat_sub.iloc[int(row.start_frame):int(row.stop_frame)]
        print(f"sh df_feat_sub {df_feat_sub.shape}")
        print(f"sh df_feat_sub_range {df_feat_sub_range.shape}")
        # print(f"tail df_feat_sub_range {df_feat_sub_range.tail(1)}")

        # append to each row values from df_new as columns in df_feat_sub_range
        for k, v in zip(row.keys().tolist(), row.values.tolist()):
            # print(f"k {k} v {v}", flush=True)
            df_feat_sub_range.loc[:, [k]] = v # [v] * len(df_feat_sub_range)
        # reorder these columns to the begining
        tmp_c = None
        tmp_c = df_feat_sub_range.columns[-11:].to_list()
        tmp_c.extend(df_feat_sub_range.columns[:-11].to_list())
        df_feat_sub_range = df_feat_sub_range[tmp_c]
        print(f"sh df_feat_sub_range {df_feat_sub_range.shape}", flush=True)

        # Append each trial and save        
        if not os.path.exists('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv'):
            print("creating file", flush=True)
            # df_feat_sub_range.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv', mode = 'w', sep = ',', encoding='utf-8', header=True, index=False)
            with open('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv', 'w') as file:
                df_feat_sub_range.to_csv(file, sep=',', encoding='utf-8', header=True, index=False)
        else:
            print("appending to file", flush=True)
            # df_feat_sub_range.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv', mode = 'a', sep = ',', encoding='utf-8', header=False, index=False)
            with open('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv', 'a') as file:
                df_feat_sub_range.to_csv(file, sep=',', encoding='utf-8', header=False, index=False)

print("DONE", flush=True)

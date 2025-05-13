import numpy as np
import pandas as pd
from datetime import datetime
from datetime import timedelta
import glob

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




# Load data from 
# df_subset = pd.read_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all.csv')
df_subset = pd.read_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_final.csv')

# Now create id column from id column already exists
# For example '23_2023-05-16_1A_1' to '2023-05-16_1A_1'
# id is date_arena_trialdaily
df_subset['id'] = df_subset.id.str.slice_replace(0, 3, '')

# Find all unique ids and loop through it
for id_sub_i in df_subset.id.unique():
    print("\n" * 100, " id_sub_i ", id_sub_i)

    # create a local copy from unique ids, ie id_sub_i
    df_local = None
    df_local = df_subset.loc[df_subset.id == id_sub_i]
    # print("df_local ", df_local)
    
    
    ### CHECK THIS WITH VARUN
    # From Varun start time is video + start_frame/30 and
    # end time is video + end_fram/30
    # Get time from video from first row in df_local    
    dt_v = datetime.strptime(df_local.iloc[0].video_name.split('-')[1], '%Y%m%dT%H%M%S')
    print(f"dt_v {dt_v}", flush=True)
    
    # Start time of trial is dt_v + (start_frame / 30) as seconds
    dt_start = dt_v + timedelta(hours=0, minutes=0, seconds=(df_local.iloc[0].start_frame)/30)
    # end time of trial is dt_v + ((stop_frame -start_frame) / 30) as seconds
    dt_end =   dt_v + timedelta(hours=0, minutes=0, seconds=(df_local.iloc[0].stop_frame)/30)
    print(f"dt_v {dt_v}  dt_start {dt_start} dt_end {dt_end}", flush=True)
    # sys.exit()

    
    

    joined_list = glob.glob(f"/media/bs001r/Jacob_Cockroach_Capture/Roach_Movement/{datetime.strptime(id_sub_i.split('_')[0], '%Y-%m-%d').strftime('%m-%d-%Y')}/temperature_humidity_*box{id_sub_i.split('_')[1][0]}.csv")
    print(f"joined_list {joined_list}", flush=True)
    # Initalize df      
    df = None
    
    # if len of joined_list > 1
    if len(joined_list) > 1:
        print(f"2: len joined_list {len(joined_list)}")    
        
        # Load all csv        
        df = pd.concat(map(pd.read_csv, joined_list), ignore_index=True) 
        
        # Extract trial interval using dt_start and dt_end
        df['pdate'] = pd.to_datetime(df['Date'], format='%Y-%m-%d_%H:%M:%S')
        mask = (df['pdate'] > dt_start) & (df['pdate'] <= dt_end)
        print(f"dt_v {dt_v} dt_start {dt_start} dt_end {dt_end} df['pdate'] {df['pdate'].describe()}", flush=True)
        print(f"df[mask] {df[mask]}", flush=True)
        print(df[mask].Temperature.median(), df[mask].Humidity.median())        

        tmp_df_temp_trails = None
        tmp_df_temp_trails = df_local.copy(deep=True)
        tmp_df_temp_trails['temp_median'] = df[mask].Temperature.median()
        tmp_df_temp_trails['humd_median'] = df[mask].Humidity.median()
        tmp_df_temp_trails['temp_mean'] = df[mask].Temperature.mean()
        tmp_df_temp_trails['humd_mean'] = df[mask].Humidity.mean() 
        tmp_df_temp_trails['temp_max'] = df[mask].Temperature.max()
        tmp_df_temp_trails['humd_max'] = df[mask].Humidity.max()   
        tmp_df_temp_trails['temp_min'] = df[mask].Temperature.min()
        tmp_df_temp_trails['humd_min'] = df[mask].Humidity.min()              

    elif len(joined_list) == 1:
        print(f"1: len joined_list {len(joined_list)}")

        
        # Load all csv
        df = pd.read_csv(joined_list[0])
        
        # Extract trial interval using dt_start and dt_end
        df['pdate'] = pd.to_datetime(df['Date'], format='%Y-%m-%d_%H:%M:%S')
        mask = (df['pdate'] > dt_start) & (df['pdate'] <= dt_end)
        print(f"dt_v {dt_v} dt_start {dt_start} dt_end {dt_end} df['pdate'] {df['pdate'].describe()}", flush=True)
        print(f"df[mask] {df[mask]}", flush=True)
        print(df[mask].Temperature.median(), df[mask].Humidity.median())                

        tmp_df_temp_trails = None
        tmp_df_temp_trails = df_local.copy(deep=True)
        tmp_df_temp_trails['temp_median'] = df[mask].Temperature.median()
        tmp_df_temp_trails['humd_median'] = df[mask].Humidity.median()
        tmp_df_temp_trails['temp_mean'] = df[mask].Temperature.mean()
        tmp_df_temp_trails['humd_mean'] = df[mask].Humidity.mean() 
        tmp_df_temp_trails['temp_max'] = df[mask].Temperature.max()
        tmp_df_temp_trails['humd_max'] = df[mask].Humidity.max()
        tmp_df_temp_trails['temp_min'] = df[mask].Temperature.min()
        tmp_df_temp_trails['humd_min'] = df[mask].Humidity.min()              

    # if no joined list insert np.nan        
    elif len(joined_list) == 0:
        print(f"0: len joined_list {len(joined_list)}")

        tmp_df_temp_trails = None
        tmp_df_temp_trails =  df_local.copy(deep=True)
        tmp_df_temp_trails['temp_median'] = np.nan 
        tmp_df_temp_trails['humd_median'] = np.nan 
        tmp_df_temp_trails['temp_mean'] =   np.nan 
        tmp_df_temp_trails['humd_mean'] =   np.nan 
        tmp_df_temp_trails['temp_max'] =    np.nan 
        tmp_df_temp_trails['humd_max'] =    np.nan 
        tmp_df_temp_trails['temp_min'] =    np.nan 
        tmp_df_temp_trails['humd_min'] =    np.nan 
        
        
    # Append each trial and save        
    if not os.path.exists('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_temphum_final.csv'):
        print("creating file", flush=True)
        # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_temphum_final.csv', mode = 'w', sep = ',', encoding='utf-8', header=True, index=False)
        with open('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_temphum_final.csv', 'w') as file:
            tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=True, index=False)
    else:
        print("appending to file", flush=True)
        # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_temphum_final.csv', mode = 'a', sep = ',', encoding='utf-8', header=False, index=False)
        with open('/media/HlabShare/ckbn/Prey_Capture_outputs/roach_solitary_dlc_features_all_temphum_final.csv', 'a') as file:
            tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=False, index=False)

print("DONE", flush=True)

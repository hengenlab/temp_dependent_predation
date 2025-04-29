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




df_subset = pd.read_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_ckbn_final.csv') # , nrows=100)

# Create column arenanum for creating id column later
# set valuea as 3A so later it can be checked any exists with 3A, 
# assign based on animals arena
# then it is wrong and crash
df_subset['areanum'] = "3A"
print(df_subset.areanum.unique(), "\n")
for animal_i, arena_j in zip(['jmg1', 'jmg2', 'jmg3', 'jmg4', 'jmg5', 'jmg6', 'jmg7', 'jmg8'],
                             ['1A',   '1B',   '1C',   '1D',   '2A',   '2B',   '2C',   '2D']):
    print("\n")
    print(animal_i, arena_j, flush=True)
    df_subset.loc[df_subset['animal'].isin([animal_i]), 'areanum'] = arena_j
    print(animal_i, arena_j,  df_subset.areanum.unique(), "\n\n\n\n")

# Check somehow 3A fake areanum still exists, then crash
tmp_uniq_list = [i.lower() for i in df_subset.areanum.unique().tolist()]
if "3A".lower() in tmp_uniq_list:
    raise RuntimeError("3A still in areanum", df_subset.areanum.unique().tolist())    
    

# Now create id column from date, arenanum and trial_daily
df_subset['id'] = df_subset.date.astype(str) + '_' + df_subset.areanum.astype(str) + '_' + df_subset.trial_daily.astype(str)

for id_sub_i in df_subset.id.unique():
    print("\n" * 100, " id_sub_i ", id_sub_i)


    # create a local copy from id's
    df_local = None
    df_local = df_subset.loc[df_subset.id == id_sub_i]
    
    # if len of df_local is > 0 raise error see at the bottom
    if len(df_local) > 0:
        # print("df_local ", df_local)

        # From Varun start time is video + start_cap and
        # end time is video + end_cap
        # Get time from video from first row in df_local
        print(f"df_local.iloc[0].video {df_local.iloc[0].video}", flush=True)
        dt_v = datetime.strptime(df_local.iloc[0].video.split('-')[1], '%Y%m%dT%H%M%S')
        
        # Now find start_time_cap and end_time_cap from first row in df_local
        s_ff = datetime.strptime(df_local.iloc[0].start_time_cap, '%H:%M:%S')
        e_ff = datetime.strptime(df_local.iloc[0].end_time_cap, '%H:%M:%S')
        print(f"dt_v {dt_v} s_ff {s_ff} e_ff {e_ff}", flush=True)
        
        
        # Start time of trial is dt_v + s_ff 
        # We are not including hours becuase, listed video corresponds to the hour that the trial started
        dt_start = dt_v + timedelta(hours=0, minutes=s_ff.minute, seconds=s_ff.second)
        print(f"dt_v {dt_v}  dt_start {dt_start} s_ff {s_ff} e_ff {e_ff}", flush=True)
        
        # End time of trial is dt_v + e_ff
        # hour is (end time hour - start time hour)
        # eg1 : 0:40:10 to 1:15:00, here hour in dt_end is (1 - 0) = 1
        # eg2 : 1:10:00 to 2:20:00, here hour in dt_end is (2 - 1) = 1
        # eg3:  1:23:45 to 1:29:34, here hour in dt_end is (1 - 1) = 0
        dt_end = dt_v + timedelta(hours=(e_ff.hour - s_ff.hour ) , minutes=e_ff.minute, seconds=e_ff.second)
        print(f"dt_v {dt_v} dt_start {dt_start} dt_end {dt_end} s_ff {s_ff} e_ff {e_ff}", flush=True)
        # check if difference in date is non negative
        if (dt_end - dt_start).total_seconds() < 0:
            raise RuntimeError(f"(dt_end - dt_start).total_seconds() {(dt_end - dt_start).total_seconds()}")
            

        # Create a joined list for all temperature_humidity box data if there is multiple ones
        print(f"joined_list /media/bs001r/Jacob_Cockroach_Capture/Main_Trials/*{datetime.strptime(id_sub_i.split('_')[0], '%m-%d-%y').strftime('%-m-%d-%y')}/temperature_humidity_*box{id_sub_i.split('_')[1][0]}.csv")
        joined_list = glob.glob(f"/media/bs001r/Jacob_Cockroach_Capture/Main_Trials/*{datetime.strptime(id_sub_i.split('_')[0], '%m-%d-%y').strftime('%-m-%d-%y')}/temperature_humidity_*box{id_sub_i.split('_')[1][0]}.csv")
        # print("\n", len(joined_list), "\njoined_list", joined_list)
        
        
        # Initalize df
        df = None
        
        # if len of joined_list > 1
        if len(joined_list) > 1:
            print(f"2: joined_list {len(joined_list)}")    
            # Load all csv
            df = pd.concat(map(pd.read_csv, joined_list), ignore_index=True) 
            # df.head()
            
            # Extract trial interval using dt_start and dt_end
            df['pdate'] = pd.to_datetime(df['Date'], format='%Y-%m-%d_%H:%M:%S')
            mask = (df['pdate'] > dt_start) & (df['pdate'] <= dt_end)
            print(f"dt_v {dt_v} dt_start {dt_start} dt_end {dt_end} df['pdate'] {df['pdate'].describe()}", flush=True)
            print(f"df[mask] {df[mask]}", flush=True)
            print(df[mask].Temperature.median(), df[mask].Humidity.median())            

            # initialize tmp_df_temp_trails and calculate mean, median, min, max
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

        # if len of joined list == 1
        elif len(joined_list) == 1:
            print(f"1: joined_list {len(joined_list)}")
            # Load all csv
            df = pd.read_csv(joined_list[0])
            # df.head()
            
            # Extract trial interval using dt_start and dt_end            
            df['pdate'] = pd.to_datetime(df['Date'], format='%Y-%m-%d_%H:%M:%S')
            mask = (df['pdate'] > dt_start) & (df['pdate'] <= dt_end)
            print(f"dt_v {dt_v} dt_start {dt_start} dt_end {dt_end} df['pdate'] {df['pdate'].describe()}", flush=True)
            print(f"df[mask] {df[mask]}", flush=True)
            print(df[mask].Temperature.median(), df[mask].Humidity.median())

            # initialize tmp_df_temp_trails and calculate mean, median, min, max
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
            print(f"0: joined_list {len(joined_list)}")

            # initialize tmp_df_temp_trails and np.nan mean, median, min, max
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
            print(f"sh tmp_df_temp_trails {tmp_df_temp_trails.shape}")

 
        # Append each trial and save
        if not os.path.exists('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv'):
            print("creating file", flush=True)
            # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv', mode = 'w', sep = ',', encoding='utf-8', header=True, index=False)
            with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv', 'w') as file:
                tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=True, index=False)
        else:
            print("appending to file", flush=True)
            # tmp_df_temp_trails.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv', mode = 'a', sep = ',', encoding='utf-8', header=False, index=False)
            with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_dlc_features_all_temphum_ckbn_final.csv', 'a') as file:
                tmp_df_temp_trails.to_csv(file, sep=',', encoding='utf-8', header=False, index=False)
            
    else:
        print("\n" * 100, " id_sub_i ", id_sub_i)
        raise RuntimeError(f'Error : len(df_local)  {len(df_local)}')

        
print("DONE", flush=True)
 

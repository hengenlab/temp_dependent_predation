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




fl = '/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/capture_trial_metadata.csv'
df_alltrials = pd.read_csv(fl) #, nrows=1000)

# Loop through animals
for animal_idx in df_alltrials.animal.unique().tolist()[0:]:
    # Loop through dates
    for date_idx in df_alltrials.date.unique().tolist()[0:]:
        # Get joined list of all features files
        # joined_list = ntk.natural_sort(glob.glob(f'/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/{date_idx}/{animal_idx}/*_features.csv'))
        # joined_list = ntk.natural_sort(glob.glob(f'/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/{date_idx}/{animal_idx}/*_features_append30f.csv'))
        # joined_list = ntk.natural_sort(glob.glob(f'/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/{date_idx}/{animal_idx}/*features_append30f_noffill_and_likelihood_50_no_smooth.csv'))
        joined_list = ntk.natural_sort(glob.glob(f'/media/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/Jacob_Cockroach_Capture_saDLC_out/{date_idx}/{animal_idx}/*features_append30f_noffill_and_likelihood_50_no_smooth_fps_dva.csv'))
        print("\n"*10)
        for jl in joined_list:
            print(jl)
        

        # Create a subset of df_alltrials based on animal_idx and date_idx
        df_new = df_alltrials.loc[df_alltrials.animal == animal_idx, :]
        print(f"df_new.shape {df_new.shape}", flush=True)
        df_new = df_new.loc[df_alltrials.date == date_idx, :]
        print(f"df_new.shape {df_new.shape}", flush=True)
        # Filter if keep is True
        # df_new = df_new.loc[df_new.keep == 'T', :]
        # print(f"df_new.shape {df_new.shape}", flush=True)
        
        # Only proceed if len df_new is > 0
        if len(df_new) > 0:
            # Load joined list as df_feat_sub
            df_feat_sub = None
            df_feat_sub = pd.concat(map(pd.read_csv, joined_list), ignore_index=True) 
            # print(df_feat_sub)
            # Rename unnamed to frame_num
            # df_feat_sub.rename(columns={'Unnamed: 0':'video_frame'}, inplace=True )
            df_feat_sub['video_frame'] = list(range(0, len(df_feat_sub.index), 1))
            
            for index, row in df_new.iterrows():
                # Only proceed it both row.start_time_cap and row.end_time_cap is available
                if ((isinstance(row.start_time_cap, str)) and (isinstance(row.end_time_cap, str))):
                    # Take care of empty spaces
                    # if empty spaces skip otherwise proceed forward
                    if not((row.start_time_cap.isspace()) or (row.end_time_cap.isspace())):
                        print(f"row {row}")
                        print("s ", datetime.strptime(row.date + '_' + row.start_time_cap, '%m-%d-%y_%H:%M:%S'))
                        print("e ", datetime.strptime(row.date + '_' + row.end_time_cap, '%m-%d-%y_%H:%M:%S'))

                        # From row.start_time_cap and row.end_time_cap calculate total_seconds from hours, minutes and seconds
                        # then convert to frames by multiplying 30
                        # finally extract that range from features list, df_feat_sub
                        print("s_f", timedelta(hours=int(row.start_time_cap.split(':')[0]),minutes=int(row.start_time_cap.split(':')[1]),seconds=int(row.start_time_cap.split(':')[2])).total_seconds()*30)
                        print("e_f", timedelta(hours=int(row.end_time_cap.split(':')[0]),minutes=int(row.end_time_cap.split(':')[1]),seconds=int(row.end_time_cap.split(':')[2])).total_seconds()*30)
                        s_i = int(timedelta(hours=int(row.start_time_cap.split(':')[0]),minutes=int(row.start_time_cap.split(':')[1]),seconds=int(row.start_time_cap.split(':')[2])).total_seconds()*30)
                        e_i = s_i * 1
                        s_i = s_i - (30 * 60)
                        # e_i = int(timedelta(hours=int(row.end_time_cap.split(':')[0]),minutes=int(row.end_time_cap.split(':')[1]),seconds=int(row.end_time_cap.split(':')[2])).total_seconds()*30)
                        # Add 60 seconds before trial
                        if s_i >= 0:
                            print(f"s_i {s_i} e_i {e_i}")
                            df_feat_sub_range = None
                            df_feat_sub_range = df_feat_sub.iloc[s_i:e_i]
                            print(f"sh df_feat_sub {df_feat_sub.shape}")
                            print(f"sh df_feat_sub_range {df_feat_sub_range.shape}")

                            # append to each row values from df_new  as columns in df_feat_sub_range
                            for k, v in zip(row.keys().tolist(), row.values.tolist()):
                                print(f"k {k} v {v}", flush=True)
                                df_feat_sub_range.loc[:, [k]] = v # [v]*len(df_feat_sub_range)
                                # df_feat_sub_range[k] = v
                            print(f"len row.keys().tolist() {len(row.keys().tolist())}")

                            # reorder these columns to the begining
                            tmp_c = None
                            tmp_c = df_feat_sub_range.columns[-25:].to_list()
                            tmp_c.extend(df_feat_sub_range.columns[:-25].to_list())
                            df_feat_sub_range = df_feat_sub_range[tmp_c]
                            df_feat_sub_range
                            # sys.exit()

                            # Append each trial and save
                            if not os.path.exists('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_pretrial_dlc_features_all_ckbn_final.csv'):
                                print("creating file", flush=True)
                                # df_feat_sub_range.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_pretrial_dlc_features_all_ckbn_final.csv', mode = 'w', sep = ',', encoding='utf-8', header=True, index=False)
                                with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_pretrial_dlc_features_all_ckbn_final.csv', 'w') as file:
                                    df_feat_sub_range.to_csv(file, sep=',', encoding='utf-8', header=True, index=False)
                            else:
                                print("appending to file", flush=True)
                                # df_feat_sub_range.to_csv('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_pretrial_dlc_features_all_ckbn_final.csv', mode = 'a', sep = ',', encoding='utf-8', header=False, index=False)
                                with open('/media/HlabShare/ckbn/Prey_Capture_outputs/capture_pretrial_dlc_features_all_ckbn_final.csv', 'a') as file:
                                    df_feat_sub_range.to_csv(file, sep=',', encoding='utf-8', header=False, index=False)


        else:
            print("\n" * 100, " df_new.shape ", df_new.shape)
            
            

print("\n", flush=True)
print("DONE", flush=True)



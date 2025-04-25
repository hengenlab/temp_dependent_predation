#!/usr/bin/env python3

import time
import board
import adafruit_tmp117
import pandas as pd
from datetime import datetime
import sys
import os.path as op
from adafruit_htu21d import HTU21D

'''
sudo apt-get install python3-pip
sudo pip3 install --upgrade setuptools
sudo apt-get install -y python-smbus
sudo apt-get install -y i2c-tools
sudo raspi-config
pip3 install RPI.GPIO
pip3 install adafruit-blinka
pip3 install adafruit-circuitpython-htu21d
sudo pip3 install adafruit-blinka
sudo apt install vim
pip3 install adafruit_tmp117
sudo apt install libatlas-base-dev
pip3 install pandas
pip3 install numpy
sudo reboot

# After experiment save to servers
# change temperature_humidity*.csv to file to copy
# scp /home/pi/temperature_humidity*.csv username@servername.wustl.edu:
'''


if __name__ == "__main__":
    '''
    Find temperature humidity and write to file
    '''
    i2c = board.I2C()

    # change file name please change name
    now = datetime.now()
    ft = now.strftime("%Y-%m-%d_%H-%M-%S")
    output_dir = '/home/pi/'
    output_file = 'temperature_humidity_exptdate_' + str(ft) + '.csv'
    output_file = op.join(output_dir, output_file)
    del ft
    del output_dir

    while True:
        # datetime object containing current date and time
        now = datetime.now()
        dt_string = now.strftime("%Y-%m-%d_%H:%M:%S")

        # Measure temperature from TMP117 accurate sensor
        tmp117 = adafruit_tmp117.TMP117(i2c)
        temperature = tmp117.temperature

        # Measure humidity from HTU21D-F humidity sensor
        # Do not get temperature from this (HTU21D-F) sensor
        hum_sensor = HTU21D(i2c)
        humidity = hum_sensor.relative_humidity

        # print on screen
        print('{0:}:\tTemp={1:0.2f}C% Humidity={2:0.1f}%'
              .format(dt_string, temperature, humidity))

        # Write to file
        d = {'Date': [dt_string],
             'Temperature': [temperature],
             'Humidity': [humidity]}
        df = pd.DataFrame(data=d)

        # Append to file if it already exists
        if not op.exists(output_file):
            df.to_csv(output_file, mode='a', header=True, index=False)
        else:
            df.to_csv(output_file,
                      mode='a', header=False, index=False)

        # sleep and take measurement
        # time.sleep(0.1)

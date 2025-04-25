## Using Temperature and Humidity sensors on Raspberry Pi
### Installation


#### Step 1: On Raspberry Pi, open terminal window and run the following lines of code:
```
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
sudo pip3 install adafruit-circuitpython-tmp117
pip3 install adafruit_tmp117
sudo apt install libatlas-base-dev
pip3 install pandas
pip3 install numpy
sudo apt install python3-numpy
sudo apt install python3-pandas
sudo reboot
```
For:
```
sudo raspi-config
```

- Under 'Interface Options', enable SSH, SPI, I2C, and Serial Port. 

- Under 'Advanced Options', enable Expanded File System.

**Step 2**: On Raspberry Pi, open terminal window, navigate to directory location of temperature_humidity_recording.py, and run the following line of code:
```
chmod 777 temperature_humidity_recording.py
```
This will make the temperature_humidity_recording.py script executable.
#!/bin/bash

python3 -m venv ./

mkdir output

source ./bin/activate

pip3 install -r requirements.txt

python3 createDrivers.py

echo "Dependencies installed."
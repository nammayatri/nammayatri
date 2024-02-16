import requests
import json
from datetime import datetime
import time
import os
from dotenv import load_dotenv
load_dotenv()

BASE_URL_DRIVER_LOCATION = os.getenv('BASE_URL_DRIVER_LOCATION')
BASE_URL_DRIVER = os.getenv('BASE_URL_DRIVER')
MerchantId = "favorit0-0000-0000-0000-00000favorit"
VehicleVariant = "SUV"

tokens = []

with open("Backend/load-test/tokens/driverTokens.json", "r") as file:
   tokens = json.load(file)


def format_custom_utc_date():
    now = datetime.utcnow()
    formatted_date = now.strftime('%Y-%m-%dT%H:%M:%SZ')
    return formatted_date

def set_driver_active(token):
    url = BASE_URL_DRIVER + "/driver/setActivity"

    headers = {
        'Accept': 'application/json;charset=utf-8',
        'token': token
    }

    params = {
        'active': 'true',
        'mode' : '"ONLINE"'
    }
    response = requests.post(url, headers=headers, params=params)
    print(response.json())

def set_driver_location(token, merchant_id, vehicle_variant):
    location_data = [
        {
            "pt": {
                "lat": 16.479615647756166,
                "lon": 75.39032459232
            },
            "ts": format_custom_utc_date()
        }
    ]

    headers = {
        "Content-Type": "application/json",
        "token": token,
        "mId": merchant_id,
        "vt": vehicle_variant,
        "dm": "ONLINE"
    }

    response = requests.post(BASE_URL_DRIVER_LOCATION + "/driver/location", json=location_data, headers=headers)
    print(token, response.json())

def main():
    for token in tokens:
        try:
            set_driver_active(token)
        except Exception as e:
            print(e)

    while True:
        for token in tokens:
            try:
                set_driver_location(token, MerchantId, VehicleVariant)
            except Exception as e:
                print(e)
        time.sleep(20)

if __name__ == "__main__":
    main()
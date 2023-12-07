import json
import requests
import os
from dotenv import load_dotenv
load_dotenv()

def auth(base_url, arr, file_name, merchant_id):
    all_tokens = []
    if not os.path.exists("./tokens"):
        os.mkdir("./tokens")

    for mobile_number in arr:
        auth_data = {
            "mobileNumber": mobile_number,
            "mobileCountryCode": "+91",
            "merchantId": merchant_id,
        }

        try:
            auth_res = requests.post(
                f"{base_url}/auth",
                json=auth_data,
                headers={"Content-Type": "application/json"},
            )

            auth_id = auth_res.json()["authId"]

            auth_verify_data = {
                "otp": "7891",
                "deviceToken": auth_id,
            }

            auth_verify_res = requests.post(
                f"{base_url}/auth/{auth_id}/verify",
                json=auth_verify_data,
                headers={"Content-Type": "application/json"},
            )

            token = auth_verify_res.json()["token"]
            all_tokens.append(token)
        except Exception as error:
            print(f"Error while fetching API: {error}")

    print(all_tokens, "token")

    with open(f"{file_name}", "w") as file:
        json.dump(all_tokens, file, indent=2)

    print("API response has been saved to the file:", file_name)

mobile_numbers = []

NUM_DRIVERS = os.getenv('NUM_DRIVERS')

for i in range(1, int(NUM_DRIVERS)):
    mobile_number = f"7777777{i:03d}"
    mobile_numbers.append(mobile_number)

base_url_driver = os.getenv('BASE_URL_DRIVER')
output_file_name_driver = "./tokens/driverTokens.json"
merchant_id_driver = "favorit0-0000-0000-0000-00000favorit"

base_url_customer = os.getenv('BASE_URL_RIDER')
output_file_name_customer = "./tokens/riderTokens.json"
merchant_id_customer = "YATRI"

auth(base_url_driver, mobile_numbers, output_file_name_driver, merchant_id_driver)
auth(base_url_customer, mobile_numbers, output_file_name_customer, merchant_id_customer)

from locust import HttpUser, task
import time
import requests
import json
import os
from dotenv import load_dotenv
load_dotenv()

class RiderApp(HttpUser):

    host = os.getenv('BASE_URL_RIDER')
    available_tokens = []
    with open("./tokens/riderTokens.json", "r") as file:
        available_tokens = json.load(file)

    def on_start(cls):
        cls.token = cls.available_tokens.pop(0)

    @task
    def list(self):
        print(f"Using token {self.token}")
        ride_resp = self.ride_search()
        search_id = ride_resp.json().get("searchId")
        estimate_id = self.get_estimate(search_id)
        if estimate_id:
            estimate_select_resp = self.estimate_select(estimate_id)
            self.log_response(estimate_select_resp)
            booking_resp = self.get_all_bookings()
            if booking_resp.get("otp") and booking_resp.get("bpp_ride_id"):
                self.send_data_to_service(booking_resp.get("otp"), booking_resp.get("bpp_ride_id"))
            else:
                print("No otp or bpp_ride_id found. Skipping send_data_to_service.")
        else:
            print("No estimate_id found. Skipping subsequent steps.")


    def ride_search(self):
        ride_data = {
            "fareProductType": "ONE_WAY",
            "contents": {
                "origin": {
                    "address": {
                        "area": "8th Block Koramangala",
                        "areaCode": "560047",
                        "building": "Juspay Buildings",
                        "city": "Bangalore",
                        "country": "India",
                        "door": "#444",
                        "street": "18th Main",
                        "state": "Karnataka"
                    },
                    "gps": {
                        "lat": 16.479615647756166,
                        "lon": 75.39032459232
                    }
                },
                "destination": {
                    "address": {
                        "area": "6th Block Koramangala",
                        "areaCode": "560047",
                        "building": "Juspay Apartments",
                        "city": "Bangalore",
                        "country": "India",
                        "door": "#444",
                        "street": "18th Main",
                        "state": "Karnataka"
                    },
                    "gps": {
                        "lat": 16.42990908333854,
                        "lon": 75.36080308508846
                    }
                }
            }
        }
        headers = {
            "Content-Type": "application/json",
            "token": self.token
        }
        response = self.client.post("/rideSearch", json=ride_data, headers=headers)
        return response


    def get_estimate(self, search_id):
        headers = {
            "Content-Type": "application/json",
            "token": self.token
        }

        for _ in range(10):
            response = self.client.get(f"/rideSearch/{search_id}/results", name="/rideSearch/:id/results", headers=headers)
            time.sleep(1)
            self.log_response(response)
            if len(response.json().get("estimates", [])) > 0:
                estimate_id = response.json()["estimates"][0]["id"]
                return estimate_id


    def estimate_select(self, estimate_id):
        headers = {
            "Content-Type": "application/json",
            "token": self.token,
        }

        estimateData =  {
            "customerExtraFee": None,
            "autoAssignEnabledV2": True,
            "autoAssignEnabled": True
        }

        response = self.client.post(url = "/estimate/{estimateId}/select2".format(estimateId = estimate_id), name="/estimate/:id/select2" , json=estimateData, headers=headers)
        return response

    def get_all_bookings(self):
        otp = None
        bpp_ride_id = None

        for _ in range(1000):
            time.sleep(3)

            headers = {
                "Content-Type": "application/json",
                "token": self.token,
            }

            response = requests.get(
                f"http://localhost:8013/v2/rideBooking/list?limit=20&onlyActive=true",
                headers=headers
            )

            body = response.json()

            if len(body.get("list", []))>0:
                ride_list = body['list'][0]['rideList']
                if ride_list:
                    otp = ride_list[0]['rideOtp']
                    bpp_ride_id = ride_list[0]['bppRideId']
                    break

        return {"otp": otp, "bpp_ride_id": bpp_ride_id}

    def send_data_to_service(self, ride_otp, bpp_ride_id):
        data = {
            "rideOtp": ride_otp,
            "rideId": bpp_ride_id,
        }

        PORT = os.getenv('OTP_SERVICE_PORT')
        url = f"http://localhost:{PORT}/setRideOtp"
        headers = {"Content-Type": "application/json"}

        response = requests.post(url, json=data, headers=headers)
        print(response)


    def log_response(self, response):
        print(response.json())
from locust import HttpUser, task
import time
import requests
import json
import os
from dotenv import load_dotenv
load_dotenv()

class DriverApp(HttpUser):

    host = os.getenv('BASE_URL_DRIVER')
    available_tokens = []
    with open("Backend/load-test/tokens/driverTokens.json", "r") as file:
        available_tokens = json.load(file)

    def on_start(cls):
        cls.token = cls.available_tokens.pop(0)

    @task
    def list(self):
        print(f"Using token {self.token}")
        ride_request_id = self.nearby_ride_request()
        quote_offer_resp = self.quote_offer(ride_request_id)
        ride_id = self.get_ride_list()
        ride_otp = self.get_ride_otp(ride_id)
        start_ride_resp = self.start_ride(ride_id, ride_otp)
        self.log_response(start_ride_resp)
        end_ride_resp = self.end_ride(ride_id)
        self.log_response(end_ride_resp)

    def nearby_ride_request(self):
        search_req_id = None

        for _ in range(2000):
            time.sleep(1)
            headers = {
                "Content-Type": "application/json",
                "token": self.token,
            }

            response = self.client.get(f"/driver/nearbyRideRequest", name="/driver/nearbyRideRequest", headers=headers)

            self.log_response(response)

            data = response.json()

            if len(data.get('searchRequestsForDriver', [])) > 0:
                search_req_id = data['searchRequestsForDriver'][0]['searchRequestId']
                break

        return search_req_id

    def quote_offer(self, ride_request_id):
        quote_offer_data = {
            "searchRequestId": ride_request_id,
            "offeredFare": 10,
        }

        headers = {
            "Content-Type": "application/json",
            "token": self.token,
        }

        response = self.client.post(f"/driver/searchRequest/quote/offer", json=quote_offer_data, headers=headers)
        return response

    def get_ride_list(self):
        ride_id = None
        while True:
            time.sleep(1)
            headers = {
                "Content-Type": "application/json",
                "token": self.token,
            }

            response = self.client.get(
                f"/driver/ride/list?onlyActive=true&limit=10",
                headers=headers
            )

            print(response.json())
            body = response.json()

            if len(body.get("list", [])) > 0:
                ride_id = body["list"][0]["id"]
                break

        return ride_id

    def get_ride_otp(self, ride_id):
        data = {
            "rideId": ride_id,
        }

        otp = None
        while True:
            PORT = os.getenv('OTP_SERVICE_PORT')
            time.sleep(1)
            response = requests.post(
                f"http://localhost:{PORT}/getRideOtp",
                json=data,
                headers={"Content-Type": "application/json"}
            )

            print(response.json())

            if response.json().get("otp") is not None:
                otp = response.json().get("otp")
                break

        return otp

    def start_ride(self, ride_id, otp):
        start_ride_data = {
            "rideOtp": otp,
            "point": {
                "lat": 12.978313106321112,
                "lon": 77.59512281916174,
            },
        }

        response = self.client.post(
            f"/driver/ride/{ride_id}/start",
            name="/driver/ride/:rideId/start",
            json=start_ride_data,
            headers={
                "Content-Type": "application/json",
                "token": self.token,
            }
        )

        return response

    def end_ride(self, ride_id):
        end_ride_data = {
            "point": {
                "lat": 12.999611228711393,
                "lon": 77.59198509906976,
            },
        }

        response = self.client.post(
            f"/driver/ride/{ride_id}/end",
            name="/driver/ride/:rideId/end",
            json=end_ride_data,
            headers={
                "Content-Type": "application/json",
                "token": self.token,
            }
        )

        return response

    def log_response(self, response):
        print(response.json())
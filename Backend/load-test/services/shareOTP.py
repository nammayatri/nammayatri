from flask import Flask, request, jsonify
import os
from dotenv import load_dotenv
load_dotenv()

app = Flask(__name__)

otp_map = {}

@app.route("/")
def home():
    return jsonify({"status": 200, "message": "server is up and running"})

@app.route("/setRideOtp", methods=["POST"])
def set_ride_otp():
    data = request.get_json()

    bpp_ride_id = data["rideId"]
    ride_otp = data["rideOtp"]

    otp_map[bpp_ride_id] = ride_otp

    return jsonify({"status": 200, "message": "Done"})

@app.route("/getRideOtp", methods=["POST"])
def get_ride_otp():
    data = request.get_json()

    print(otp_map)
    otp = otp_map.get(data["rideId"])

    return jsonify({"status": 200, "otp": otp})

PORT = os.getenv('OTP_SERVICE_PORT')

if __name__ == "__main__":
    port = PORT
    app.run(port=port, debug=True)






import time
import os
import redis
import google.oauth2.credentials
from google.oauth2 import service_account
import google.auth.transport.requests

SCOPES = ['https://www.googleapis.com/auth/firebase.messaging']
SERVICE_ACCOUNT_FILE = "jp-beckn-dev-4fbd238801a3.json"
credentials = service_account.Credentials.from_service_account_file(SERVICE_ACCOUNT_FILE, scopes=SCOPES)
request = google.auth.transport.requests.Request()

r_ba = redis.Redis(port=6379)
r_tr = redis.Redis(port=6380)


starttime=time.time()
while True:
    credentials.refresh(request)
    # os.environ["FCM_AUTH_TOKEN"] = credentials.token
    fcm_dict = {"fcm_auth_token": credentials.token}
    r_ba.mset(fcm_dict)
    r_tr.mset(fcm_dict)
    print("tick")
    time.sleep(600.0 - ((time.time() - starttime) % 600.0))

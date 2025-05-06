import requests
import json
import ast # To safely evaluate the string list

# --- Configuration ---
FCM_URL = 'https://fcm.googleapis.com/v1/projects/namma-yatri/messages:send'
# IMPORTANT: Replace with your actual bearer token before running
BEARER_TOKEN = 'ya29.c.c0ASRK0GaojVqaq_07TulCA7PhA9Hf74Af0lfJrRY85qgr-DmLlAIIYEL_OwPtw0NCQ9nNYKXNAeFxkkkT01s8RAuqqvKV8irObbtjS9-xXFTeIYGjG93eULjupYIBY46vUV18RNqJ8pgqTVe8doTf7I8QDBeWhaDJcP70Wn_ojTH-PMQrcXtJJr90Cuo7bbIzopvqwKe7a4uI7L_BUdmzucfiRB6QciFNguvUjEhkP3xIudPunnoUfToNxWVUa95dwGnvZK1DayLGvMyIFjL0abNj1XuzJpXW3Uh3SEsznWIQIMZZ9BkX45uEAe1Djc3XkpIveUpRlUPv-Om7JI12sARIe_mTDTqoOC1B9H0SLuoHin8LBDgn5E4E384PRt0240du14VyW3QkvwJ79kBymaIftng2v7I_wjrmbnF9-0jqy2ceUJ86I_FkmIUV4IeyVfrVnX_VclRoRn0cXFrJzvYverXwhj3kXj7bSu7snoWdXe-lq1Wo-aZjsnMi8jeaQedjQjSRruYn-wBgBOaF8Izj7UIlrbmY_dBVYhbbJ4_BVM002MuaXa6no9_hxwYq0996vy0JrIsodBYIqjk6sQFJS1sxgl0-q0rg1dJZx4wIkY5J-99olRRUXbIWXU7OiBaffm27y6VJxm7-hVczIxp_3rVtvpV5XZsWtOieyor3hq565uuWs1lJjUtaYIU1kuutmuc8ic2m1kuw-xIa9oSUMYXsnvu6UVpovvI4eimROx7ss7rrteB85U4_s-skxgVdgtozkg_z9-jFRBzcX1Ja11FvSZpn5X5Rofy1qRdi2Xv8QX4svyidRV4FM8v1drW3sZRk_e64crZfQIusY0vRdRUv7f7gRQU5zg5tmF0k5Sd62uR-0yZVZZugxsm6g_ko1865b16n7rr1MsShx-MReUJ7rf0Iddi1njxmUiWY9226hUsWO718bB5xsqXsXFy9J-8_MX2dJXc53nBkw5gxO6dZicM61npQ88IaZdv8cbk5sriiorl'
TARGET_TOKEN = "fKZ1ZPAyTFae0g3R0l8TzC:APA91bG7DMR9tOvDDDR38ap8x4elSwLbVc_XFCoEyfyIZJ0WGMpCYMFpXBoqgUUJZL0k1cgn5oijko9t0J2LMvrBMjMVAtDZlpLGlZyujoYqoSQ9r-4flW8"
KEYS_FILE_PATH = "Frontend/android-native/hyper-sdk/src/main/java/in/juspay/mobility/sdk/core/key.csv"
ENTITY_IDS = "f071d5a7-fe7f-4b98-b95f-933df86bc193" # From your example curl

# --- Read Keys ---
try:
    with open(KEYS_FILE_PATH, 'r') as f:
        keys_string = f.read()
        # Safely evaluate the string representation of the list
        keys = ast.literal_eval(keys_string)
        print(f"Successfully read {len(keys)} keys from {KEYS_FILE_PATH}")
except FileNotFoundError:
    print(f"Error: File not found at {KEYS_FILE_PATH}")
    exit(1)
except Exception as e:
    print(f"Error reading or parsing keys from {KEYS_FILE_PATH}: {e}")
    exit(1)

# --- Prepare Headers ---
headers = {
    'Content-Type': 'application/json',
    'Authorization': f'Bearer {BEARER_TOKEN}'
}

# --- Iterate and Send Requests ---
success_count = 0
error_count = 0

for key in keys:
    print(f"Processing key: {key}...")

    # Construct the specific notification_json string for this key
    notification_data = {
        "storage_key": key,
        "storage_value": "__failed"
    }
    notification_json_string = json.dumps(notification_data) # Convert the inner dict to a JSON string

    # Construct the main payload
    payload = {
        "message": {
            "token": TARGET_TOKEN,
            # Note: APNS part removed as it wasn't strictly needed for the key replacement
            # If you need it, copy it back from your original curl command
            "android": {
                "data": {
                    "notification_type": "UPDATE_STORAGE",
                    "show_notification": "false",
                    "entity_type": "Case",
                    "entity_ids": ENTITY_IDS,
                    "notification_json": notification_json_string # Use the generated JSON string here
                }
            }
        }
    }

    try:
        response = requests.post(FCM_URL, headers=headers, json=payload)
        response.raise_for_status() # Raise an exception for bad status codes (4xx or 5xx)
        print(f"  Success ({response.status_code}): {response.text[:100]}...") # Print first 100 chars of response
        success_count += 1
    except requests.exceptions.RequestException as e:
        print(f"  Error sending request for key '{key}': {e}")
        if e.response is not None:
            print(f"  Response Status: {e.response.status_code}")
            print(f"  Response Body: {e.response.text}")
        error_count += 1
    except Exception as e:
        print(f"  An unexpected error occurred for key '{key}': {e}")
        error_count += 1

print("\n--- Script Finished ---")
print(f"Total keys processed: {len(keys)}")
print(f"Successful requests: {success_count}")
print(f"Failed requests: {error_count}")
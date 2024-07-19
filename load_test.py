import threading
import requests
import datetime
import time

# Define the number of threads
NUM_THREADS = 50
NUM_REQUESTS = 10

def get_current_timestamp():
    return datetime.datetime.now().isoformat()

# Define the API endpoints and headers
driver_location_url = 'http://localhost:8081/ui/driver/location'
ride_search_url = 'http://localhost:8013/v2/rideSearch'

def get_ride_search_results_url(ride_search_id):
    return f"http://localhost:8013/v2/rideSearch/{ride_search_id}/results"

ride_search_result_headers = {
    'token': 'b7d'
}

driver_location_headers = {
    'Content-Type': 'application/json;charset=utf-8',
    'token': 'c3896059-37f8-4a51-a24e-71f34fd97204',
    'mId': 'favorit0-0000-0000-0000-00000favorit',
    'vt': 'AUTO_RICKSHAW',
    'dm': 'ONLINE'
}

ride_search_headers = {
    'Content-Type': 'application/json;charset=utf-8',
    'token': 'ec22d5d7-f1aa-4d8c-8105-211a62c821f4'
}

driver_location_data = [
    {
        "pt": {
            "lat": 12.95247991,
            "lon": 77.6050944
        },
        "ts": get_current_timestamp() + "+00:00",
    }
]

ride_search_data = {
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
                "lat": 12.952479909463571,
                "lon": 77.60509448873273
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
                "lat": 12.94840464059951,
                "lon": 77.58996589788522
            }
        }
    }
}

driver_location_latencies = []
ride_search_latencies = []

def call_driver_location():
    for _ in range(NUM_REQUESTS):
        start_time = time.time()
        response = requests.post(driver_location_url, headers=driver_location_headers, json=driver_location_data)
        end_time = time.time()
        latency = end_time - start_time
        driver_location_latencies.append(latency)
        print(f"Driver Location API response status: {response.status_code}, latency: {latency:.4f} seconds")

def call_ride_search():
    for _ in range(NUM_REQUESTS):
        start_time = time.time()
        response = requests.post(ride_search_url, headers=ride_search_headers, json=ride_search_data)
        end_time = time.time()
        latency = end_time - start_time
        ride_search_latencies.append(latency)
        print(f"Ride Search API response status: {response.status_code}, latency: {latency:.4f} seconds")

def call_ride_search_results(ride_search_id):
    response = requests.get(get_ride_search_results_url(ride_search_id), headers=ride_search_result_headers)
    print(f"Ride Search Results API response status: {response.status_code}")

def call_api():
    threads = []

    # Create threads for driver location API
    # for _ in range(NUM_THREADS):
    #     thread = threading.Thread(target=call_driver_location)
    #     threads.append(thread)
    #     thread.start()

    # Create threads for ride search API
    for _ in range(NUM_THREADS):
        thread = threading.Thread(target=call_ride_search)
        threads.append(thread)
        thread.start()

    # Wait for all threads to complete
    for thread in threads:
        thread.join()

    avg_driver_location_latency = sum(driver_location_latencies) / len(driver_location_latencies) if driver_location_latencies else 0
    avg_ride_search_latency = sum(ride_search_latencies) / len(ride_search_latencies) if ride_search_latencies else 0

    print(f"Average Driver Location API latency: {avg_driver_location_latency:.4f} seconds")
    print(f"Average Ride Search API latency: {avg_ride_search_latency:.4f} seconds")

    print("Load testing complete")

call_driver_location()    
call_api()      

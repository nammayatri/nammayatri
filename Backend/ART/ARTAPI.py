import time
import requests
import json
import re
import os
from datetime import datetime, timedelta


import subprocess

def flush_redis_clusters():
    clusters = [
        {"port": 6379, "name": "Main Cluster"},
        {"port": 30001, "name": "Cluster 1"},
        {"port": 30002, "name": "Cluster 2"},
        {"port": 30003, "name": "Cluster 3"}
    ]

    for cluster in clusters:
        print(f"[+] Flushed Redis {cluster['name']}")
        subprocess.run(["redis-cli", "-p", str(cluster['port']), "FLUSHALL"])


def clean_file(filename):
    dummy_line ='{"dataTimestamp":"2024-03-14T07:10:25.215311256Z","whereClauseText":""}'
    with open(filename, 'w') as file:
        file.write(dummy_line)


def getFilePath(file_name):
    current_file_path = os.path.abspath(__file__)
    current_directory = os.path.dirname(current_file_path)
    new_file_path = os.path.join(current_directory, file_name)
    return new_file_path


dataFilePath = getFilePath("data.log")
path = getFilePath("groupedRequestIds.log")
processed_file = getFilePath("processedData.log")

def calculate_new_time(time1, time2):
    time1_format = '%Y-%m-%dT%H:%M:%SZ'
    time2_format = '%Y-%m-%dT%H:%M:%SZ'
    time1 = time1.split('.')[0] + 'Z'
    time2 = time2.split('.')[0] + 'Z'
    time1 = datetime.strptime(time1, time1_format)
    time2 = datetime.strptime(time2, time2_format)

    # Find the difference between the times
    time_difference = max(time1, time2) - min(time1, time2)

    current_time = datetime.utcnow()
    new_time = current_time + time_difference
    return new_time.strftime('%Y-%m-%dT%H:%M:%SZ')

def calculate_new_time_without_z(time1, time2):
    time1_format = '%Y-%m-%dT%H:%M:%S'
    time2_format = '%Y-%m-%dT%H:%M:%S'
    time1 = time1.split('.')[0]
    time2 = time2.split('.')[0]
    time1 = datetime.strptime(time1, time1_format)
    time2 = datetime.strptime(time2, time2_format)

    # Find the difference between the times
    time_difference = max(time1, time2) - min(time1, time2)

    current_time = datetime.utcnow()
    new_time = current_time + time_difference
    return new_time.strftime('%Y-%m-%dT%H:%M:%S')


def replaceAllTimeInLine(line):
    if "queryData" in line:
        matches = re.findall(r'"timestamp":"([^"]+)"', line)
        if matches:
            timestamp = matches[0]
            pattern = r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d*Z'
            pattern_without_z = r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d*'
            # dataMatches = re.findall(pattern, line)
            # dataMatches_without_z = re.findall(pattern_without_z, line)
            # print("Data Matches:", dataMatches)
            new_line = re.sub(pattern, lambda x: calculate_new_time(x.group(), timestamp), line)
            new_line = re.sub(pattern_without_z, lambda x: calculate_new_time_without_z(x.group(), timestamp), new_line)
            new_line = re.sub(r'"timestamp":"([^"]+)"', f'"timestamp":"{timestamp}"', new_line)
            return new_line
        else:
            print("Timestamp not found in the line.")
            return line
    else:
        return line



def groupDataIntoFile(file_path,apikey):
    with open(file_path, 'r') as file:
        data = json.load(file)
        for key, value in data.items():
            if key == apikey:
                lines = ""
                for line in value:
                    lines += replaceAllTimeInLine(line)
                lines = lines.strip()

                with open(dataFilePath, 'w') as file:
                    file.write(lines)
                return
            else:
                continue





def callApiForART (path):
    with open(path, 'r') as file:
        api_data = json.load(file)
        for api_name, api_details in api_data.items():
            for line in api_details:
                if "rawPathInfo" in line:
                    # we will ask here to call the api or not or move to next
                    ask = input(f' Do you want to call the API: {api_name} (y/n) or Exit (e):')
                    if ask.lower() == "e":
                        return
                    elif ask.lower() == "n":
                        continue
                    groupDataIntoFile(path,api_name)
                    clean_file(processed_file)
                    api_details = json.loads(line)
                    api_details = api_details["request"]
                    requestMethod = eval(api_details["requestMethod"])
                    headers = dict(eval(api_details["requestHeaders"]))

                    if "host" in headers:
                        host = "http://"+ headers["host"]
                    else:
                        host = "http://" + headers["Host"]
                    urlPath = eval(api_details["rawPathInfo"])
                    handledUrl = host + urlPath + eval(api_details["rawQueryString"])
                    requestBody = eval(api_details["body"])
                    if len (requestBody) :
                        requestBody = json.loads(requestBody)

                    print(f"Calling API: {handledUrl}..............")
                    print("\n********** Request **********\n")
                    print(f'Request Method: {requestMethod}')
                    print(f'URL: {handledUrl}')
                    print(f'Headers: {headers}')
                    print(f'Body: {requestBody}')

                    try:
                        response = requests.request(
                            method=requestMethod,
                            url=handledUrl,
                            headers=headers,
                            json=requestBody
                        )
                        if response.status_code == 200:
                            print("\n\n********** Response **********\n")
                            print(f"Response: {response.json()}")
                            print("\nSleeping for 1 second............\n")
                            time.sleep(1)

                        else :
                            print("\n\n********** Response Error **********\n")
                            print(f"ErrorCode: {response.status_code}")
                            print(f"Error: {response.text}")
                            print("\nSleeping for 1 second............\n")
                            time.sleep(1)
                    except Exception as e:
                        print(f"Error: {e}")
                        print("\nSleeping for 1 seconds............\n")
                        time.sleep(1)
                else:
                    continue



callApiForART(path)

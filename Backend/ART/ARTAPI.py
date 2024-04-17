import time
import requests
import json
import re
import os
from datetime import datetime
import ARTDataFromKafka as art_kafka_data
import ARTDiffChecker as art_diff_checker


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
            pattern_till_seconds = r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}'
            pattern_without_z = r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d*'  # without Z
            new_line = re.sub(pattern, lambda x: calculate_new_time(x.group(), timestamp), line)
            new_line = re.sub(pattern_without_z, lambda x: calculate_new_time_without_z(x.group(), timestamp), new_line)
            new_line = re.sub(pattern_till_seconds, lambda x: calculate_new_time_without_z(x.group(), timestamp), new_line)
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
        time_to_sleep = 0
        api_data = json.load(file)
        for api_name, api_details in api_data.items():
            for line in api_details:
                if "rawPathInfo" in line and not "/beckn/" in line:
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
                    requestBody = eval(api_details["body"].replace("Just", "").replace("Nothing", ""))
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
                            print(f'\nSleeping for {time_to_sleep} seconds............\n')
                            time.sleep(time_to_sleep)

                        else :
                            print("\n\n********** Response Error **********\n")
                            print(f"ErrorCode: {response.status_code}")
                            print(f"Error: {response.text}")
                            print(f'\nSleeping for {time_to_sleep} seconds............\n')
                            time.sleep(time_to_sleep)
                    except Exception as e:
                        print(f"Error: {e}")
                        print("\nSleeping for 5 seconds............\n")
                        time.sleep(time_to_sleep)
                else:
                    continue

# --------------------------------- Paths ---------------------------------------#
input_file_path = getFilePath("custom.log")
input_path_mocked_data = getFilePath("artRunner.log")

# =====================================================================================================#'


# write data for art mocker
art_kafka_data.write_data_for_art_mocker(input_file_path)
print("Data written for ART Mocker")

print("Deleting kafka topic............\n")
art_kafka_data.delete_kafka_topic("ART-Logs")

print("Reading data and calling API for ART")
callApiForART(path)
print("API called for ART")

# write data for art runner mocker
# get mocked logs from kafka
print("Reading data from Kafka")
art_kafka_data.read_data_from_kafka("ART-Logs", input_path_mocked_data)
print("Data read from Kafka")

print("Writing data for ART Diff Checker")
art_kafka_data.write_data_for_art_diff_checker(input_path_mocked_data)
print("Data written for ART Diff Checker")


# lets run diff checker here
print("Running Diff Checker")
art_diff_checker.main()



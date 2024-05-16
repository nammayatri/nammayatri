import time
import requests
import json
import re
import os
from datetime import datetime
import ARTDataFromKafka as art_kafka_data
import ARTDiffChecker as art_diff_checker

class colors:
    RESET = '\033[0m'
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    PURPLE = '\033[95m'
    CYAN = '\033[96m'

def get_current_time():
    return datetime.now().strftime('%Y-%m-%dT%H:%M:%SZ')

def create_folder_with_file(filepath):
    directory = os.path.dirname(filepath)
    os.makedirs(directory, exist_ok=True)

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


def write_forked_data_to_file(data):
    forkedTagData = {}
    for line in data:
        if "forkedTag" in line:
            forkedTag = json.loads(line)["forkedTag"]
            if forkedTag not in forkedTagData:
                forkedTagData[forkedTag] = []
            forkedTagData[forkedTag].append(line)
    for key, value in forkedTagData.items():
        lines = ""
        for line in value:
            lines += replaceAllTimeInLine(line)
        lines = lines.strip()
        file_name = getFilePath("ArtForked/"+key+".log")
        create_folder_with_file(file_name)
        with open(file_name, 'w') as file:
            file.write(lines)
        print(f"Forked Data written to file: {key}.log\n")

def groupDataIntoFile(file_path,apikey):
    with open(file_path, 'r') as file:
        data = json.load(file)
        for key, value in data.items():
            if key == apikey:
                write_forked_data_to_file(value)
                lines = ""
                for line in value:
                    if "forkedTag" not in line:
                        lines += replaceAllTimeInLine(line)
                lines = lines.strip()

                with open(dataFilePath, 'w') as file:
                    file.write(lines)
                print(f"Data written to file: data.log\n")
                return
            else:
                continue


def callApiForART (path):
    with open(path, 'r') as file:
        time_to_sleep = 1
        api_data = json.load(file)
        for api_name, api_details in api_data.items():
            for line in api_details:
                if "rawPathInfo" in line and not "/beckn/" in line:
                    # we will ask here to call the api or not or move to next
                    # ask = input(f' Do you want to call the API: {api_name} (y/n) or Exit (e):')
                    # if ask.lower() == "e":
                    #     return
                    # elif ask.lower() == "n":
                    #     continue
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

                    print(f"{colors.YELLOW}Calling API: {api_name}{colors.RESET}")
                    print("\n********** Request **********\n")
                    print(f'Request Method: {requestMethod}')
                    print(f'URL: {handledUrl}')
                    print(f'Headers: {headers}')
                    print(f'Body: {json.dumps(requestBody)}')

                    try:
                        response = requests.request(
                            method=requestMethod,
                            url=handledUrl,
                            headers=headers,
                            json=requestBody
                        )
                        if response.status_code == 200:
                            print(f"\n\n{colors.GREEN}********** Success Response **********{colors.RESET}\n")
                            print(f"Response: {json.dumps(response.json(), indent=4)}")
                            print(f'\nSleeping for {time_to_sleep} seconds............\n')
                            time.sleep(time_to_sleep)

                        else :
                            print(f"\n\n{colors.RED}********** Response Error **********{colors.RESET}\n")
                            print(f"ErrorCode: {response.status_code}")
                            print(f"Error: {response.text}")
                            print(f'\nSleeping for {time_to_sleep} seconds............\n')
                            time.sleep(time_to_sleep)
                    except Exception as e:
                        print(f"{colors.RED}Error in calling API: {api_name} -> {e}{colors.RESET}")
                        print(f'\nSleeping for {time_to_sleep} seconds............\n')
                        time.sleep(time_to_sleep)
                else:
                    continue

# --------------------------------- Paths ---------------------------------------#
input_file_path = getFilePath("recordedData.log")
input_path_mocked_data = getFilePath("artRunner.log")

# =====================================================================================================#'


# write data for art mocker
print(f"{colors.CYAN}Writing data for ART Mocker =>{colors.RESET}")
art_kafka_data.write_data_for_art_mocker(input_file_path)
print(f"{colors.GREEN}Data written for ART Mocker{colors.RESET}\n")

print(f"{colors.CYAN}Deleting Kafka Topic: ART-Logs =>{colors.RESET}")
art_kafka_data.delete_kafka_topic("ART-Logs")
print(f"{colors.GREEN}Kafka Topic Deleted: ART-Logs{colors.RESET}\n")

print(f"{colors.CYAN}Calling API for ART =>{colors.RESET}")
callApiForART(path)
print(f"{colors.GREEN}API called for ART{colors.RESET}\n")

# write data for art runner mocker
# get mocked logs from kafka
print(f"{colors.CYAN}Reading data from Kafka => {colors.RESET}")
art_kafka_data.read_data_from_kafka("ART-Logs", input_path_mocked_data)
print(f"{colors.GREEN}Data read from Kafka{colors.RESET}\n")

print(f"{colors.CYAN}Writing data for ART Diff Checker => {colors.RESET}")
art_kafka_data.write_data_for_art_diff_checker(input_path_mocked_data)
print(f"{colors.GREEN}Data written for ART Diff Checker{colors.RESET}\n")


# lets run diff checker here
print(f"{colors.CYAN}Running ART Diff Checker => {colors.RESET}")
art_diff_checker.main()



import json
import os
from kafka import KafkaConsumer # type: ignore
import time
from kafka.admin import KafkaAdminClient, NewTopic # type: ignore

def getFilePath(file_name):
    current_file_path = os.path.abspath(__file__)
    current_directory = os.path.dirname(current_file_path)
    new_file_path = os.path.join(current_directory, file_name)
    return new_file_path

def write_null_requestIds_to_file(nullRequestIds, output_file_path):
    with open(output_file_path, 'w') as output_file:
        lines = ""
        for line in nullRequestIds:
            lines += line
        output_file.write(lines.strip())

def handleAPIdata(APIdata):
    handledData = []
    try :
        for line in APIdata:
            parsableLine = json.loads(line)
            if "queryData" in line and len (parsableLine["queryData"]["tableObject"]) == 0 and "find" in parsableLine["queryType"] :
                continue
            else :
                handledData.append(line)
        return handledData
    except Exception as e:
        return APIdata
        print(f"Error in parsing line in handleAPIdata -> {line}")
        print(f"Error -> {e}")



def processAPIdatainLogFile(input_file_path, output_file_path):
    print(f"Processing log file to get API data from -> {input_file_path}")
    APIdata = []
    counter = 0
    with open(input_file_path, 'r') as file:
        for line in file:
            if "requestMethod" in line :
                # json_string = eval(line)
                APIdata.append(line)

    with open(output_file_path, 'w') as output_file:
        length = len(APIdata)
        output_file.write("{")
        for line in APIdata:
            counter += 1
            api = "API" + str(counter)
            if counter == length:
                output_file.write('"'+api+'"' + ":" + str(line) + "\n")
            else:
                output_file.write('"'+api+'"' + ":" + str(line) + ",\n")

        output_file.write("}")
    print(f"Log file processed successfully")
    print(f"Total API calls in the log file -> {length}")
    print(f"API data is written to -> {output_file_path}\n\n")



def process_log_file(input_file_path):
    print(f"Processing log file to group by requestIds from -> {input_file_path}")
    requestIds = {}
    requestIdForAPI = {}
    groupedRequestIds = {}
    groupedRequestIdsForDiffChecker = {}
    nullRequestIds = []
    nullRequestIdsCheck = {}
    with open(input_file_path, 'r') as file:
        for line in file:
            if "requestId" in line :
                try:
                    parsableLine = json.loads(line)
                    requestId = parsableLine["requestId"]
                    if requestId == "" and not "forkedTag" in line and not "producerTimestampKey" in line: #check for null requestIds and skip forkedTag for all the app startup logs
                        nullReqIdLine = (line.split(',"timestamp"')[0]+"}")
                        if nullReqIdLine not in nullRequestIdsCheck:
                            nullRequestIdsCheck[nullReqIdLine] = True
                            nullRequestIds.append(line)
                        continue
                    if requestId not in requestIds:
                        requestIds[requestId] = []
                    # check if the line is not there corresponding to its requestId , get all the lines for that requestId not the list of line and timestamp
                    if line not in (line[0] for line in requestIds[requestId]):
                        requestIds[requestId].append([line, parsableLine['timestamp']])
                    if "requestMethod" in line :
                        apiPath = parsableLine["request"]["rawPathInfo"].replace('"', "")
                        if apiPath not in requestIdForAPI and "/beckn/" not in apiPath:
                            requestIdForAPI[apiPath] = [requestId,parsableLine['timestamp']]

                except Exception as e:
                    print(f"Error in parsing line -> {parsableLine}")
                    print(f"Error -> {e}")
                    continue
            else :
                print(f"Line does not contain requestId -> {line}")
        nullRequestIds = handleAPIdata(nullRequestIds)

        requestIdForAPI = {key: value[0] for key, value in sorted(requestIdForAPI.items(), key=lambda item: item[1][1])}
        for key, value in requestIdForAPI.items():
            requestData = requestIds[value]
            requestData = [sublist[0] for sublist in sorted(requestData, key=lambda x: x[1])] # sort by timestamp
            groupedRequestIdsForDiffChecker[key] = requestData # will be used to for diff checker
            groupedRequestIds[key]=(requestData+nullRequestIds)

        return (groupedRequestIds, nullRequestIds, groupedRequestIdsForDiffChecker)

def read_data_from_kafka(topic_name, output_file_path):
    read_time_frame = 30
    consumer = KafkaConsumer(
        topic_name,
        bootstrap_servers='localhost:29092',
        group_id='my_consumer_group',
        auto_offset_reset='earliest',
        enable_auto_commit=False,
        value_deserializer=lambda x: x.decode('utf-8')
    )
    print(f"Connecting to kafka topic -> {topic_name} to read data for {read_time_frame} seconds")
    with open(output_file_path, 'w') as output_file:
        start_time = time.time()
        is_frist_time = True
        for message in consumer:
            if is_frist_time:
                print(f"Reading data from kafka topic -> {topic_name} for {read_time_frame} seconds ... ")
                start_time = time.time()
                is_frist_time = False
            output_file.write(f'{message.value}\n')
            # Check if time limit has been reached
            if time.time() - start_time >= read_time_frame:
                break
    print(f"Data is read from kafka topic -> {topic_name}  ✓")
    print(f"Data is written to -> {output_file_path}  ✓\n\n")

def delete_kafka_topic(topic_name):
    try:
        admin_client = KafkaAdminClient(bootstrap_servers='localhost:29092')
        print(f"Deleting kafka topic -> {topic_name}")
        admin_client.delete_topics([topic_name])
        print(f"Kafka topic -> {topic_name} is deleted  ✓")
    except Exception as e:
        print(f"Error in deleting kafka topic -> {topic_name}")
        print(f"Error -> {e}")

def write_grouped_data_to_file(groupedRequestIds, nullRequestIds, output_file_path):
    write_null_requestIds_to_file(nullRequestIds, data_path) # write null requestIds to a file from where all the app startup logs can be fetched
    with open(output_file_path, 'w') as output_file:
        output_file.write(json.dumps(groupedRequestIds, indent=4,ensure_ascii=False))
    print(f"Total grouped requestIds in the log file -> {len(groupedRequestIds)}  ✓")
    print(f"Grouped data is written to -> {output_file_path}  ✓\n\n")


def write_diff_checker_data_to_file(groupedRequestIdsForDiffChecker, output_file_path):
    for _, value in groupedRequestIdsForDiffChecker.items():
        for line in value:
            if "system_configs" in line or "log_levels" in line:
                value.remove(line)
    with open(output_file_path, 'w') as output_file:
        output_file.write(json.dumps(groupedRequestIdsForDiffChecker, indent=4,ensure_ascii=False))
    print(f"Total requestIds in the log file -> {len(groupedRequestIdsForDiffChecker)}  ✓")
    print(f"Grouped data for diff checker is written to -> {output_file_path}  ✓\n\n")



# --------------------------------- Paths ---------------------------------------#
input_file_path = getFilePath("recordedData.log")
output_file_path_api = getFilePath("APIdata.json")
output_log_file_path_grouped = getFilePath("groupedRequestIds.log")
diff_checker_file_path_mocker = getFilePath("groupedRequestIdsForDiffChecker.log")
data_path = getFilePath("data.log")
input_path_mocked_data = getFilePath("artRunner.log")
diff_checker_file_path_mocked_data = getFilePath("APIdataArtRunner.log")

# =====================================================================================================#


#--------------------------------- data for art Mocker ------------------------------------------#

def write_data_for_art_mocker(path):
    groupedRequestIds, nullRequestIds, groupedRequestIdsForDiffChecker = process_log_file(path)
    write_grouped_data_to_file(groupedRequestIds, nullRequestIds, output_log_file_path_grouped)
    write_diff_checker_data_to_file(groupedRequestIdsForDiffChecker, diff_checker_file_path_mocker)
    print("** Done!! writing data for ART Mocker **\n")


# process data for ART mocker
# write_data_for_art_mocker(input_file_path)

#====================================================================================================#


#--------------------------------- data for art Diff Checker ---------------------------------#
def write_data_for_art_diff_checker(path):
    _ , _ , diff_checker_data = process_log_file(path)
    write_diff_checker_data_to_file (diff_checker_data, diff_checker_file_path_mocked_data)
    print("** Done!! writing data for ART Runner **\n")


# process data for ART mocked diff checker
# read_data_from_kafka("ART-Logs", input_path_mocked_data)
# write_data_for_art_diff_checker(input_path_mocked_data)

#====================================================================================================#






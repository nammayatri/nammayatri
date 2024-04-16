import json
import re
import os



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

def handleBecknCalls(requestDataBeckn):
    becknQueries = []
    for line in requestDataBeckn:
        if "requestMethod" not in line[0] and "forkedTag" not in line[0]:
            becknQueries.append(line)
    return becknQueries

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
    becknCallsQueries = []
    with open(input_file_path, 'r') as file:
        for line in file:
            if "requestId" in line :
                try:
                    parsableLine = json.loads(line)
                    requestId = parsableLine["requestId"]
                    if requestId == "" and not "forkedTag" in line: #check for null requestIds and skip forkedTag for all the app startup logs
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
        nullRequestIds = list(set(nullRequestIds))

        requestIdForAPI = {key: value[0] for key, value in sorted(requestIdForAPI.items(), key=lambda item: item[1][1])}
        for key, value in requestIdForAPI.items():
            # if "/beckn/" in key:
            #     requestDataBeckn = requestIds[value]
            #     requestDataBeckn = handleBecknCalls(requestDataBeckn)
            #     # requestDataBeckn = [sublist[0] for sublist in sorted(requestDataBeckn, key=lambda x: x[1])]
            #     becknCallsQueries+=(requestDataBeckn)

            # else:
            requestData = requestIds[value]
            requestData = [sublist[0] for sublist in sorted(requestData, key=lambda x: x[1])] # sort by timestamp
            requestData = handleAPIdata(requestData)
            groupedRequestIdsForDiffChecker[key] = requestData # will be used to for diff checker
            groupedRequestIds[key]=(requestData+nullRequestIds)
        #append beckn calls to each key in groupedRequestIds
        # lets make the beckn calls unique by first element  and sort them by timestamp
        # becknCallsQueries = [sublist[0] for sublist in sorted(becknCallsQueries, key=lambda x: x[1])]
        # becknCallsQueries = list(set(becknCallsQueries))
        # groupedRequestIds = {key: value+becknCallsQueries for key, value in groupedRequestIds.items()}

        return (groupedRequestIds, nullRequestIds, groupedRequestIdsForDiffChecker)




def write_grouped_data_to_file(groupedRequestIds, nullRequestIds, output_file_path):
    write_null_requestIds_to_file(nullRequestIds, data_path) # write null requestIds to a file from where all the app startup logs can be fetched
    with open(output_file_path, 'w') as output_file:
        output_file.write(json.dumps(groupedRequestIds, indent=4,ensure_ascii=False))
    print(f"Total grouped requestIds in the log file -> {len(groupedRequestIds)}  ✓")
    print(f"Grouped data is written to -> {output_file_path}  ✓\n\n")


def write_diff_checker_data_to_file(groupedRequestIdsForDiffChecker, output_file_path):
    with open(output_file_path, 'w') as output_file:
        output_file.write(json.dumps(groupedRequestIdsForDiffChecker, indent=4,ensure_ascii=False))
    print(f"Total requestIds in the log file -> {len(groupedRequestIdsForDiffChecker)}  ✓")
    print(f"Grouped data for diff checker is written to -> {output_file_path}  ✓\n\n")




input_file_path = getFilePath("custom.log")
output_file_path_api = getFilePath("APIdata.json")
output_log_file_path_grouped = getFilePath("groupedRequestIds.log")
diff_checker_file_path_mocker = getFilePath("groupedRequestIdsForDiffChecker.log")
data_path = getFilePath("data.log")
input_path_mocked_data = getFilePath("artRunner.log")
diff_checker_file_path_mocked_data = getFilePath("APIdataArtRunner.log")



#--------------------------------- data for art Mocker ---------------------------------#

def write_data_for_art_mocker(path):
    groupedRequestIds, nullRequestIds, groupedRequestIdsForDiffChecker = process_log_file(path)
    write_grouped_data_to_file(groupedRequestIds, nullRequestIds, output_log_file_path_grouped)
    write_diff_checker_data_to_file(groupedRequestIdsForDiffChecker, diff_checker_file_path_mocker)
    print("** Done!! writing data for ART Mocker **\n")


# process data for ART mocker
write_data_for_art_mocker(input_file_path)



#--------------------------------- data for art Diff Checker ---------------------------------#
def write_data_for_art_diff_checker(path):
    _ , _ , diff_checker_data = process_log_file(path)
    write_diff_checker_data_to_file (diff_checker_data, diff_checker_file_path_mocked_data)
    print("** Done!! writing data for ART Runner **\n")


# process data for ART mocked diff checker
write_data_for_art_diff_checker(input_path_mocked_data)







